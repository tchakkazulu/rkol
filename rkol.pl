use strict;
use warnings;

use JSON;
use IO::Socket::INET;
use HTTP::Response;
use URI::Escape;
use DateTime::Format::Strptime;
use DateTime::Format::Duration;

my %commands = (
  when => \&rkol_when,
  dj => \&rkol_dj,
  next => \&rkol_next,
  after => \&rkol_after,
);

my $durFormat = DateTime::Format::Duration->new(pattern => '%e days, %H hours, and %M minutes', normalise => 1);
my $outZone = 'America/New_York';
# 2012-02-10 20:00:00
my $inFormat = new DateTime::Format::Strptime(pattern => '%F %T', time_zone => 'UTC');
# Jezebelle is scheduled in 0 days, 2 hours, and 22 minutes. Which is on Sun Dec 18 03:00 EST.
my $outFormat = new DateTime::Format::Strptime(pattern => '%a %b %d %H:%M %Z');

Xchat::register('Radio-KoL Schedule Script', '0.3', 'Perl version of the schedulebot');
Xchat::hook_print('Channel Message', \&rkol_schedule);
keys %commands;
while (my ($command, $func) = each %commands) {
	Xchat::hook_command($command, clientwrap($func));
}

my $test = 0;

sub output {
	if ($test) {
		Xchat::print(shift);
	} else {
		Xchat::command("say " . shift);
	}
}

#   rkol_schedule(@xchatargs)
#
# Hook to Xchat's "Channel Message" event. Arguments are passed as described by
# http://xchat.org/docs/xchat2-perl.html#xchat_hook_print
sub rkol_schedule {
    return Xchat::EAT_NONE unless(defined Xchat::get_info("network"));
    return Xchat::EAT_NONE unless(defined Xchat::get_info("channel"));
	return Xchat::EAT_NONE unless(lc Xchat::get_info("network") eq 'synirc');
	return Xchat::EAT_NONE unless(grep(lc Xchat::get_info("channel"), '#tchakkablaat', '#radio-kol'));

	my ($call, $args) = split ' ', $_[0][1], 2;

	keys %commands;
	while (my ($command, $func) = each %commands) {
		&$func($args) if (lc $call eq lc "!$command");
	}
	
	return Xchat::EAT_NONE;
}

#   rkol_when(@args)
#
# Look for the DJ specified in @args
sub rkol_when {
	my $dj = shift;

	my $rkol_when_raw = sub {
		my $data = shift;
		my $slot = $data->{slots}[0];
		my $dur = $durFormat->format_duration($inFormat->parse_datetime($slot->{startDate}) - DateTime->now(time_zone => 'UTC'));
		output($slot->{djName} . " is scheduled between " . reformat($slot->{startDate}) . " and " . reformat($slot->{endDate}) . ", " . $dur);
	};

	rkol_request({c => "djfind", name => $dj},$rkol_when_raw);
}

#   rkol_dj(@args)
#
# Get the current DJ
sub rkol_dj {
	my $rkol_dj_raw = sub {
		my $data = shift;
		my $slot = $data->{slots}[0];
		output("The current DJ is " . $slot->{djName} . ", who is on until " . reformat($slot->{endDate}) . ".");
	};
	
	rkol_request({c => "djnow"},$rkol_dj_raw);
}

#   rkol_next(@args)
#
# Get the next DJ
sub rkol_next {
	my $rkol_next_raw = sub {
		my $data = shift;
		my $slot = $data->{slots}[0];
		output("The next DJ is " . $slot->{djName} . ", and they're on from " . reformat($slot->{startDate}) . " until " . reformat($slot->{endDate}) . ".");
	};

	rkol_request({c => "djnext"},$rkol_next_raw);
}

#   rkol_after(@args)
#
# Look for the DJ scheduled after the DJ specified in @args
sub rkol_after {
	my $dj = shift;

	my $rkol_after_raw = sub {
		my $data = shift;
		my $slot = $data->{slots}[0];
		output($slot->{djName} . " is scheduled between " . reformat($slot->{startDate}) . " and " . reformat($slot->{endDate}) . ".");
	};
	
	rkol_request({c => "djafter", name => $dj},$rkol_after_raw);
}

sub slotData {
  my $slot = shift;
  my $start = $inFormat->parse_datetime($slot->{startDate});
  my $end = $inFormat->parse_datetime($slot->{endDate});
  my $data = {dj => $slot->{djName}
             ,start => $start
			 ,end => $end
			 ,duration => $end - $start
			 ,untilStart => $start - DateTime::now(time_zone => 'UTC')
			 ,untilEnd => $end - DateTime::now(time_zone => 'UTC')
			 ,confirm => $slot->{status}
			 };
  return $data;
}

#   rkol_request($args, $callback)
#
# Perform a GET request to the RKoL schedule API as described by $args,
# Parse response body as JSON, checks for a status of "OK", and give it to the callback.
#
#   example: rkol_request({c => "djafter", name => "DjMcDjpants"}, \&callback})
#
# API documentation: http://ccgi.kitsbury.force9.co.uk/kol/index.php?whsmodule=content/user&type=page&id=Schedule+API
# Beware: the resulting record is {info : {...}, slots : [...]}, not {info : {... , slots : []}}
sub rkol_request {
    my ($args, $callback) = @_;
	my $document = "/kol/raw.php?whsmodule=query&tz=utc" . mkArgs($args);
	
	my $othercall = sub {
		my $data = HTTP::Response->parse(shift)->content;
		$data = decode_json((split('\n', $data))[1]);
		
		if ($data->{info}{result} eq "OK") {
			&$callback($data);
		} else {
			output("Something unexpected happened:");
			output($data->{info}{result} . ": " . $data->{info}{error}{message});
		}

	};

	http_request("ccgi.kitsbury.force9.co.uk",$document,$othercall);
}

#   mkArgs($args)
#
# Makeshift query-string creation from a hash-reference. Ugly.
sub mkArgs {
	my $args = shift;
	my $parms = "";
	while (my ($key, $val) = each %$args) {
		$parms .= "&" . $key . "=" . uri_escape($val);
	}
	return $parms;
}

#   http_request($host, $document, $callback)
#
# Performs a GET request to $host/$document, then gives the response to
# $callback.
#
#   example: http_request("www.example.com","index.php?x=5",\&callback);
#
# This so that Xchat doesn't block too long while waiting for content.
sub http_request {
	my ($host, $document, $callback) = @_;

	my $remote = IO::Socket::INET->new(Proto => "tcp",
					PeerAddr => $host,
					PeerPort => "http(80)",
					Type => SOCK_STREAM,
					Timeout => 10
					);

	if ($remote)
	{
		my $context = Xchat::get_context();
		Xchat::hook_fd($remote, sock_handler($callback,$context));
		$remote->autoflush(1);

		print $remote "GET $document HTTP/1.1\n";
		print $remote "HOST: $host \n";
		print $remote "Connection: close\n\n";
	}
	else 
	{
		output "There was a problem and it wasn't possible to establish a connection to the server";
	}
}

#   sock_handler($callback,$context)
#
# Returns a function to be used with Xchat::hook_fd to read the contents of a socket,
# then feeds the entire string to the callback function.
# Automatically unhooks the socket.
#
#   example: Xchat::hook_fd($fd, sock_handler(\&callback));
#
# This so that Xchat doesn't block too long while waiting for content.
sub sock_handler {
	my ($callback, $context) = @_;
	
	my $proc = sub{
		Xchat::set_context($context);
		my $remote = $_[0];
		my $data = "";
	
		while (<$remote>) {
			$data .= $_;	
		}
		close $remote;
	
		&$callback($data);
	
		return Xchat::REMOVE;
	};

	return $proc;
}

#   clientwrap($callback)
#
# Wraps rkol_* function such that it can be used as a hook for Xchat::hook_command
#
#   example: Xchat::hook_command("after",clientwrap(\&rkol_after));
sub clientwrap {
	my $command = shift;

	my $proc = sub {
		&$command($_[1][1]);
		return Xchat::EAT_NONE;
	};
	
	return $proc;
}

sub reformat {
  my $timestr = shift;
  my $dt = $inFormat->parse_datetime($timestr);
  $dt->set_time_zone($outZone);
  return $outFormat->format_datetime($dt);
}

