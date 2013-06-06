module KoLChat where

data KoLMsg = ChanMessage Channel From String
            | PrivMessage From String
            | LogOut From
            | LogIn From
            | Other String
  deriving Show

toStr :: KoLMsg -> String
toStr (ChanMessage c _ str) = "/" ++ c ++ " " ++ str
toStr (PrivMessage to str) = "/msg " ++ withUnderscores to ++ " " ++ str
  where withUnderscores = map toUnderscore
        toUnderscore ' ' = '_'
        toUnderscore x   = x

type From = String
type Channel = String

