module Handler.Models where

import Import

data Task = Task{
  tId :: Integer,
  text :: String,
  done :: Bool
}


