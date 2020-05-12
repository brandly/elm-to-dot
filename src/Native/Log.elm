port module Native.Log exposing (..)


port line : String -> Cmd msg


port exitWithError : String -> Cmd msg
