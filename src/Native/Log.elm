port module Native.Log exposing (..)

import Json.Encode as E


port line : E.Value -> Cmd msg
