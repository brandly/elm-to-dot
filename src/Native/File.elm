port module Native.File exposing (..)

import Json.Encode as E


port readFile : E.Value -> Cmd msg


port readFileSuccess : (E.Value -> msg) -> Sub msg


port readFileError : (E.Value -> msg) -> Sub msg
