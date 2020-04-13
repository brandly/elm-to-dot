port module Native.File exposing (..)

import Json.Decode as D
import Json.Encode as E


port readFile : E.Value -> Cmd msg


port readFileSuccess : (E.Value -> msg) -> Sub msg


port readFileError : (E.Value -> msg) -> Sub msg


type alias File =
    { name : String, contents : String }


decodeReadFileSuccess : D.Value -> Result D.Error File
decodeReadFileSuccess =
    D.decodeValue <|
        D.map2 File
            (D.field "name" D.string)
            (D.field "contents" D.string)


type alias NativeError =
    { code : String
    , syscall : String
    , path : String
    , message : String
    }


decodeNativeError : D.Value -> Result D.Error NativeError
decodeNativeError =
    D.decodeValue <|
        D.map4 NativeError
            (D.field "code" D.string)
            (D.field "syscall" D.string)
            (D.field "path" D.string)
            (D.field "message" D.string)
