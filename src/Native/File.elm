port module Native.File exposing (..)


port readFile : String -> Cmd msg


port readFileSuccess : (File -> msg) -> Sub msg


port readFileError : (NativeError -> msg) -> Sub msg


type alias File =
    { name : String
    , contents : String
    }


type alias NativeError =
    { code : String
    , syscall : String
    , path : String
    , message : String
    }
