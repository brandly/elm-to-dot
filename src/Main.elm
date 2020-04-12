module Main exposing (..)

import Json.Decode as D
import Json.Encode as E
import Native.File
import Platform


type Msg
    = Msg
    | FileContents (Result D.Error String)
    | FileError (Result D.Error String)


type Model
    = Model


main : Program () Model Msg
main =
    Platform.worker
        { init = \_ -> ( Model, Native.File.readFile (E.string "src/Main.elm") )
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FileContents (Ok str) ->
            Debug.log ("contents: " ++ str) ( model, Cmd.none )

        FileError (Ok str) ->
            Debug.log ("Error: " ++ str) ( model, Cmd.none )

        _ ->
            Debug.log "other msg" ( model, Cmd.none )


decodeFileContents : D.Value -> Result D.Error String
decodeFileContents =
    D.decodeValue D.string


decodeFileError : D.Value -> Result D.Error String
decodeFileError =
    D.decodeValue (D.field "code" D.string)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Native.File.readFileSuccess
            (decodeFileContents >> FileContents)
        , Native.File.readFileError
            (decodeFileError >> FileError)
        ]
