module Main exposing (..)

import Json.Decode as D
import Json.Encode as E
import Native.File
import Platform


type Msg
    = Msg
    | FileContents (Result D.Error String)


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
            Debug.log str ( model, Cmd.none )

        FileContents (Err e) ->
            Debug.log "fail" ( model, Cmd.none )

        _ ->
            Debug.log "other msg" ( model, Cmd.none )


decodeFileContents : D.Value -> Result D.Error String
decodeFileContents =
    D.decodeValue D.string


subscriptions : Model -> Sub Msg
subscriptions _ =
    Native.File.readFileSuccess
        (decodeFileContents >> FileContents)
