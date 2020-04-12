module Main exposing (..)

import Elm.Parser
import Elm.RawFile as RawFile
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node
import Json.Decode as D
import Json.Encode as E
import Native.File
import Parser
import Platform


type Msg
    = ReadFileSuccess (Result D.Error File)
    | FileError (Result D.Error NativeError)


type Model
    = Base String


main : Program String Model Msg
main =
    Platform.worker
        { init =
            \entryFile ->
                let
                    splits =
                        String.split "/" entryFile

                    base : String
                    base =
                        List.take (List.length splits - 1) splits
                            |> String.join "/"
                in
                ( Base base, Native.File.readFile (E.string entryFile) )
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ((Base base) as model) =
    case msg of
        ReadFileSuccess (Ok { name, contents }) ->
            case parseImports contents of
                Ok rawImports ->
                    let
                        imports =
                            rawImports
                                |> List.map
                                    (\path ->
                                        (base :: path)
                                            |> String.join "/"
                                            |> (\p -> p ++ ".elm")
                                    )
                    in
                    Debug.log ("imports: " ++ Debug.toString imports)
                        ( model
                        , Cmd.batch
                            (List.map (\imp -> Native.File.readFile (E.string imp)) imports)
                        )

                Err e ->
                    Debug.log ("failed to parse: " ++ Debug.toString e) ( model, Cmd.none )

        FileError (Ok { code, message }) ->
            if code == "ENOENT" then
                ( model, Cmd.none )

            else
                Debug.log ("Error: " ++ message) ( model, Cmd.none )

        _ ->
            Debug.log "other msg" ( model, Cmd.none )


parseImports : String -> Result (List Parser.DeadEnd) (List (List String))
parseImports elm =
    Elm.Parser.parse elm
        |> Result.map
            (\v ->
                RawFile.imports v
                    |> List.map (.moduleName >> Node.value)
            )


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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Native.File.readFileSuccess
            (decodeReadFileSuccess >> ReadFileSuccess)
        , Native.File.readFileError
            (decodeNativeError >> FileError)
        ]
