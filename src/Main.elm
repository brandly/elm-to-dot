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
    = FileContents (Result D.Error String)
    | FileError (Result D.Error String)


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
        FileContents (Ok contents) ->
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

        FileError (Ok str) ->
            Debug.log ("Error: " ++ str) ( model, Cmd.none )

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


decodeFileContents : D.Value -> Result D.Error String
decodeFileContents =
    D.decodeValue D.string


decodeFileError : D.Value -> Result D.Error String
decodeFileError =
    D.decodeValue (D.field "message" D.string)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Native.File.readFileSuccess
            (decodeFileContents >> FileContents)
        , Native.File.readFileError
            (decodeFileError >> FileError)
        ]
