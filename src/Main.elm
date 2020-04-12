module Main exposing (..)

import Elm.Parser
import Elm.RawFile as RawFile
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node
import Json.Decode as D
import Json.Encode as E
import Native.File
import Platform


type Msg
    = Msg
    | FileContents (Result D.Error String)
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
        FileContents (Ok str) ->
            let
                imports =
                    parseImports str
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

        FileError (Ok str) ->
            Debug.log ("Error: " ++ str) ( model, Cmd.none )

        _ ->
            Debug.log "other msg" ( model, Cmd.none )


parseImports : String -> List (List String)
parseImports str =
    Elm.Parser.parse str
        |> Result.map
            (\v ->
                RawFile.imports v
                    |> List.map (.moduleName >> Node.value)
            )
        -- otherwise, we could handle (List DeadEnd) and exit with a message?
        |> Result.withDefault []


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
