module Main exposing (..)

import Dict exposing (Dict)
import Elm.Parser
import Elm.RawFile as RawFile
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node
import Json.Decode as D
import Json.Encode as E
import Native.File
import Native.Log
import Parser
import Platform


type Msg
    = ReadFileSuccess (Result D.Error File)
    | FileError (Result D.Error NativeError)


type alias CrawlingState =
    { base : String
    , graph : Dict String (List String)
    , pending : List String
    }


type Model
    = Crawling CrawlingState
    | Ready { graph : Dict String (List String) }


mapGraph : (Dict String (List String) -> Dict String (List String)) -> CrawlingState -> CrawlingState
mapGraph map m =
    { m | graph = map m.graph }


mapPending : (List String -> List String) -> CrawlingState -> CrawlingState
mapPending map m =
    { m | pending = map m.pending }


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
                ( Crawling { base = base, graph = Dict.empty, pending = [ entryFile ] }
                , Native.File.readFile (E.string entryFile)
                )
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        finishCrawling ( m, cmd ) =
            let
                pending =
                    case m of
                        Crawling s ->
                            s.pending

                        _ ->
                            []

                graph =
                    case m of
                        Crawling s ->
                            s.graph

                        Ready s ->
                            s.graph
            in
            if List.length pending > 0 then
                ( m, cmd )

            else
                ( Ready { graph = graph }
                , Native.Log.line (E.string "done!!!")
                )
    in
    case ( model, msg ) of
        ( Crawling ({ base, graph } as state), ReadFileSuccess (Ok { name, contents }) ) ->
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

                        importsToFetch =
                            List.filter (\file -> Dict.member file graph |> not) imports

                        state_ =
                            state
                                |> mapGraph (Dict.insert name imports)
                                |> mapPending (\p -> List.filter ((/=) name) p ++ importsToFetch)
                    in
                    finishCrawling
                        ( Crawling state_
                        , Cmd.batch
                            (importsToFetch
                                |> List.map (E.string >> Native.File.readFile)
                            )
                        )

                Err e ->
                    Debug.log ("failed to parse: " ++ Debug.toString e) ( model, Cmd.none )

        ( Crawling state, FileError (Ok { code, message, path }) ) ->
            if code == "ENOENT" then
                finishCrawling
                    ( Crawling <| mapPending (List.filter ((/=) path)) state, Cmd.none )

            else
                Debug.log ("Error: " ++ message) ( model, Cmd.none )

        _ ->
            Debug.log ("other msg: " ++ Debug.toString msg) ( model, Cmd.none )


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
subscriptions model =
    case model of
        Crawling _ ->
            Sub.batch
                [ Native.File.readFileSuccess
                    (decodeReadFileSuccess >> ReadFileSuccess)
                , Native.File.readFileError
                    (decodeNativeError >> FileError)
                ]

        _ ->
            Sub.none
