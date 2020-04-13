module Main exposing (..)

import Dict exposing (Dict)
import DotLang as DL
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
                , Native.Log.line (E.string (viewGraph graph))
                )
    in
    case ( model, msg ) of
        ( Crawling ({ base, graph } as state), ReadFileSuccess (Ok { name, contents }) ) ->
            case parseModules contents of
                Ok modules ->
                    let
                        files =
                            modules |> List.map (moduleToFile base)

                        filesToFetch =
                            List.filter (\file -> Dict.member (fileToModule base file) graph |> not) files

                        state_ =
                            state
                                |> mapGraph (Dict.insert (fileToModule base name) modules)
                                |> mapPending (\p -> List.filter ((/=) name) p ++ filesToFetch)
                    in
                    finishCrawling
                        ( Crawling state_
                        , Cmd.batch
                            (filesToFetch
                                |> List.map (E.string >> Native.File.readFile)
                            )
                        )

                Err e ->
                    ( model, Native.Log.line <| E.string ("failed to parse: " ++ Parser.deadEndsToString e) )

        ( Crawling state, FileError (Ok { code, message, path }) ) ->
            -- dead end because we naively look for local file paths, even for installed modules
            -- treat it like a dead end but still insert it into the graph
            if code == "ENOENT" then
                let
                    state_ =
                        state
                            |> mapGraph (Dict.insert (fileToModule state.base path) [])
                            |> mapPending (List.filter ((/=) path))
                in
                finishCrawling
                    ( Crawling state_, Cmd.none )

            else
                ( model, Native.Log.line <| E.string ("Error: " ++ message) )

        _ ->
            ( model, Native.Log.line <| E.string "unexpected msg" )


parseModules : String -> Result (List Parser.DeadEnd) (List String)
parseModules elm =
    Elm.Parser.parse elm
        |> Result.map
            (\v ->
                RawFile.imports v
                    |> List.map (.moduleName >> Node.value >> importToModule)
            )


importToModule : List String -> String
importToModule =
    String.join "."


moduleToFile : String -> String -> String
moduleToFile base mod =
    ((base :: String.split "." mod) |> String.join "/") ++ ".elm"


fileToModule : String -> String -> String
fileToModule base file =
    file
        -- +1 for the /
        |> String.dropLeft (String.length base + 1)
        |> String.dropRight (String.length ".elm")
        |> String.split "/"
        |> String.join "."


viewGraph : Dict String (List String) -> String
viewGraph =
    toDot >> DL.toString


toDot : Dict String (List String) -> DL.Dot
toDot graph =
    DL.Dot DL.Digraph
        Nothing
        (Dict.toList graph
            |> List.map
                (\( node, deps ) ->
                    let
                        edges =
                            deps
                                |> List.filter (\dep -> Dict.member dep graph)
                                |> List.map (toNodeId >> DL.EdgeNode)
                    in
                    List.map
                        (\edge -> DL.EdgeStmtNode (toNodeId node) edge [] [])
                        edges
                )
            |> List.concat
        )


toNodeId : String -> DL.NodeId
toNodeId id =
    DL.NodeId (DL.ID id) Nothing


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
