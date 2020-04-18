module Main exposing (..)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Elm.Parser
import Elm.RawFile as RawFile
import Elm.Syntax.Node as Node
import Graph exposing (Graph)
import Json.Decode as D
import Json.Encode as E
import Native.File exposing (File, NativeError)
import Native.Log
import Parser
import Platform


type Msg
    = ReadFileSuccess (Result D.Error File)
    | FileError (Result D.Error NativeError)


type alias CrawlingState =
    { base : String
    , graph : Graph
    , pending : List String
    }


type Model
    = Crawling CrawlingState
    | Ready Graph


mapGraph : (Graph -> Graph) -> CrawlingState -> CrawlingState
mapGraph map m =
    { m | graph = map m.graph }


mapPending : (List String -> List String) -> CrawlingState -> CrawlingState
mapPending map m =
    { m | pending = map m.pending }


type alias CliOptions =
    { includeExternal : Bool
    , entryFile : String
    }


main : Program.StatefulProgram Model Msg CliOptions {}
main =
    Program.stateful
        { printAndExitFailure = E.string >> Native.Log.line
        , printAndExitSuccess = E.string >> Native.Log.line
        , init =
            \flags { entryFile } ->
                let
                    splits =
                        String.split "/" entryFile

                    base : String
                    base =
                        List.take (List.length splits - 1) splits
                            |> String.join "/"
                in
                ( Crawling
                    { base = base
                    , graph = Graph.empty
                    , pending = [ entryFile ]
                    }
                , Native.File.readFile (E.string entryFile)
                )
        , config = programConfig
        , subscriptions = subscriptions
        , update = update
        }


programConfig : Program.Config CliOptions
programConfig =
    Program.config
        |> Program.add
            (OptionsParser.build CliOptions
                |> OptionsParser.with
                    (Option.flag "include-external")
                |> OptionsParser.with
                    (Option.requiredPositionalArg "entry file")
            )


update : CliOptions -> Msg -> Model -> ( Model, Cmd Msg )
update options msg model =
    let
        finishCrawling ( m, cmd ) =
            case m of
                Crawling s ->
                    if List.length s.pending > 0 then
                        ( m, cmd )

                    else
                        ( Ready s.graph
                        , Native.Log.line (E.string (Graph.toString s.graph))
                        )

                _ ->
                    ( m, cmd )
    in
    case ( model, msg ) of
        ( Crawling ({ base, graph } as state), ReadFileSuccess (Ok { name, contents }) ) ->
            case parseModules contents of
                Ok modules ->
                    let
                        files =
                            modules |> List.map (moduleToFile base)

                        filesToFetch =
                            List.filter (\file -> not <| Graph.includes (fileToModule base file) graph) files

                        state_ =
                            state
                                |> mapGraph (Graph.insert (fileToModule base name) modules)
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
            if code == "ENOENT" then
                let
                    updateGraph =
                        -- dead end because we naively look for local file paths, even for installed modules
                        -- treat it like a dead end but still insert it into the graph
                        if options.includeExternal then
                            mapGraph (Graph.insert (fileToModule state.base path) [])

                        else
                            identity

                    state_ =
                        state
                            |> updateGraph
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


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Crawling _ ->
            Sub.batch
                [ Native.File.readFileSuccess
                    (Native.File.decodeReadFileSuccess >> ReadFileSuccess)
                , Native.File.readFileError
                    (Native.File.decodeNativeError >> FileError)
                ]

        _ ->
            Sub.none
