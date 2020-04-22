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


type Msg
    = ReadFileSuccess (Result D.Error File)
    | FileError (Result D.Error NativeError)


type alias CrawlingState =
    { sourceDirs : List String
    , graph : Graph
    , pending : List String
    }


type Model
    = FindingPackage { entryFile : String, dir : String }
    | Crawling CrawlingState
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
            \_ { entryFile } ->
                let
                    splits =
                        String.split "/" entryFile

                    dir : String
                    dir =
                        List.take (List.length splits - 1) splits
                            |> String.join "/"
                in
                ( FindingPackage { entryFile = entryFile, dir = dir }
                , Native.File.readFile (E.string (dir ++ "/elm.json"))
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
        ( FindingPackage { entryFile, dir }, ReadFileSuccess (Ok jsonFile) ) ->
            case D.decodeString elmJsonDecoder jsonFile.contents of
                Ok { sourceDirs } ->
                    ( Crawling
                        { -- TODO: more sophisticated join, handle relative paths
                          sourceDirs = List.map (\src -> dir ++ "/" ++ src) sourceDirs
                        , graph = Graph.empty
                        , pending = [ entryFile ]
                        }
                    , Native.File.readFile (E.string entryFile)
                    )

                Err _ ->
                    ( model, Native.Log.line <| E.string "Failed to decode JSON" )

        ( FindingPackage { entryFile, dir }, FileError (Ok { code }) ) ->
            if code == "ENOENT" then
                let
                    parent =
                        getParent dir
                in
                ( FindingPackage { entryFile = entryFile, dir = parent }
                , Native.File.readFile (E.string (parent ++ "/elm.json"))
                )

            else
                ( model, Native.Log.line <| E.string (code ++ " " ++ dir) )

        ( Crawling ({ sourceDirs, graph } as state), ReadFileSuccess (Ok file) ) ->
            case parseModules file.contents of
                Ok { name, dependencies } ->
                    let
                        filesToFetch =
                            dependencies
                                |> List.filter (\dep -> not <| Graph.includes dep graph)
                                |> List.map
                                    (\mod ->
                                        sourceDirs |> List.map (\dir -> moduleToFile dir mod)
                                    )
                                |> List.concat

                        withExternal =
                            if options.includeExternal && (not <| Graph.includes name graph) then
                                mapGraph (\graph_ -> List.foldl (\dep g -> Graph.insert dep [] g) graph_ dependencies)

                            else
                                identity

                        state_ =
                            state
                                |> withExternal
                                |> mapGraph (Graph.insert name dependencies)
                                |> mapPending (\p -> List.filter ((/=) file.name) p ++ filesToFetch)
                    in
                    finishCrawling
                        ( Crawling state_
                        , Cmd.batch
                            (List.map (E.string >> Native.File.readFile) filesToFetch)
                        )

                Err e ->
                    ( model, Native.Log.line <| E.string ("failed to parse: " ++ deadEndsToString e) )

        ( Crawling state, FileError (Ok { code, message, path }) ) ->
            if code == "ENOENT" then
                let
                    state_ =
                        mapPending (List.filter ((/=) path))
                            state
                in
                finishCrawling
                    ( Crawling state_, Cmd.none )

            else
                ( model, Native.Log.line <| E.string ("Error: " ++ message) )

        _ ->
            ( model, Native.Log.line <| E.string "unexpected msg" )


type alias ElmJson =
    { type_ : String
    , sourceDirs : List String
    , elmVersion : String
    }


elmJsonDecoder : D.Decoder ElmJson
elmJsonDecoder =
    D.map3 ElmJson
        (D.at [ "type" ] D.string)
        (D.at [ "source-directories" ] (D.list D.string))
        (D.at [ "elm-version" ] D.string)


type alias ElmModule =
    { name : String
    , dependencies : List String
    }


parseModules : String -> Result (List Parser.DeadEnd) ElmModule
parseModules elm =
    Elm.Parser.parse elm
        |> Result.map
            (\v ->
                { name = (RawFile.moduleName >> String.join ".") v
                , dependencies =
                    RawFile.imports v
                        |> List.map (.moduleName >> Node.value >> importToModule)
                }
            )


importToModule : List String -> String
importToModule =
    String.join "."


moduleToFile : String -> String -> String
moduleToFile base mod =
    ((base :: String.split "." mod) |> String.join "/") ++ ".elm"


getParent : String -> String
getParent dir =
    let
        splits =
            String.split "/" dir
    in
    splits |> List.take (List.length splits - 1) |> String.join "/"


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Ready _ ->
            Sub.none

        _ ->
            Sub.batch
                [ Native.File.readFileSuccess
                    (Native.File.decodeReadFileSuccess >> ReadFileSuccess)
                , Native.File.readFileError
                    (Native.File.decodeNativeError >> FileError)
                ]


deadEndsToString : List Parser.DeadEnd -> String
deadEndsToString deadEnds =
    -- Parser.deadEndsToString is broken/neglected. Here's a patch from this PR:
    -- https://github.com/elm/parser/pull/38
    let
        deadEndToString : Parser.DeadEnd -> String
        deadEndToString deadEnd =
            let
                position : String
                position =
                    "row:" ++ String.fromInt deadEnd.row ++ " col:" ++ String.fromInt deadEnd.col ++ "\n"
            in
            case deadEnd.problem of
                Parser.Expecting str ->
                    "Expecting " ++ str ++ "at " ++ position

                Parser.ExpectingInt ->
                    "ExpectingInt at " ++ position

                Parser.ExpectingHex ->
                    "ExpectingHex at " ++ position

                Parser.ExpectingOctal ->
                    "ExpectingOctal at " ++ position

                Parser.ExpectingBinary ->
                    "ExpectingBinary at " ++ position

                Parser.ExpectingFloat ->
                    "ExpectingFloat at " ++ position

                Parser.ExpectingNumber ->
                    "ExpectingNumber at " ++ position

                Parser.ExpectingVariable ->
                    "ExpectingVariable at " ++ position

                Parser.ExpectingSymbol str ->
                    "ExpectingSymbol " ++ str ++ " at " ++ position

                Parser.ExpectingKeyword str ->
                    "ExpectingKeyword " ++ str ++ "at " ++ position

                Parser.ExpectingEnd ->
                    "ExpectingEnd at " ++ position

                Parser.UnexpectedChar ->
                    "UnexpectedChar at " ++ position

                Parser.Problem str ->
                    "ProblemString " ++ str ++ " at " ++ position

                Parser.BadRepeat ->
                    "BadRepeat at " ++ position
    in
    List.foldl (++) "" (List.map deadEndToString deadEnds)
