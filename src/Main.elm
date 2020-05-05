module Main exposing (..)

import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Elm.Parser
import Elm.RawFile as RawFile
import Elm.Syntax.Node as Node
import Graph exposing (Graph)
import Json.Decode as Decode
import Native.File exposing (File, NativeError)
import Native.Log
import Parser


type Msg
    = ReadFileSuccess File
    | FileError NativeError


type Model
    = FindingPackage { entryFile : String, dir : String }
    | Crawling CrawlingState
    | Ready Graph


type alias CrawlingState =
    { sourceDirs : List String
    , graph : Graph
    , pending : List String
    }


mapGraph : (Graph -> Graph) -> CrawlingState -> CrawlingState
mapGraph map state =
    { state | graph = map state.graph }


mapPending : (List String -> List String) -> CrawlingState -> CrawlingState
mapPending map state =
    { state | pending = map state.pending }


type alias CliOptions =
    { includeExternal : Bool
    , entryFile : String
    }


main : Program.StatefulProgram Model Msg CliOptions { pwd : String }
main =
    Program.stateful
        { printAndExitFailure = Native.Log.line
        , printAndExitSuccess = Native.Log.line
        , init =
            \flags { entryFile } ->
                let
                    absoluteEntry =
                        makeAbsolute flags.pwd entryFile

                    splits =
                        String.split "/" absoluteEntry

                    dir : String
                    dir =
                        List.take (List.length splits - 1) splits
                            |> String.join "/"
                in
                ( FindingPackage { entryFile = absoluteEntry, dir = dir }
                , Native.File.readFile (dir ++ "/elm.json")
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
                    (Option.requiredPositionalArg "entry file"
                        |> Option.validateMap
                            (\file ->
                                if String.endsWith ".elm" file then
                                    Ok file

                                else
                                    Err <| "Expected an Elm file but received \"" ++ file ++ "\""
                            )
                    )
            )


update : CliOptions -> Msg -> Model -> ( Model, Cmd Msg )
update options msg model =
    let
        finishCrawling ( model_, cmd ) =
            case model_ of
                Crawling s ->
                    if List.length s.pending > 0 then
                        ( model_, cmd )

                    else
                        ( Ready s.graph
                        , Native.Log.line (Graph.toString s.graph)
                        )

                _ ->
                    ( model_, cmd )
    in
    case ( model, msg ) of
        ( FindingPackage { entryFile, dir }, ReadFileSuccess jsonFile ) ->
            case Decode.decodeString elmJsonDecoder jsonFile.contents of
                Ok { sourceDirs } ->
                    ( Crawling
                        { sourceDirs =
                            List.map (makeAbsolute dir)
                                sourceDirs
                        , graph = Graph.empty
                        , pending = [ entryFile ]
                        }
                    , Native.File.readFile entryFile
                    )

                Err error ->
                    ( model
                    , Native.Log.line
                        (String.join "\n"
                            [ "Failed to decode JSON."
                            , jsonFile.name
                            , Decode.errorToString error
                            ]
                        )
                    )

        ( FindingPackage { entryFile, dir }, FileError { code } ) ->
            if code == "ENOENT" then
                let
                    parent =
                        getParent dir
                in
                if parent == "" then
                    ( model, Native.Log.line "No elm.json file found." )

                else
                    ( FindingPackage { entryFile = entryFile, dir = parent }
                    , Native.File.readFile (parent ++ "/elm.json")
                    )

            else
                ( model, Native.Log.line (code ++ " " ++ dir) )

        ( Crawling ({ sourceDirs, graph } as state), ReadFileSuccess file ) ->
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
                            if options.includeExternal then
                                mapGraph
                                    (\graph_ ->
                                        List.foldl (\dependency g -> Graph.insert dependency [] g)
                                            graph_
                                            dependencies
                                    )

                            else
                                identity

                        state_ =
                            state
                                |> withExternal
                                |> mapGraph (Graph.insert name dependencies)
                                |> mapPending
                                    (\pending ->
                                        List.filter ((/=) file.name) pending ++ filesToFetch
                                    )
                    in
                    finishCrawling
                        ( Crawling state_
                        , Cmd.batch <|
                            List.map Native.File.readFile filesToFetch
                        )

                Err e ->
                    ( model
                    , Native.Log.line ("failed to parse: " ++ deadEndsToString e)
                    )

        ( Crawling state, FileError { code, message, path } ) ->
            if code == "ENOENT" && state.graph /= Graph.empty then
                let
                    state_ =
                        mapPending (List.filter ((/=) path))
                            state
                in
                finishCrawling
                    ( Crawling state_, Cmd.none )

            else
                ( model, Native.Log.line message )

        _ ->
            ( model, Native.Log.line "unexpected msg" )


type alias ElmJson =
    { type_ : String
    , sourceDirs : List String
    , elmVersion : String
    }


elmJsonDecoder : Decode.Decoder ElmJson
elmJsonDecoder =
    Decode.map3 ElmJson
        (Decode.at [ "type" ] Decode.string)
        (Decode.at [ "source-directories" ] (Decode.list Decode.string))
        (Decode.at [ "elm-version" ] Decode.string)


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
                        |> List.map (.moduleName >> Node.value >> String.join ".")
                }
            )


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


makeAbsolute : String -> String -> String
makeAbsolute pwd file =
    case String.split "/" file of
        ".." :: tail ->
            makeAbsolute (getParent pwd) (String.join "/" tail)

        "." :: tail ->
            pwd ++ String.join "/" tail

        "" :: _ ->
            file

        _ :: _ ->
            pwd ++ "/" ++ file

        [] ->
            file


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Ready _ ->
            Sub.none

        _ ->
            Sub.batch
                [ Native.File.readFileSuccess ReadFileSuccess
                , Native.File.readFileError FileError
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
