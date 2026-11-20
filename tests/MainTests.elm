module MainTests exposing (suite)

import Dict
import Expect
import Graph
import Json.Decode as Decode
import Main
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Main"
        [ describe "makeAbsolute"
            [ test "resolves a relative path against the working directory" <|
                \_ ->
                    Main.makeAbsolute "/home/me/proj" "src/Main.elm"
                        |> Expect.equal "/home/me/proj/src/Main.elm"
            , test "resolves a path starting with ./" <|
                \_ ->
                    Main.makeAbsolute "/home/me/proj" "./src/Main.elm"
                        |> Expect.equal "/home/me/proj/src/Main.elm"
            , test "walks up for each ../" <|
                \_ ->
                    Main.makeAbsolute "/home/me/proj/src" "../../other/Main.elm"
                        |> Expect.equal "/home/me/other/Main.elm"
            , test "leaves an absolute path alone" <|
                \_ ->
                    Main.makeAbsolute "/home/me/proj" "/elsewhere/Main.elm"
                        |> Expect.equal "/elsewhere/Main.elm"
            , test "treats . as the working directory" <|
                \_ ->
                    Main.makeAbsolute "/home/me/proj" "."
                        |> Expect.equal "/home/me/proj"
            , test "treats an empty path as the working directory" <|
                \_ ->
                    Main.makeAbsolute "/home/me/proj" ""
                        |> Expect.equal "/home/me/proj"
            ]
        , describe "getParent"
            [ test "drops the last segment" <|
                \_ ->
                    Main.getParent "/home/me/proj"
                        |> Expect.equal "/home/me"
            , test "bottoms out at the root" <|
                \_ ->
                    Main.getParent "/home"
                        |> Expect.equal ""
            ]
        , describe "moduleToFile"
            [ test "turns a module name into a path under a source directory" <|
                \_ ->
                    Main.moduleToFile "/proj/src" "Native.File"
                        |> Expect.equal "/proj/src/Native/File.elm"
            , test "handles a top level module" <|
                \_ ->
                    Main.moduleToFile "/proj/src" "Main"
                        |> Expect.equal "/proj/src/Main.elm"
            ]
        , describe "elmJsonDecoder"
            [ test "reads source-directories" <|
                \_ ->
                    """
                    { "type": "application"
                    , "source-directories": [ "src", "vendor" ]
                    , "elm-version": "0.19.1"
                    }
                    """
                        |> Decode.decodeString Main.elmJsonDecoder
                        |> Expect.equal
                            (Ok
                                { type_ = "application"
                                , sourceDirs = [ "src", "vendor" ]
                                , elmVersion = "0.19.1"
                                }
                            )
            , test "defaults to src when source-directories is missing" <|
                \_ ->
                    """
                    { "type": "package"
                    , "elm-version": "0.19.0 <= v < 0.20.0"
                    }
                    """
                        |> Decode.decodeString Main.elmJsonDecoder
                        |> Result.map .sourceDirs
                        |> Expect.equal (Ok [ "src" ])
            , test "fails when source-directories isn't a list of strings" <|
                \_ ->
                    """
                    { "type": "application"
                    , "source-directories": "src"
                    , "elm-version": "0.19.1"
                    }
                    """
                        |> Decode.decodeString Main.elmJsonDecoder
                        |> Expect.err
            ]
        , describe "parseModules"
            [ test "reads the module name and its imports" <|
                \_ ->
                    [ "module Page.Home exposing (view)"
                    , ""
                    , "import Html"
                    , "import Json.Decode as Decode"
                    , ""
                    , "view ="
                    , "    Html.text \"hello\""
                    , ""
                    ]
                        |> String.join "\n"
                        |> Main.parseModules
                        |> Expect.equal
                            (Ok
                                { name = "Page.Home"
                                , dependencies = [ "Html", "Json.Decode" ]
                                }
                            )
            , test "lists a module imported twice once" <|
                \_ ->
                    [ "module Page.Home exposing (view)"
                    , ""
                    , "import Html"
                    , "import Html exposing (text)"
                    , ""
                    , "view ="
                    , "    text \"hello\""
                    , ""
                    ]
                        |> String.join "\n"
                        |> Main.parseModules
                        |> Result.map .dependencies
                        |> Expect.equal (Ok [ "Html" ])
            , test "fails on something that isn't Elm" <|
                \_ ->
                    Main.parseModules "this is not an elm module"
                        |> Expect.err
            ]
        , describe "update"
            [ test "doesn't queue a file that is already pending" <|
                \_ ->
                    let
                        -- Main imports A and B, and both import Shared.
                        -- A has been read, so Shared is already pending.
                        crawling =
                            Main.Crawling
                                { sourceDirs = [ "/proj/src" ]
                                , graph =
                                    Graph.empty
                                        |> Graph.insert "Main" [ "A", "B" ]
                                        |> Graph.insert "A" [ "Shared" ]
                                , pending = [ "/proj/src/B.elm", "/proj/src/Shared.elm" ]
                                }

                        fileB =
                            { name = "/proj/src/B.elm"
                            , contents = "module B exposing (..)\n\nimport Shared\n\nb =\n    1\n"
                            }
                    in
                    case Main.update { includeExternal = False, entryFile = "/proj/src/Main.elm" } (Main.ReadFileSuccess fileB) crawling of
                        ( Main.Crawling state, _ ) ->
                            Expect.equal [ "/proj/src/Shared.elm" ] state.pending

                        _ ->
                            Expect.fail "expected to still be crawling"
            , test "with --include-external, keeps the edges of a module that's already been read" <|
                \_ ->
                    let
                        -- Shared has been read and imports Html. B imports
                        -- Shared and is read afterwards, while C is still pending.
                        crawling =
                            Main.Crawling
                                { sourceDirs = [ "/proj/src" ]
                                , graph =
                                    Graph.empty
                                        |> Graph.insert "Main" [ "Shared", "B", "C" ]
                                        |> Graph.insert "Shared" [ "Html" ]
                                        |> Graph.insert "Html" []
                                        |> Graph.insert "B" []
                                        |> Graph.insert "C" []
                                , pending = [ "/proj/src/B.elm", "/proj/src/C.elm" ]
                                }

                        fileB =
                            { name = "/proj/src/B.elm"
                            , contents = "module B exposing (..)\n\nimport Shared\n\nb =\n    1\n"
                            }
                    in
                    case Main.update { includeExternal = True, entryFile = "/proj/src/Main.elm" } (Main.ReadFileSuccess fileB) crawling of
                        ( Main.Crawling state, _ ) ->
                            Dict.get "Shared" state.graph
                                |> Expect.equal (Just [ "Html" ])

                        _ ->
                            Expect.fail "expected to still be crawling"
            ]
        ]
