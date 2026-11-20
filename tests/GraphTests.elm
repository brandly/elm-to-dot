module GraphTests exposing (suite)

import Expect
import Graph
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Graph"
        [ describe "toString"
            [ test "an empty graph is still a digraph" <|
                \_ ->
                    Graph.empty
                        |> Graph.toString
                        |> Expect.equal (digraph [])
            , test "draws an edge from a module to each of its imports" <|
                \_ ->
                    Graph.empty
                        |> Graph.insert "Main" [ "Graph", "Utils" ]
                        |> Graph.insert "Graph" []
                        |> Graph.insert "Utils" []
                        |> Graph.toString
                        |> Expect.equal
                            (digraph
                                [ "Main -> \"Graph\""
                                , "Main -> Utils"
                                ]
                            )
            , test "ignores imports that aren't in the graph" <|
                \_ ->
                    Graph.empty
                        |> Graph.insert "Main" [ "Utils", "Json.Decode" ]
                        |> Graph.insert "Utils" []
                        |> Graph.toString
                        |> Expect.equal (digraph [ "Main -> Utils" ])
            , test "renders a module with no surviving imports as a bare node" <|
                \_ ->
                    Graph.empty
                        |> Graph.insert "Main" [ "Json.Decode" ]
                        |> Graph.toString
                        |> Expect.equal (digraph [ "Main" ])
            , test "renders an isolated module alongside connected ones" <|
                \_ ->
                    Graph.empty
                        |> Graph.insert "Main" [ "Utils" ]
                        |> Graph.insert "Utils" []
                        |> Graph.insert "Orphan" []
                        |> Graph.toString
                        |> Expect.equal
                            (digraph
                                [ "Main -> Utils"
                                , "Orphan"
                                ]
                            )
            , test "quotes module names that aren't bare identifiers" <|
                \_ ->
                    Graph.empty
                        |> Graph.insert "Main" [ "Native.File" ]
                        |> Graph.insert "Native.File" []
                        |> Graph.toString
                        |> Expect.equal (digraph [ "Main -> \"Native.File\"" ])
            , test "sorts modules and keeps import order within a module" <|
                \_ ->
                    Graph.empty
                        |> Graph.insert "Utils" [ "Parser" ]
                        |> Graph.insert "Main" [ "Utils", "Parser" ]
                        |> Graph.insert "Parser" []
                        |> Graph.toString
                        |> Expect.equal
                            (digraph
                                [ "Main -> Utils"
                                , "Main -> Parser"
                                , "Utils -> Parser"
                                ]
                            )
            ]
        , describe "includes"
            [ test "is True for a module that was inserted" <|
                \_ ->
                    Graph.empty
                        |> Graph.insert "Main" []
                        |> Graph.includes "Main"
                        |> Expect.equal True
            , test "is False for a module that wasn't" <|
                \_ ->
                    Graph.empty
                        |> Graph.insert "Main" [ "Utils" ]
                        |> Graph.includes "Utils"
                        |> Expect.equal False
            ]
        ]


{-| The DOT output that `elm-to-dot` prints, with the boilerplate filled in:

    digraph {
        rankdir=LR
        <statements>
    }

-}
digraph : List String -> String
digraph statements =
    String.join "\n" <|
        List.concat
            [ [ "digraph {" ]
            , List.map (\line -> "    " ++ line) ("rankdir=LR" :: statements)
            , [ "}" ]
            ]
