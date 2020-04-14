module Graph exposing (Graph, empty, includes, insert, toString)

import Dict exposing (Dict)
import DotLang as DL


type alias Graph =
    Dict String (List String)


insert : String -> List String -> Graph -> Graph
insert =
    Dict.insert


includes : String -> Graph -> Bool
includes =
    Dict.member


empty : Graph
empty =
    Dict.empty


toString : Graph -> String
toString =
    toDot >> DL.toString


toDot : Graph -> DL.Dot
toDot graph =
    let
        edgeStmts =
            Dict.toList graph
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
    in
    DL.Dot DL.Digraph
        Nothing
        (DL.LooseAttr (DL.Attr (DL.ID "rankdir") (DL.ID "LR"))
            :: edgeStmts
        )


toNodeId : String -> DL.NodeId
toNodeId id =
    DL.NodeId (DL.ID id) Nothing
