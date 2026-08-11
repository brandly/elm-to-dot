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
        survivingDeps : String -> List String
        survivingDeps node =
            Dict.get node graph
                |> Maybe.withDefault []
                |> List.filter (\dep -> Dict.member dep graph)

        isEdgeTarget : String -> Bool
        isEdgeTarget node =
            Dict.keys graph
                |> List.any (\other -> List.member node (survivingDeps other))

        edgeStmts =
            Dict.toList graph
                |> List.map
                    (\( node, _ ) ->
                        case List.map (toNodeId >> DL.EdgeNode) (survivingDeps node) of
                            [] ->
                                if isEdgeTarget node then
                                    []

                                else
                                    [ DL.NodeStmt (toNodeId node) [] ]

                            edges ->
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
