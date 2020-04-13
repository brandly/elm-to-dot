# elm-to-dot

Crawl Elm dependencies and produce a dependency graph in [DOT](<https://en.wikipedia.org/wiki/DOT_(graph_description_language)>).

```shell
$ npm install --global elm-to-dot
$ elm-to-dot src/Main.elm
```

## why?

I looked at `elm-analyse` and was surprised to find this CLI is largely written in Elm. However portions of the logic, like the [`file-gatherer`](https://github.com/stil4m/elm-analyse/blob/master/ts/util/file-gatherer.ts), are written in TypeScript.

I wondered what it would be like to thinly wrap Node.js APIs in `ports`, allowing full filesystem access in Elm.

## dev

```
$ npm install
$ ./bin.js src/Main.elm
digraph {
    rankdir=LR
    Main -> Dict
    Main -> DotLang
    Main -> "Elm.Parser"
    Main -> "Elm.RawFile"
    Main -> "Elm.Syntax.Import"
    Main -> "Elm.Syntax.Node"
    Main -> "Json.Decode"
    Main -> "Json.Encode"
    Main -> "Native.File"
    Main -> "Native.Log"
    Main -> Parser
    Main -> Platform
    "Native.File" -> "Json.Decode"
    "Native.File" -> "Json.Encode"
    "Native.Log" -> "Json.Encode"
}
```

Generate an SVG with [Graphviz](https://www.graphviz.org/). I used [this browser-based version](http://viz-js.com/).

![dependencies](https://github.com/brandly/elm-to-dot/blob/master/dependency-graph.svg)
