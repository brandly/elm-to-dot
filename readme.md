# elm-to-dot

Crawl an Elm project and produce a dependency graph in [DOT](<https://en.wikipedia.org/wiki/DOT_(graph_description_language)>).

```shell
$ npm install --global elm-to-dot
$ elm-to-dot --help
elm-to-dot [--include-external] <entry file>
```

## usage

```shell
$ elm-to-dot src/Main.elm
digraph {
    rankdir=LR
    Main -> "Graph"
    Main -> "Native.File"
    Main -> "Native.Log"
}
$ elm-to-dot --include-external src/Main.elm
digraph {
    rankdir=LR
    "Graph" -> Dict
    "Graph" -> DotLang
    Main -> "Cli.Option"
    Main -> "Cli.OptionsParser"
    Main -> "Cli.Program"
    Main -> "Elm.Parser"
    Main -> "Elm.RawFile"
    Main -> "Elm.Syntax.Node"
    Main -> "Graph"
    Main -> "Json.Decode"
    Main -> "Json.Encode"
    Main -> "Native.File"
    Main -> "Native.Log"
    Main -> Parser
    "Native.File" -> "Json.Decode"
    "Native.File" -> "Json.Encode"
    "Native.Log" -> "Json.Encode"
}
```

DOT can be fed into [Graphviz](https://www.graphviz.org/) to generate an SVG.

![dependencies](https://github.com/brandly/elm-to-dot/blob/master/dependency-graph.svg)

I like [this browser-based version](http://viz-js.com/) of Graphviz, but I generated the dependency graph above like so.

```shell
$ elm-to-dot --include-external src/Main.elm \
    | dot -Tsvg \
    | svgo --input - \
    > dependency-graph.svg
```

- `dot` is [Graphviz](https://graphviz.gitlab.io/download/). With the `-Tsvg` flag, it outputs an SVG.
- [`svgo`](https://github.com/svg/svgo) optimizes SVG files.

## why?

I looked at `elm-analyse` and was surprised to find this CLI is largely written in Elm. However portions of the logic, like the [`file-gatherer`](https://github.com/stil4m/elm-analyse/blob/master/ts/util/file-gatherer.ts), are written in TypeScript.

I wondered what it would be like to thinly wrap Node.js APIs in `ports`, allowing full filesystem access in Elm.

## dev

```shell
$ npm install
$ ./bin.js src/Main.elm
digraph {
    rankdir=LR
    Main -> "Graph"
    Main -> "Native.File"
    Main -> "Native.Log"
}
```
