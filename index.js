const subscribeToNode = require('./elm-node')
const { Elm } = require('./dist')

const app = Elm.Main.init({
  flags: {
    pwd: process.env.PWD,
    argv: process.argv,
    versionMessage: require('./package.json').version
  }
})

subscribeToNode(app)
