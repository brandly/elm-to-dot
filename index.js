const subscribeToNode = require('./elm-node')
const { Elm } = require('./dist')

const app = Elm.Main.init({
  flags: {
    pwd: process.cwd(),
    argv: process.argv,
    versionMessage: require('./package.json').version,
    colorMode: !!(process.stdout.isTTY && !process.env.NO_COLOR)
  }
})

subscribeToNode(app)
