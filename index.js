const subscribeToNode = require('./elm-node')
const { Elm } = require('./dist')

const app = Elm.Main.init({
  flags: process.argv[2]
})
subscribeToNode(app)
