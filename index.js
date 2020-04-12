const subscribeToNode = require('./elm-node')
const { Elm } = require('./dist')

const app = Elm.Main.init({})
subscribeToNode(app)
