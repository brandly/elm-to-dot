const fs = require('fs')

module.exports = app => {
  app.ports.readFile.subscribe((fileName) => {
    fs.readFile(fileName, (e, file) => {
      if (e) {
        app.ports.readFileError.send(e)
      } else {
        app.ports.readFileSuccess.send(file.toString())
      }
    })
  })
}
