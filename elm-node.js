const fs = require('fs')

module.exports = app => {
  app.ports.readFile.subscribe(fileName => {
    fs.readFile(fileName, (e, file) => {
      if (e) {
        app.ports.readFileError.send({
          code: e.code,
          syscall: e.syscall,
          path: e.path,
          message: e.message
        })
      } else {
        app.ports.readFileSuccess.send({
          name: fileName,
          contents: file.toString()
        })
      }
    })
  })

  app.ports.line.subscribe(line => {
    console.log(line)
  })

  app.ports.exitWithError.subscribe(error => {
    console.error(error)
    process.exit(1)
  })
}
