My ClashJS ship written in Purescript

To build & use:

Get and change into the source directory:

  git clone https://github.com/tim-m89/ClashPS
  cd ClashPS

Setup the tooling:

  npm install purescript bower pulp

Setup path:

  export "PATH=./node_modules/.bin/:$PATH"

Install dependencies:

  pulp dep install

Build the Source

  pulp build

Bundle the output into a single javscript file

  psc-bundle output/**/*.js -m TimPS > tim.js

And finally to cope with a module export limitation:

  echo "module.exports = PS[\"TimPS\"].playerTim;" >> tim.js

Then copy tim.js into the clashjs src/players directory and modify src/Players.js so that tim.js is included

Run from the clashjs root directory:

  npm run dev

