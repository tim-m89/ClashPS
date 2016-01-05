#!/bin/zsh
pulp build
psc-bundle output/**/*.js -m TimPS > tim.js
echo "module.exports = PS[\"TimPS\"].playerTim;" >> tim.js
