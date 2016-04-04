var webpack = require("webpack");

module.exports = {
    entry: "./entry.js",
    output: {
        path: __dirname + "/js",
        filename: "bundle.js"
    },

    module: {
        loaders: [
        { test: /\.css$/, loader: "style!css" },
        { test: /jquery\.js$/, loader: 'expose?$' },
        { test: /jquery\.js$/, loader: 'expose?jQuery' }
        ]
    },

};
