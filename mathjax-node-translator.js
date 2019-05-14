#!/usr/bin/env node
const argv = require('yargs').argv;
const path = require('path');
const mjAPI = require("mathjax-node");
const crypto = require('crypto');
const fs = require('fs');
const md5sum = crypto.createHash('md5');

const events = require('events');
const eventEmitter = new events.EventEmitter();


const stdin = process.stdin,
      stdout = process.stdout,
      stderr = process.stderr,
      inputChunks = [];

// stdin.resume();
stdin.setEncoding('utf8');
stdin.on('data', function (chunk) {
    inputChunks.push(chunk);
});


stdin.on('end', function () {
    mjAPI.config({
        extensions: '[commands]/commands.js',
        paths: {
            'commands': path.join(__dirname,'assets/'),
        }, 
        MathJax: {
            TeX: { 
                extensions: ["AMSmath.js", "AMSsymbols.js", "mhchem.js"], 
                Macros: {}, 
            }, 
        } 
    });
    mjAPI.start();
    const inputJSON = JSON.parse(inputChunks.join(""));
    const promises = []
    for (var md5 in inputJSON) {
        const array = inputJSON[md5];
        const value_maybe_with_delim = array["value"];
        if (! "value" in array ||! "type" in array)
            throw "Wrong JSON formatting (missing value or type key)!";
        reg_array = [
            [/^\\\((?<latexcontent>.*)\\\)$/us, "inline-TeX"],
            [/^\\\[(?<latexcontent>.*)\\\]$/us, "TeX"],
            [/^(?<latexcontent>\\begin\{(?<envname>[^}]*)\}.*\\end\{\k<envname>\})$/us,
             "TeX"]
        ];
        matched = false
        format = "inline-TeX"
        for (i in reg_array) {
            [reg, tmpformat] = reg_array[i]
            match = value_maybe_with_delim.match(reg);
            if (match) {
                format = tmpformat
                matched = true
                jax_value = match.groups.latexcontent;
            }
        }
        if (! matched)
            throw `Wrong latex formatting (${array["value"]}, md5=${md5}, beginchar=${array["begin"]})`;
        promise = mjAPI.typeset({
            math: jax_value,
            format: format,
            html: true,
            md5: md5
        }).then(val => array["html"] = val.html)
            .catch((error) => stderr.write(`!! ${jax_value}: ${error} (${array["begin"]})\n`));
        promises.push(promise);
    }
    Promise.all(promises).then(function(values) {
        stdout.write(JSON.stringify(inputJSON));
    });
});

