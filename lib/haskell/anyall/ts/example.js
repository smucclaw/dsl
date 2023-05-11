"use strict";
exports.__esModule = true;
var mustsing_1 = require("./mustsing");
var mustSing = {
    marking: {
        "walk": { "Right": true },
        "run": { "Right": true },
        "sink": { "Right": false },
        "swim": { "Right": false },
        "eat": { "Left": false },
        "drink": { "Right": false }
    },
    andOrTree: mustsing_1.mustSingTree
};
console.log(JSON.stringify(mustSing));
