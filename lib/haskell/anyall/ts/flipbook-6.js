"use strict";
exports.__esModule = true;
var mustsing_1 = require("./mustsing");
var mustSing = {
    andOrTree: mustsing_1.mustSingTree,
    marking: {
        "eat": { "Left": false },
        "drink": { "Left": true },
        "walk": { "Left": true },
        "swim": { "Right": true },
        "sink": { "Right": false }
    }
};
console.log(JSON.stringify(mustSing));
