"use strict";
exports.__esModule = true;
var mustsing_1 = require("./mustsing");
var mustSing = {
    andOrTree: mustsing_1.mustSingTree,
    marking: {
        "eat": { "Left": false },
        "drink": { "Left": true }
    }
};
console.log(JSON.stringify(mustSing));
