"use strict";
exports.__esModule = true;
var mustSing = {
    marking: {
        "walk": { byDefault: true }
    },
    andOrTree: {
        nodetype: "all",
        pre: "all of",
        children: [
            { leaf: "walk" },
            { leaf: "run" },
            { nodetype: "any",
                pre: "either",
                children: [{ leaf: "eat" }, { leaf: "drink" }]
            }
        ]
    }
};
console.log(JSON.stringify(mustSing));
