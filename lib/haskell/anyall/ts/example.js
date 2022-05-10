"use strict";
exports.__esModule = true;
var mustSing = {
    marking: {
        // I think this is now broken -- we need to change this to just be Default Right True
        "walk": { getDefault: { "Right": true } },
        "run": { getDefault: { "Right": true } },
        "sink": { getDefault: { "Right": false } },
        "swim": { getDefault: { "Right": false } },
        "eat": { getDefault: { "Left": false } },
        "drink": { getDefault: { "Right": false } }
    },
    andOrTree: {
        nodetype: "all",
        pre: "all of",
        children: [
            { leaf: "walk" },
            { nodetype: "not", children: [{ nodetype: "any", pre: "either",
                        children: [{ leaf: "sink" }, { leaf: "swim" }]
                    }] },
            { nodetype: "all", pre: "both",
                children: [{ leaf: "eat" }, { leaf: "drink" }]
            }
        ]
    }
};
console.log(JSON.stringify(mustSing));
