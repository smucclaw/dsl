"use strict";
exports.__esModule = true;
exports.mustSingTree = void 0;
exports.mustSingTree = {
    tag: "All",
    contents: [
        { tag: "Pre", contents: "all of:" },
        [{ tag: "Leaf", contents: "walk" },
            { tag: "Not", contents: { tag: "Any", contents: [{ tag: "Pre", contents: "either" },
                        [{ contents: "swim", tag: "Leaf" },
                            { contents: "sink", tag: "Leaf" }
                        ]
                    ]
                }
            },
            { tag: "Any", contents: [{ tag: "Pre", contents: "either" },
                    [{ contents: "eat", tag: "Leaf" },
                        { contents: "drink", tag: "Leaf" }]] }
        ]
    ]
};
