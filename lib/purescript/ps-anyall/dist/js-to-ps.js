"use strict";
exports.__esModule = true;
var AA = require("./anyall");
console.log("* now we call fromNode1");
console.log(AA.fromNode1);
console.log("* now we call fromNode2");
console.log(AA.fromNode2("moo"));
console.log("* now we call fromNode3");
console.log(JSON.stringify(AA.fromNode3, null, 0));
console.log("* function calling is curried");
console.log(AA.howDoWeEven("input string")(100));
console.log("* now we print example1 :: Item String");
console.log("** raw");
console.log(AA.example1);
console.log("** json");
console.log(JSON.stringify(AA.example1, null, 0));
console.log("** example1_encoded");
console.log(AA.example1_encoded);
console.log("** example1_encoded as JSON");
console.log(JSON.stringify(AA.example1_encoded));
console.log("* now we make up our own rules, using the same encoding convention");
// https://github.com/paf31/purescript-foreign-generic/blob/master/generated-docs/Foreign/Generic/Class.md
// this is what the Purescript side of things calls an "Item"
var fancyRules = {
    tag: "All", contents: [{ tag: "Pre", contents: "all of" },
        [{ tag: "Leaf", contents: "perambulate" },
            { tag: "Leaf", contents: "accelerate" },
            { tag: "Any", contents: [
                    { tag: "Pre", contents: "either" },
                    [{ tag: "Leaf", contents: "ingest" },
                        { tag: "Leaf", contents: "imbibe" }]
                ] }
        ]]
};
console.log("we made up a fancy version of the rules:");
console.log(fancyRules);
// "marking1" is sourced from inside PS
console.log("* marking1_encoded");
console.log(AA.marking1_encoded);
console.log("* marking1_encoded then decoded");
console.log(AA.marking1_decoded);
console.log("* marking1_encoded then decoded then encoded again");
console.log(AA.marking1_recoded);
console.log("* we make up a marking of our own for the simple rules. the UI produces this -- each click creates a new marking.");
var simpleMarking = {
    drink: { source: "default", value: "true" },
    eat: { source: "default", value: "true" },
    walk: { source: "default", value: "true" },
    run: { source: "user", value: "false" } // "i don't like to run"
};
console.log(simpleMarking);
console.log("* now we make up a marking of our own, for the fancy rules");
var fancyMarking = {
    imbibe: { source: "default", value: "true" },
    ingest: { source: "default", value: "true" },
    perambulate: { source: "default", value: "true" },
    accelerate: { source: "default", value: "true" }
};
console.log(fancyMarking);
console.log("* external simple marking read into Purescript");
console.log(AA.decodeMarking(simpleMarking));
console.log("* external fancy marking read into Purescript");
console.log(AA.decodeMarking(fancyMarking));
// the UI calls this function to paint the elements with view, etc.
console.log("* now we call paint()");
var paintOut = AA.paint(AA.hard)(simpleMarking)(AA.getItemByName("example1"));
console.log("* paint() JSONified");
console.log(JSON.stringify(paintOut, null, 0));
console.log("* paint() JSONified with a bit more breathing room");
console.log(JSON.stringify(paintOut, null, 2));
