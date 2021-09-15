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
console.log("* we decode the encoded example1");
// https://github.com/paf31/purescript-foreign-generic/blob/master/generated-docs/Foreign/Generic/Class.md
console.log(AA.decodeItemString(AA.example1_encoded));
// "marking1" is sourced from inside PS
console.log("* marking1_encoded");
console.log(AA.marking1_encoded);
console.log("* marking1_encoded then decoded");
console.log(AA.marking1_decoded);
console.log("* marking1_encoded then decoded then encoded again");
console.log(AA.marking1_recoded);
console.log("* we make up a marking of our own for the simple rules. the UI produces this -- each click creates a new marking.");
var simpleMarking = {
    walk: { source: "default", value: "undefined" },
    run: { source: "user", value: "false" },
    drink: { source: "default", value: "true" },
    eat: { source: "default", value: "true" }
};
console.log(simpleMarking);
console.log("* external simple marking read into Purescript");
console.log(AA.decodeMarking(simpleMarking));
// the UI calls this function to paint the elements with view, etc.
console.log("* now we call paint() for example 1");
var paintOut = AA.paint(AA.hard)(simpleMarking)(AA.example1_nl)(AA.getItemByName("example1"));
console.log("* paint() JSONified");
console.log(JSON.stringify(paintOut, null, 0));
console.log("* paint() JSONified with a bit more breathing room");
console.log(JSON.stringify(paintOut, null, 2));
// let's try it with the example we made up
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
console.log("* we made up a fancy version of the rules:");
console.log(fancyRules);
console.log("* we import the fancy rules as example2");
var example2 = AA.decodeItemString(fancyRules);
console.log(example2);
console.log("* now we make up a marking for the fancy rules");
var fancyMarking = {
    imbibe: { source: "default", value: "true" },
    ingest: { source: "default", value: "true" },
    perambulate: { source: "default", value: "true" },
    accelerate: { source: "default", value: "undefined" }
};
console.log(fancyMarking);
console.log("* external fancy marking read into Purescript");
console.log(AA.decodeMarking(fancyMarking));
console.log("* now we call paint() for the fancyRules, with soft");
var paintOut2 = AA.paint(AA.soft)(fancyMarking)(AA.example1_nl)(example2);
// todo: be able to take NLDict input -- we need a decode function
console.log("* 2 paint() JSONified");
console.log(JSON.stringify(paintOut2, null, 0));
console.log("* 2 paint() JSONified with a bit more breathing room");
console.log(JSON.stringify(paintOut2, null, 2));
