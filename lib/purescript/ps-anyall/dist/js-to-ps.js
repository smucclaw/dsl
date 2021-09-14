"use strict";
exports.__esModule = true;
var anyall_1 = require("./anyall");
console.log("* now we call fromNode1");
console.log(anyall_1.fromNode1);
console.log("* now we call fromNode2");
console.log((0, anyall_1.fromNode2)("moo"));
console.log("* now we call fromNode3");
console.log(JSON.stringify(anyall_1.fromNode3, null, 2));
