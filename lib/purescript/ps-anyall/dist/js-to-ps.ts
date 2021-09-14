
import { fromNode1, fromNode2, fromNode3 } from "./anyall"

console.log("* now we call fromNode1")
console.log(fromNode1)

console.log("* now we call fromNode2")
console.log(fromNode2("moo"))

console.log("* now we call fromNode3")
console.log(JSON.stringify(fromNode3,null,2))

