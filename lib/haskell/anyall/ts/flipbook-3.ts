import StdinSchema from "./types"
import { mustSingTree } from "./mustsing"

let mustSing : StdinSchema = {
    andOrTree: mustSingTree,
    marking: { // we begin with no markings at all.
	"eat": { "Left": false }, // let's assume eat = false
	"drink": { "Left": true }, // this is now a parent node
	"non-alcoholic": { "Left": true }, // this leaf node should bubble to the top in soft mode, but not in hard.
  },
}

console.log(JSON.stringify(mustSing));
