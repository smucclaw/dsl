import StdinSchema from "./types"
import { mustSingTree } from "./mustsing"

let mustSing : StdinSchema = {
    andOrTree: mustSingTree,
    marking: { // we begin with no markings at all.
  },
}

console.log(JSON.stringify(mustSing));
