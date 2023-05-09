import StdinSchema from "./types"
import { mustSingTree } from "./mustsing"

let mustSing : StdinSchema = {
    andOrTree: mustSingTree,
    marking: {
	"eat": { "Left": false }, // let's assume eat = false
	"drink": { "Left": true }, // let's assume eat = false
      "walk":  { "Left": true  }, // we assume that everybody walks
      "swim":  { "Left": true },   // we assume that most people do swim
  },
}

console.log(JSON.stringify(mustSing));
