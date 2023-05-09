import StdinSchema from "./types"
import { mustSingTree } from "./mustsing"

let mustSing : StdinSchema = {
    andOrTree: mustSingTree,
    marking: {
      "eat":   { "Left": false }, // let's assume eat = false
      "drink": { "Left": true }, // let's assume eat = false
      "walk":  { "Left": true  }, // we assume that everybody walks
      "swim":  { "Right": true },  // we assume that most people do swim
      "sink":  { "Right": false }, // we assume that most people don't sink -- and this is what gets us a connection
  },
}

console.log(JSON.stringify(mustSing));
