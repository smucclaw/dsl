import StdinSchema from "./types"
import { mustSingTree } from "./mustsing"

let mustSing : StdinSchema = {
    marking: {
      "walk":  { "Right": true  },
      "run":   { "Right": true  },
      "sink":  { "Right": false },
      "swim":  { "Right": false },
      "eat":   { "Left":  false },
      "drink": { "Right": false },
    },
    andOrTree: mustSingTree
}

console.log(JSON.stringify(mustSing));
