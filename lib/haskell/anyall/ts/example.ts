import StdinSchema from "./types"

let mustSing : StdinSchema = {
    marking: {
	// I think this is now broken -- we need to change this to just be Default Right True
    "walk":  { fromUser: true },
    "run":   { fromUser: true },
//    "drink": { fromUser: false },
    "eat":   { fromUser: true  }
  },
  andOrTree: {
    nodetype: "all",
    pre: "all of",
    children: [
	{ leaf: "walk" },
	{ nodetype: "not", children: [{ nodetype: "all"
					, pre: "either"
					, children: [ { leaf: "sink" }, { leaf: "swim" } ]
				      }] },
	{ nodetype: "any"
	  , pre: "either"
	  , children: [ { leaf: "eat" }, { leaf: "drink" } ]
	}
    ]
  }
}

console.log(JSON.stringify(mustSing));
