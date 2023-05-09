import StdinSchema from "./types"
import type { LeafNode, AnyNode, AllNode, NotNode } from "./types"

export const mustSingTree : AllNode = {
    tag: "All",
    contents: [
	{ tag: "Pre", contents: "all of:" },
	[ { tag: "Leaf", contents: "walk" },
	  { tag: "Not", contents:
	    { tag: "Any", contents:
	      [ { tag: "Pre", contents: "either" },
		[ { contents: "sink", tag: "Leaf" },
		  { contents: "swim", tag: "Leaf" }
		]
	      ]
	    }
	  },
	  { tag: "Any" , contents: [ { tag: "Pre", contents: "either" },
	      			     [ { contents: "eat",   tag: "Leaf" },
	      			       { contents: "drink", tag: "Leaf" } ] ] }
	]
    ]
}
