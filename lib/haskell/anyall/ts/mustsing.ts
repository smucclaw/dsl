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
		[ { contents: "swim", tag: "Leaf" },
		  { contents: "sink", tag: "Leaf" }
		]
	      ]
	    }
	  },
	  { tag: "Any",
	    contents:
	    [ { tag: "Pre", contents: "either" },
	      [ { contents: "eat",   tag: "Leaf" },
	      	{ tag: "Any",
		  contents:
		  [ { tag: "Pre", contents: "drink" },
		    [ { tag: "Leaf", contents: "alcoholic" },
		      { tag: "Leaf", contents: "non-alcoholic" }
		    ]
		  ]}
	      ]
	    ]
	  }
	]
    ]
}
