import StdinSchema from "./types"

let mustSing : StdinSchema = {
    marking: {
      "walk":  { "Right": true  },
      "run":   { "Right": true  },
      "sink":  { "Right": false },
      "swim":  { "Right": false },
      "eat":   { "Left":  false },
      "drink": { "Right": false },
  },
    andOrTree: {
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
	      { tag: "All" , contents: [ { tag: "Pre", contents: "both" },
	      				 [ { contents: "eat",   tag: "Leaf" },
	      				   { contents: "drink", tag: "Leaf" } ] ] }
	    ]
	    ]
    }
}

console.log(JSON.stringify(mustSing));
