
import * as AA from "./anyall"

let say = console.log

say("This output is intended to be opened in Emacs Org-Mode -- node dict/js-to-ps.js | tee output.org")
say("Once in emacs, use TAB to cycle folding/unfolding visibility.")
say("But also have the source code open alongside for inspection.")

say("* First we learn how to call into a purescript-generated library.")

say("** fromNode1 is a value");                          say(AA.fromNode1)
say("** fromNode2 is a function");                       say(AA.fromNode2("moo"))
say("** fromNode3 is a complex data structure");         code(AA.fromNode3, true)

function code( obj
               , asjson : boolean = false
               , style="src javascript"
             ) {
  say("#+begin_"+style)
  if (asjson) { say(JSON.stringify(obj,null,2)) }
  else        { say(obj) }
  say("#+end_"+style)
}

say("** function calling is curried, so each argument gets its own ()")
say(AA.howDoWeEven("input string")(100))

say("* Let's learn the two important input datatypes we'll be using in the paint() call")

say("** example1 :: Item String")
say("This represents the logic of the rule: how predicates conjoin with one another in an and/or tree.")
say("The /Leaf/ nodes contain strings which show up again in the Marking, below.")

say("*** raw");                                         code(AA.example1)
say("*** json");                                        code(JSON.stringify(AA.example1,null,0))
say("*** encoded by purescript for consumption in JS"); code(AA.example1_encoded, true)

say("** marking1 :: Marking")
say("A /Marking/ represents a particular state of true/false settings, typically read from the UI. We also use this to preset defaults.")

say("*** The Purescript ~Main.purs~ provides a ~marking1~ to go with ~example1~:")
say("**** marking1_encoded");                                 say(AA.marking1_encoded, true)
say("**** marking1_encoded then decoded");                    say(AA.marking1_decoded)
say("**** marking1_encoded then decoded then encoded again"); say(AA.marking1_recoded, true) // prove our codec works by roundtripping

say("*** We can make up a marking of our own for the simple rules. The JS UI produces this -- each click creates a new marking.")

let simpleMarking = {
    walk : { source: "default", value: "undefined" },
    run  : { source: "user",    value: "true" },       // "i don't like to run"
    drink: { source: "default", value: "true" },
    eat  : { source: "default", value: "true" }
}
code(simpleMarking, true)
say('When the user clicks on something, the /default/ changes to /user/, and the /value/ changes to either "true", "false", or "undefined".')

say("** We can make up our own rules, using the same encoding convention")
// https://github.com/paf31/purescript-foreign-generic/blob/master/generated-docs/Foreign/Generic/Class.md

// this is what the Purescript side of things calls an "Item"
let fancyRules = {
  tag: "All", contents:
  [ { tag: "Pre", contents: "all of" },
    [ { tag: "Leaf", contents: "perambulate" },
      { tag: "Leaf", contents: "accelerate"  },
      { tag: "Any",  contents: [
        { tag: "Pre", contents: "either" },
        [ { tag: "Leaf", contents: "ingest" },
          { tag: "Leaf", contents: "imbibe" } ]
      ] }
    ] ]
}
say("#+NAME: fancyRules");                                     code(fancyRules, true)

say("** We can make up a marking to go with it:")
let fancyMarking = {
  imbibe      : { source: "default", value: "true" },
  ingest      : { source: "default", value: "true" },
  perambulate : { source: "default", value: "true" },
  accelerate  : { source: "default", value: "undefined" }
}
say("#+NAME: fancyMarking");                                  code(fancyMarking, true)

say("* The purescript library is able to consume our rules and our markings:")

say("** external simple marking read into Purescript")
code(AA.decodeMarking(simpleMarking))

let decodedFancyRule = AA.decodeItemString(fancyRules)
say("** external fancy Rule read into Purescript");           code(decodedFancyRule, true)

say("** external fancy Marking read into Purescript")
let decodedFancyMarking = AA.decodeMarking(fancyMarking);     code(fancyMarking, true)

say("* now we call paint() for the fancyRules, with soft")
let paintOut2 = AA.paint(AA.soft)(fancyMarking)(AA.getNLByName("example1"))(decodedFancyRule)

say("NOTE: the input to ~paint~ takes a JS-style marking, but a PS-style (decoded) rule.")
say("This is because we expect the rules to typically be authored on the PS side.")

say("* TL;DR: now, most importantly, we use all this to call ~paint()~");

say("** we call paint() on a simpleMarking, in hard mode")
let paintsimple = AA.paint
                  (AA.hard)
                  (simpleMarking)
                  (AA.getNLByName("example1"))
                  (AA.getItemByName("example1"))

code(paintsimple, true)

say("** we call paint() on a fancyMarking, in soft mode")
let paintfancy =  AA.paint
                  (AA.soft)
                  (fancyMarking)
                  (AA.getNLByName("example1"))
                  (decodedFancyRule)

code(paintfancy, true)

