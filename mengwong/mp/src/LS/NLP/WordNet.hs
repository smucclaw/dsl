{-# LANGUAGE TransformListComp #-}

module LS.NLP.WordNet where

import Data.List (isPrefixOf, sortOn, isSuffixOf, nub)
import Text.EditDistance
import qualified Data.Text.Lazy     as Text
import           Data.Text.Lazy              (Text)
import WordNet.DB
import WordNet.Structured

wnNounDerivations :: Text -> IO Text
wnNounDerivations ogWord = do
  {- Suppose that ogWord is "respond". The word "respond" belongs to three synsets:
      1) sWords=[react, respond],         defn=show a response or a reaction to something
      2) sWords=[answer, reply, respond], defn=react verbally
      3) sWords=[respond]                 defn=respond favorably; "cancer responded to therapy"
    In vanilla WordNet, derivation is a morphological feature, so you would't get "reaction" for "respond".
    But in haskell-wordnet, the function getDerivations' takes a single word, like "respond",
    and looks up the derivations of /all words in all its synsets/: [react, respond, answer, reply].
    Furthermore, it even looks at the synsets of all the derivations, so we get quite a few links ahead.

    Simplified example: suppose respond only has two synonyms over all synsets, "react" and "respond".
    respond --synons--> [react, respond]                              -- synonyms of responds over /all synsets/
            --derivs--> [reaction, responder, …]                      -- derivations of all those synonyms
            --synDer--> [reaction, response, responder, answerer, …]  -- synonyms of the derivations, TODO: only from legit synsets?

    The last step is important: we are not going to get junk like "chemical reaction" as a derivation of "respond",
    because the sense in which "react" is a synonym of "respond", does not lead to "chemical reaction". TODO: how does this work exactly?

    TODO: explore wnsns and frequency order within a synset vs. frequency order of synsets
  -}
  resultRaw <- getDerivations' $ Text.unpack ogWord :: IO [(Synset, [(SynsetLink, Synset)])]
  let ogWordStr = Text.unpack ogWord
      ogWordLen = length ogWordStr
      candidates =
        [ (editDistance, prefixDistance, weightedEditDistance, candidate, fromWord, toWord, probableSuffix) -- "derivSynset="++ show (sWords derivSynset)) --take 20 (defn derivSynset))
        | (ogSynset, derivs) <- resultRaw
        , (synsetLink, derivSynset) <- derivs -- Each of these
        , let fromWord = getWord ogSynset (lfrm synsetLink)
        , let toWord = getWord derivSynset (lto synsetLink)

        -- Derivation must be noun, and not a human: we want an abstract noun (close->closure, not person)
        , isNoun derivSynset         -- 100% reliable: POS is in WN data
        , not (isHuman derivSynset)  -- Heuristic based on gloss: filter out defns like "a person who", "someone", …
        , (editDistance, candidate) <- sortByEditDistance ogWord derivSynset
        , not $ looksLikeHuman candidate -- Heuristic based on word: remove those that end in -or, -ee, …

        -- Sorting heuristics
        , let weight = 3 -- completely arbitrary number here
        , let prefixDistance = prefixSimilarity ogWordStr candidate
        , let weightedEditDistance = editDistance `div` weight + prefixDistance
        , let fromOgWord = whichword ogSynset == lfrm synsetLink -- Does the candidate come from ogWord or one of its synonyms
          -- faster way to say that ogWord == fromWord,
          -- because getWord (whichWord ogSynset) ogSynset == ogWord

        -- Not so promising heuristics
       , let probableSuffix = fromEnum $ not $ or [suf `isSuffixOf` candidate | suf <- ["ion", "ing", "ment", "ance", "ancy", "ure"]]
--        , let candidateEquals = candidate == toWord -- seems unreliable
        , let sortMeasure = if fromOgWord -- && candidateEquals -- TODO: this is completely ad hoc
                              then (0, weightedEditDistance, probableSuffix)
                              else (weightedEditDistance, probableSuffix, 0)
        , then sortOn by sortMeasure
        ]
  let result =
        case candidates of
              (_,_,dist,noun,_,_,_):_ ->  -- Check 1: is
                if dist >= ogWordLen
                  then mkGerund ogWordStr
                  else noun
              [] -> mkGerund ogWordStr
  appendFile "test.txt" $ prettyPrintResult ogWord candidates
  pure $ Text.pack result

prefixSimilarity :: String -> String -> Int
prefixSimilarity expect prospect = levenshteinDistance myEditCosts expect (take (length expect) prospect)

-- For debugging/testing heuristics
prettyPrintResult :: Text -> [(Int, Int, Int, String, String, String, Int)] -> String
prettyPrintResult ogWord res = unlines $ nub
  [ Text.unpack ogWord
  , unlines $ nub $ map show res
  ]

-- Last resort: make gerund ourselves. These rules are copied from the GF RGL smart paradigms.
mkGerund :: String -> String
mkGerund cry = case reverse cry of
        'e':'e':_   -> cry ++ "ing"           -- bungee -> bungeeing
        'e':'i':d   -> reverse d  ++ "ying" ; -- die -> dying
        'e':us      -> reverse us ++ "ing" ;  -- use -> using
        'r':'e':ent -> cry ++ "ing" ;         -- enter -> entering
        _           -> duplFinal cry ++ "ing" -- jar -> jarring
  where
    duplFinal :: String -> String
    duplFinal w = case reverse w of
        c:v:aeo:_   | isVowel v && isAEO aeo -> w           -- waiting, needing
        c:v:_:_:_:_ | isVowel v && isDuplCons c -> w        -- happening, fidgeting
        c:v:_       | isVowel v && isDuplCons c -> w ++ [c] -- omitting, winning
        _ -> w

    isAEO v = v `elem` ("aeo" :: String)
    isVowel v =  v `elem` ("aeiou" :: String)
    isDuplCons c = c `elem` ("bdgmnprt" :: String)

isHuman :: Synset -> Bool
isHuman synset = or [pref `isPrefixOf` def | pref <- humanPrefixes] || aPersonWho (words def)
  where
    def = defn synset
    humanPrefixes = ["(a person", "(someone", "(one who", "(a licensed practitioner", "(the party"]
    aPersonWho ("(a":_:"who":_) = True
    aPersonWho ("(an":_:"who":_) = True
    aPersonWho ("(the":_:"who":_) = True
    aPersonWho _ = False

looksLikeHuman :: String -> Bool
looksLikeHuman w = "or" `isSuffixOf` w || "ee" `isSuffixOf` w || ("er" `isSuffixOf` w && w `notElem` legitErWords)
  where legitErWords = ["answer"] --TODO: more

isNoun :: Synset -> Bool
isNoun Synset {pos=Noun} = True
isNoun _ = False

type SimilarityScore = Int

sortByEditDistance :: Text -> Synset -> [(SimilarityScore, String)]
sortByEditDistance w synset =
  [ (score, candidate)
  | candidate <- sWords synset
  , let score = levenshteinDistance myEditCosts w' candidate]
  where
    w' = Text.unpack w

myEditCosts :: EditCosts
myEditCosts = defaultEditCosts {
  substitutionCosts = VariableCost cheapSubs
  }
  where
    -- Only heuristic, no way to check this happens in the intended context
    -- Evaluate with more data and remove if needed
    cheapSubs ('y','i') = 0 -- apply   -> application
    cheapSubs ('z','s') = 0 -- analyze -> analysis
    cheapSubs ('e','i') = 0 -- close   -> closing
    cheapSubs ('d','s') = 0 -- respond -> response
    cheapSubs _         = 1