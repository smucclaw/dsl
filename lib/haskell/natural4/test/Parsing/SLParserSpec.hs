{-# LANGUAGE OverloadedStrings #-}
module Parsing.SLParserSpec where

import Text.Megaparsec
import LS.Lib
import LS.Parser
import LS.RelationalPredicates
import LS.Tokens
import AnyAll hiding (asJSON)
import LS.BasicTypes
import LS.Types
import LS.Rule
import Test.Hspec
import qualified Data.ByteString.Lazy as BS
import Control.Monad (when, guard)
import qualified Data.Text as T
import Test.Hspec.Megaparsec (shouldParse)


filetest :: (HasCallStack, ShowErrorComponent e, Show b, Eq b) => String -> String -> (String -> MyStream -> Either (ParseErrorBundle MyStream e) b) -> b -> SpecWith ()
filetest testfile desc parseFunc expected =
  it (testfile ++ ": " ++ desc ) $ do
  testcsv <- BS.readFile ("test/Parsing/slparser/" <> testfile <> ".csv")
  parseFunc testfile `traverse` exampleStreams testcsv
    `shouldParse` [ expected ]

xfiletest :: (HasCallStack, ShowErrorComponent e, Show b, Eq b) => String -> String -> (String -> MyStream -> Either (ParseErrorBundle MyStream e) b) -> b -> SpecWith ()
xfiletest testfile _desc parseFunc expected =
  xit testfile $ do
  testcsv <- BS.readFile ("test/Parsing/slparser/" <> testfile <> ".csv")
  parseFunc testfile `traverse` exampleStreams testcsv
    `shouldParse` [ expected ]

spec :: Spec
spec = do
    let runConfig = defaultRC { sourceURL = "test/Spec" }
        runConfigDebug = runConfig { debug = True }
    let  combine (a,b) = a ++ b
    let _parseWith1 f x y s = f <$> runMyParser combine runConfigDebug x y s
    let  parseR       x y s = runMyParser combine runConfig x y s
    let _parseR1      x y s = runMyParser combine runConfigDebug x y s
    let  parseOther   x y s = runMyParser id      runConfig x y s
    let _parseOther1  x y s = runMyParser id      runConfigDebug x y s

    describe "Parsing primitives" $ do
      filetest "primitive-pNumber" "primitive number"
        (parseOther pNumber) (42, [])

      filetest "primitive-number-string-multi" "primitive number"
        (parseOther ( (,) <$> pNumber <*> pOtherVal )) ( (42,"boo"), [])

      filetest "primitive-number-string-single" "primitive number"
        (parseOther ( (,)
                      <$> pNumber
                      <*> someIndentation pOtherVal
                      ))
        ( (42,"boo"), [])

      filetest "primitive-number-string-single-indented-1" "primitive number boo indented"
        (parseOther ( (,)
                      <$> pNumber
                      <*> someIndentation pOtherVal
                      ))
        ( (42,"boo"), [])

      filetest "primitive-number-string-single-indented-1" "primitive number boo indented"
        (parseOther ( (,)
                      $>| pNumber
                      |>< pOtherVal
                    ))
        ( (42,"boo"), [])

      filetest "primitive-number-string-single-indented-2" "primitive number boo indented"
        (parseOther ( (,)
                      $*| ($>>) pNumber
                      |>< pOtherVal
                    ))
        ( (42,"boo"), [])

      filetest "primitive-number-string-single-indented-3" "primitive number boo indented"
        (parseOther ( (,)
                      $*| ($>>) pNumber
                      |>< pOtherVal
                    ))
        ( (42,"boo"), [])

      filetest "primitive-pOtherVal" "primitive number"
        (parseOther pOtherVal) ("this is a string", [])

      filetest "primitive-pOtherVal-indented" "primitive number"
        (parseOther ( id
                      $*| ($>>) pOtherVal
                      |<$ undeepers
                    ))
        ( "this is a string", [])

      filetest "primitive-pOtherVal-indented" "primitive number"
        (parseOther ( id
                      >>| pOtherVal -- consumes GoDeepers, then returns a plain parser upgraded to fancy
                      |<$ undeepers
                    ))
        ( "this is a string", [])

      filetest "primitive-pOtherVal-indented" "primitive number"
        (parseOther ( (>><) pOtherVal )) -- consumes GoDeepers, then runs the plain parser, and runs undeepers
        ( "this is a string", [])

      filetest "compound-pairOfNumbers-1" "primitive number"
        (parseOther ( (,)
                      <$> pNumber
                      <*> someIndentation pNumber
                      ))
        ( (42, 43), [])

      filetest "compound-pairOfNumbers-2" "compound-pair"
        (parseOther ( (,)
                      >>| pNumber
                      |>< pNumber
                    ))
        ( (42,43), [])

      let slPairNum = (,)
                      >>| pNumber
                      |>| pNumber

      filetest "compound-pairOfNumbers-3" "compound-pair"
        (parseOther ( slPairNum |<$ undeepers ))
        ( (42,43), [])

      filetest "compound-pairOfNumbers-4" "compound-pair"
        (parseOther ( (,)
                      $*| slPairNum
                      |>< pOtherVal
                    ))
        ( (  (42,43)
          , "my string"), [])

      filetest "inline-1-a" "line crossing"
        (parseOther ( (,,)
                      >*| slMultiTerm
                      |<| pToken Means
                      |>| pBSR
                      |<$ undeepers
                    ))
        ( ( [MTT "Food"]
          , Means
          , Any (Just $ Pre "yummy nightshades") [ mkLeaf (RPMT [MTT "potato"])
                                                 , mkLeaf (RPMT [MTT "tomato"])]
          ), []
        )

      it "SLParser combinators 1 /+=" $ do
        parseOther ((,,)
                     $*| (someLiftSL (pToken (Other "foo")))
                     |>| pToken (Other "bar")
                     |>< pToken (Other "qux")) ""
         (exampleStream "foo,foo,foo,bar,qux")
          `shouldParse` (([Other "foo",Other "foo",Other "foo"],Other "bar",Other "qux"),[])

      it "SLParser combinators 2 /+=" $ do
        parseOther ((,,)
                     $*| pToken (Other "foo") /+= (liftSL $ pToken (Other "bar"))
                     |>| pToken (Other "bar")
                     |>< pToken (Other "qux")) ""
         (exampleStream "foo,foo,foo,bar,qux")
          `shouldParse` ((([Other "foo",Other "foo",Other "foo"],Other "bar"),Other "bar",Other "qux"),[])

      it "SLParser combinators 3 /+=" $ do
        parseOther ((,,)
                     $*| (pOtherVal) /+= ((,) >>| (pToken (Other "bar")) |>| pOtherVal)
                     |>| pToken (Other "bar")
                     |>< pToken (Other "qux")) ""
         (exampleStream "foo,foo,foo,bar,qux")
          `shouldParse` (((["foo","foo","foo"],(Other "bar", "qux")),Other "bar",Other "qux"),[])

      it "SLParser combinators 4 /+=" $ do
        parseOther ((,,)
                     $*| (pOtherVal) /+= ((,) >>| pOtherVal |>| pOtherVal)
                     |>| pToken (Other "bar")
                     |>< pToken (Other "qux")) ""
         (exampleStream "foo,foo,foo,bar,qux")
          `shouldParse` (((["foo","foo","foo"],("bar", "qux")),Other "bar",Other "qux"),[])

-- [TODO] disturbingly, this fails if we use the "standard" version from Parser.
      let aNLK :: Int -> SLParser (MultiTerm,MyToken)
          aNLK maxDepth = mkSL $ do
            (toreturn, n) <- runSL aboveNextLineKeyword2
            debugPrint $ "got back toreturn=" ++ show toreturn ++ " with n=" ++ show n ++ "; maxDepth=" ++ show maxDepth ++ "; guard is n < maxDepth = " ++ show (n < maxDepth)
            guard (n < maxDepth)
            return (toreturn, n)

      it "SLParser combinators 5 aboveNextLineKeyword2" $ do
        parseOther ((,,)
                    $>| pOtherVal
                    |*| aboveNextLineKeyword2
                    |>< pOtherVal
                   ) ""
         (exampleStream "foo,foo,foo,\n,OR,bar")
          `shouldParse` (("foo"                          -- pOtherVal
                         ,([MTT "foo",MTT "foo"],LS.Types.Or)    -- aboveNextLineKeyword
                         ,"bar")                         -- pOtherVal
                        ,[])

      it "SLParser combinators 6 greedy star" $ do
        parseOther ((,,,)
                     $*| (debugName "first outer pOtherVal" pOtherVal /*= aNLK 1)
                     |>| debugName "looking for foo3"    pOtherVal
                     |<| debugName "looking for the Or" (pToken LS.Types.Or)
                     |>< debugName "looking for the Bar" pOtherVal
                   ) ""
         (exampleStream "foo1,foo2,foo3,\n,OR,bar")
          `shouldParse` ( ( ( ["foo1","foo2"], ( [MTT "foo3"],LS.Types.Or ) )
                          , "foo3"
                          , LS.Types.Or
                          , "bar" )
                        ,[])

      it "SLParser combinators 7 nongreedy star" $ do
        parseOther ((,,,)
                     $*| (debugName "first outer pOtherVal" pOtherVal /*?= aNLK 1)
                     |>| debugName "looking for foo3"    pOtherVal
                     |<| debugName "looking for the Or" (pToken LS.Types.Or)
                     |>< debugName "looking for the Bar" pOtherVal
                   ) ""
         (exampleStream "foo1,foo2,foo3,\n,OR,bar")
          `shouldParse` ( ( ( ["foo1","foo2"], ( [MTT "foo3"],LS.Types.Or ) )
                          , "foo3"
                          , LS.Types.Or
                          , "bar" )
                        ,[])

      it "SLParser combinators 8 greedy plus" $ do
        parseOther ((,,,)
                     $*| (debugName "first outer pOtherVal" pOtherVal /+= aNLK 1)
                     |>| debugName "looking for foo3"    pOtherVal
                     |<| debugName "looking for the Or" (pToken LS.Types.Or)
                     |>< debugName "looking for the Bar" pOtherVal
                   ) ""
         (exampleStream "foo1,foo2,foo3,\n,OR,bar")
          `shouldParse` ( ( ( ["foo1","foo2"], ( [MTT "foo3"],LS.Types.Or ) )
                          , "foo3"
                          , LS.Types.Or
                          , "bar" )
                        ,[])

      it "SLParser combinators 9 nongreedy plus" $ do
        parseOther ((,,,)
                     $*| (debugName "first outer pOtherVal" pOtherVal /+?= aNLK 1)
                     |>| debugName "looking for foo3"    pOtherVal
                     |<| debugName "looking for the Or" (pToken LS.Types.Or)
                     |>< debugName "looking for the Bar" pOtherVal
                   ) ""
         (exampleStream "foo1,foo2,foo3,\n,OR,bar")
          `shouldParse` ( ( ( ["foo1","foo2"], ( [MTT "foo3"],LS.Types.Or ) )
                          , "foo3"
                          , LS.Types.Or
                          , "bar" )
                        ,[])

      it "SLParser combinators 10 maxdepth 0" $ do
        parseOther ((,,,)
                     $*| (debugName "first outer pOtherVal" pOtherVal /*?= aNLK 1)
                     |>| debugName "looking for foo3"    pOtherVal
                     |<| debugName "looking for the Or" (pToken LS.Types.Or)
                     |>< debugName "looking for the Bar" pOtherVal
                   ) ""
         (exampleStream "foo1,foo2,,,foo3,\n,,,OR,bar")
          `shouldParse` ( ( ( ["foo1","foo2"], ( [MTT "foo3"],LS.Types.Or ) )
                          , "foo3"
                          , LS.Types.Or
                          , "bar" )
                        ,[])

      let inline_1 = ( ( [MTT "Bad"] , Means , inline_pp ), [] )
          inline_2 = ( ( [MTT "Bad"] , Means , inline_p  ), [] )
          inline_3 = ( ( [MTT "Bad"] , Means , inline_   ), [] )
          inline_r = (([MTT "multiwonk"],Means,Any Nothing [mkLeaf (RPMT [MTT "poopoo"]),Any (Just (Pre "the")) [mkLeaf (RPMT [MTT "honk"]),mkLeaf (RPMT [MTT "ponk"])]]),[])
          inline_4 = ( ( [MTT "a data breach occurred"] , Means , inline_4xs), [] )
          inline_pp = Any (Just $ PrePost "any unauthorised" "of personal data" ) inline_xs
          inline_p  = Any (Just $ Pre     "any unauthorised"                    ) inline_xs
          inline_   = Any Nothing                                                 inline_xs
          inline_xs = mkLeaf <$> ( RPMT <$> ((:[]) <$> (MTT <$> ["access", "use", "disclosure", "copying", "modification", "disposal"])))
          inline4_pp= Any (Just $ PrePost
                           "loss of storage medium on which personal data is stored in circumstances where the unauthorised"
                           "of the personal data is likely to occur") inline_xs
          inline_4xs= Any Nothing [inline_pp, inline4_pp]

          pInline1 = parseOther $ do
            (,,)
              >*| debugName "subject slMultiTerm" slMultiTerm  -- "Bad"
              |<| pToken Means
              |-| debugName "made it to pBSR" pBSR
              |<$ undeepers

      filetest "inline-1-c" "line crossing" pInline1 inline_1
      filetest "inline-1-d" "line crossing" pInline1 inline_1
      filetest "inline-1-e" "line crossing" pInline1 inline_1
      filetest "inline-1-f" "line crossing" pInline1 inline_1
      filetest "inline-1-g" "line crossing" pInline1 inline_1
      filetest "inline-1-h" "line crossing" pInline1 inline_1
      filetest "inline-1-i" "line crossing" pInline1 inline_1
      filetest "inline-1-j" "line crossing" pInline1 inline_1
      filetest "inline-1-k" "line crossing" pInline1 inline_1
      filetest "inline-1-l" "line crossing" pInline1 (
        const [DefTypically { name = [MTT "disposal"]
                            , defaults = [RPConstraint [MTT "disposal"] RPis [MTB True]]
                            , srcref = mkTestSrcRef 3 8}]
          <$> inline_1)
      filetest "inline-1-m" "line crossing" pInline1 inline_1
      filetest "inline-1-n" "line crossing" pInline1 inline_2
      filetest "inline-1-o" "line crossing" pInline1 inline_3
      filetest "inline-1-p" "line crossing" pInline1 (([MTT "wonk"],Means,Any (Just (Pre "a")) [mkLeaf (RPMT [MTT "honk"]),mkLeaf (RPMT [MTT "ponk"])]),[])
      filetest "inline-1-q" "line crossing" pInline1 (([MTT "poowonk"],Means,Any Nothing [mkLeaf (RPMT [MTT "poopoo"]),mkLeaf (RPMT [MTT "just a",MTT "honk"])]),[])
      filetest "inline-1-r" "line crossing" pInline1 inline_r
      filetest "inline-1-s" "line crossing" pInline1 inline_4

      filetest "multiterm-with-blanks-1" "p, no blanks"              (parseOther pMultiTerm) (MTT <$> ["foo","bar","baz"],[])
      filetest "multiterm-with-blanks-2" "p, with blanks"            (parseOther pMultiTerm) (MTT <$> ["foo","bar","baz"],[])

      filetest "multiterm-with-blanks-1" "sl, no blanks"             (parseOther (slMultiTerm |<$ undeepers)) (MTT <$> ["foo","bar","baz"],[])
      filetest "multiterm-with-blanks-2" "sl, with blanks"           (parseOther (slMultiTerm |<$ undeepers)) (MTT <$> ["foo","bar","baz"],[])


-- sl style
    describe "sameOrNextLine" $ do
      let potatoParser = parseOther (sameOrNextLine
                                      (flip const $>| (pToken Declare) |*| (someSL (liftSL pOtherVal)))
                                      (flip const $>| (pToken Has    ) |*| (someSL (liftSL pOtherVal))))
          potatoExpect = ( ( [ "Potato" ]
                           , [ "genus", "species" ] ), [] )

      filetest "sameornext-1-same"  "a b on same line"  potatoParser potatoExpect
      filetest "sameornext-2-next"  "a b on next line"  potatoParser potatoExpect
      filetest "sameornext-3-dnl"   "a b on next left"  potatoParser potatoExpect
      filetest "sameornext-4-right" "a b on next right" potatoParser potatoExpect

    describe "ampersand" $ do
      let nextLineP = myindented $ sameOrNextLine (someLiftSL pOtherVal) (someLiftSL pOtherVal)
      xfiletest "ampersand-1" "should fail to parse" -- [TODO] how do we run a shouldFailOn? we are expecting this to fail.
        (parseOther nextLineP) ((["Potato"],["genus", "species"]), [])
      filetest "ampersand-2" "this bed is just right"
        (parseOther nextLineP) ((["Potato"],["genus", "species"]), [])
      filetest "ampersand-3" "to the right shouldbe OK"
        (parseOther nextLineP) ((["Potato"],["genus", "species"]), [])
      xfiletest "ampersand-4" "to the right with extra should leave uncaptured uncaptured"
        (parseOther nextLineP) ((["Potato"],["genus", "species"]), [])
      filetest "ampersand-4" "to the right with extra"
        (parseOther nextLineP) ((["Potato", "uncaptured"],["genus", "species"]), [])
