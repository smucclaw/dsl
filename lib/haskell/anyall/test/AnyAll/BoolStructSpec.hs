{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module AnyAll.BoolStructSpec where

import Test.Hspec
import Data.Text (Text)
import AnyAll.BoolStruct
import Prelude hiding (any, all)
import AnyAll.Types
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import Test.Hspec.Checkers (testBatch)
import Test.Hspec.QuickCheck (prop)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy.Encoding
import Data.Text.Lazy (fromStrict)

all :: [BoolStruct (Maybe l) a] -> BoolStruct (Maybe l) a
all = All Nothing

allPre :: Text -> [BoolStruct (Maybe (Label Text)) a] -> BoolStruct (Maybe (Label Text)) a
allPre label = All (Just (Pre label))

anyPre :: Text -> [BoolStruct (Maybe (Label Text)) a] -> BoolStruct (Maybe (Label Text)) a
anyPre label = Any (Just (Pre label))

any :: [BoolStruct (Maybe l) a ] -> BoolStruct (Maybe l) a
any = Any Nothing

atom :: Text -> BoolStruct (Maybe Text) Text
atom = Leaf

type WireBoolStruct = BoolStruct (Maybe (Label Text)) Text

leaf :: Text -> WireBoolStruct
leaf = Leaf

data Jb = Jb { contents :: Text, tag :: Text } deriving (Show)

toJ :: Jb -> ByteString
--myfn C{name=n} = length n
toJ Jb {contents=c, tag=t } = encodeUtf8 $ fromStrict $ "{\"contents\":\"" <> c <> "\",\"tag\":\"" <> t <> "\"}"

spec :: Spec
spec = do
  describe "simplifyBoolStruct" do
    it "should leave Leaf " do
      simplifyBoolStruct (leaf "foo") `shouldBe` leaf "foo"

    it "should elevate nested All children" do
      simplifyBoolStruct ( all
                      [ all [leaf "foo1", leaf "foo2"],
                        all [leaf "bar", all [leaf "bat"]],
                        allPre "something"
                          [ leaf "baz",
                            allPre "something" [leaf "bbb"]
                          ],
                        anyPre "different something" [leaf "qux"]
                      ] )
        `shouldBe` all
                    [ leaf "foo1",
                      leaf "foo2",
                      leaf "bar",
                      leaf "bat",
                      allPre "something" [leaf "baz", leaf "bbb"],
                      leaf "qux"
                    ]

    it "should simplify singleton Leaf" do
      simplifyBoolStruct ( all [leaf "foo"] ) `shouldBe`  leaf "foo"

    it "should simplify singleton Leaf" do
      simplifyBoolStruct ( any [leaf "foo"] ) `shouldBe`  leaf "foo"

    it "should simplify not-nots" do
      simplifyBoolStruct ( Not $ Not $ leaf "not" ) `shouldBe`  leaf "not"

    it "collapse all" do
      simplifyBoolStruct ( allPre "a" [allPre "a" [leaf "foo", leaf "bar"], leaf "baz"] ) `shouldBe`  allPre "a" [leaf "foo", leaf "bar", leaf "baz"]

    it "does not collapse all" do
      simplifyBoolStruct (allPre "b" [allPre "a" [leaf "foo", leaf "bar"], leaf "baz"]) `shouldBe` allPre "b" [allPre "a" [leaf "foo", leaf "bar"], leaf "baz"]

    it "collapse any" do
      simplifyBoolStruct ( anyPre "a" [anyPre "a" [leaf "foo", leaf "bar"], leaf "baz"] ) `shouldBe`  anyPre "a" [leaf "foo", leaf "bar", leaf "baz"]

    it "does not collapse any" do
      simplifyBoolStruct (anyPre "b" [anyPre "a" [leaf "foo", leaf "bar"], leaf "baz"]) `shouldBe` anyPre "b"  [anyPre "a" [leaf "foo", leaf "bar"], leaf "baz"]

    it "does not collapse any all" do
      simplifyBoolStruct (anyPre "b" [allPre "a" [leaf "foo", leaf "bar"], leaf "baz"]) `shouldBe` anyPre "b"  [allPre "a" [leaf "foo", leaf "bar"], leaf "baz"]

    it "does not collapse all any" do
      simplifyBoolStruct (allPre "b" [anyPre "a" [leaf "foo", leaf "bar"], leaf "baz"]) `shouldBe` allPre "b"  [anyPre "a" [leaf "foo", leaf "bar"], leaf "baz"]

  describe "nnf transformation" do
    let
        a = atom "a"
        b = atom "b"

    it "nnf (not (not a)) == a" do
      nnf (Not . Not $ a) `shouldBe` a

    it "nnf (not (not all [not not a, not not b])) == all [a, b]" do
      nnf (Not . Not $ all [Not . Not $ a, Not . Not $ b]) `shouldBe` all [a, b]

    it "nnf (all [not not a, not not b]) == (all [a,b])" do
      nnf (all [Not . Not $ a, Not . Not $ b]) `shouldBe` all [a, b]

    it "nnf (not (all [a, b])) == (any [not a, not b])" do
      nnf (Not $ all [a, b]) `shouldBe` any [Not a, Not b]

    it "nnf (not (any a)) == (all (not b))" do
      nnf (Not $ any [a, b]) `shouldBe` all [ Not a, Not b]

  describe "extractLeaves" do
    let
        a = atom "a"
        b = atom "b"

    it "extractLeaves a == a" do
      extractLeaves a `shouldBe` ["a"]

    it "extractLeaves not a == a" do
      extractLeaves (Not a) `shouldBe` ["a"]

    it "extractLeaves (not (any [a, b])) == [a, b]" do
      extractLeaves (Not $ any [a, b]) `shouldBe` ["a", "b"]

    it "extractLeaves  (all [a, b]) == [a, b]" do
      extractLeaves (all [a, b]) `shouldBe` ["a", "b"]

  describe "addJust" do
    let
        a = Leaf "a" :: BoolStruct Text Text
        b = Leaf "b" :: BoolStruct Text Text
        anyAB = Any "" [a, b]
        aMaybe = Leaf "a" :: BoolStruct (Maybe Text) Text
        bMaybe = Leaf "b" :: BoolStruct (Maybe Text) Text

    it "addJust a == a" do
      addJust a `shouldBe` aMaybe

    it "addJust not a == a" do
      addJust (Not a) `shouldBe` Not aMaybe

    it "addJust (any [a, b]) == [a, b]" do
      addJust (Any "" [a, b]) `shouldBe` Any (Just "") [aMaybe, bMaybe]

    it "addJust (all [a, b]) == [a, b]" do
      addJust (All "" [a, b]) `shouldBe` All (Just "") [aMaybe, bMaybe]

  describe "alwaysLabeled" do
    let
        a = Leaf "a" :: BoolStruct (Label Text) Text
        b = Leaf "b" :: BoolStruct (Label Text) Text
        aMaybe = Leaf "a" :: BoolStruct (Maybe (Label Text)) Text
        bMaybe = Leaf "b" :: BoolStruct (Maybe (Label Text)) Text
        preLabel = Pre "Label"
        prePostLabel = PrePost "Label" "Suffix"

    it "alwaysLabeled a == a" do
      alwaysLabeled aMaybe `shouldBe` a

    it "alwaysLabeled not a == a" do
      alwaysLabeled (Not aMaybe) `shouldBe` Not a

    it "alwaysLabeled (any Nothing [a, b]) == (any (pre 'any of:') [a, b])" do
      alwaysLabeled (Any Nothing [aMaybe, bMaybe]) `shouldBe` Any (Pre "any of:") [a, b]

    it "alwaysLabeled (all Nothing [a, b]) == (all (pre 'all of:') [a, b])" do
      alwaysLabeled (All Nothing [aMaybe, bMaybe]) `shouldBe` All (Pre "all of:") [a, b]

    it "alwaysLabeled (any (Just l) [a, b]) == (any l [a, b])" do
      alwaysLabeled (Any (Just preLabel) [aMaybe, bMaybe]) `shouldBe` Any preLabel [a, b]

    it "alwaysLabeled (all (Just l) [a, b]) == (all l [a, b])" do
      alwaysLabeled (All (Just prePostLabel) [aMaybe, bMaybe]) `shouldBe` All prePostLabel [a, b]

  describe "BoolStruct Semigroup" do
    testBatch (semigroup (undefined ::BoolStruct String String, 1::Int))

  describe "siblingfyBoolStruct" do
    let
      x = 1
      foobar = [leaf "foo", leaf "bar"]
      fizbaz = [leaf "fiz", leaf "baz"]

    it "should leave Leaf " do
      siblingfyBoolStruct [leaf "foo", leaf "foo"] `shouldBe` [leaf "foo", leaf "foo"]

    it "should leave Not Leaf " do
      siblingfyBoolStruct [Not $ leaf "foo", Not $ leaf "foo"] `shouldBe` [Not $ leaf "foo", Not $ leaf "foo"]

    it "should merge alls Leaf" do
      siblingfyBoolStruct [allPre "a" foobar, allPre "a" fizbaz, leaf "fig"] `shouldBe` [allPre "a" (foobar ++ fizbaz), leaf "fig"]

    xit "should simplify singleton Leaf" do
      siblingfyBoolStruct [allPre "a" foobar, leaf "fig", allPre "a" fizbaz] `shouldBe` [allPre "a" (foobar ++ fizbaz), leaf "fig"]

    it "should merge alls Leaf" do
      siblingfyBoolStruct [anyPre "a" foobar, anyPre "a" fizbaz, leaf "fig"] `shouldBe` [anyPre "a" (foobar ++ fizbaz), leaf "fig"]

    xit "should simplify singleton Leaf" do
      siblingfyBoolStruct [anyPre "a" foobar, leaf "fig", anyPre "a" fizbaz] `shouldBe` [anyPre "a" (foobar ++ fizbaz), leaf "fig"]

    it "should merge alls Leaf" do
      siblingfyBoolStruct [anyPre "a" foobar, allPre "a" fizbaz, leaf "fig"] `shouldBe` [anyPre "a" foobar, allPre "a" fizbaz, leaf "fig"]

  describe "JSON marshalling" do
    let
      foobar = [leaf "foo", leaf "bar"]
      fizbaz = [leaf "fiz", leaf "baz"]

    it "encode Leaf" do
      encode (leaf "foo") `shouldBe` "{\"contents\":\"foo\",\"tag\":\"Leaf\"}"

    it "encode Not" do
      encode (Not (leaf "foo")) `shouldBe` "{\"contents\":{\"contents\":\"foo\",\"tag\":\"Leaf\"},\"tag\":\"Not\"}"

    it "encode Any (Label Pre" do
      encode (anyPre "any" foobar) `shouldBe` "{\"contents\":[{\"contents\":\"any\",\"tag\":\"Pre\"},[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"Any\"}"

    it "encode Any (Label PrePost" do
      encode (Any (Just (PrePost "PreText" "PostText")) foobar) `shouldBe` "{\"contents\":[{\"contents\":[\"PreText\",\"PostText\"],\"tag\":\"PrePost\"},[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"Any\"}"

    it "encode Any (Label Nothing" do
      encode (any foobar) `shouldBe` "{\"contents\":[null,[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"Any\"}"

    it "encode All (Label Pre" do
      encode (allPre "all" foobar) `shouldBe` "{\"contents\":[{\"contents\":\"all\",\"tag\":\"Pre\"},[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"All\"}"

    it "encode All (Label PrePost" do
      encode (All (Just (PrePost "PreText" "PostText")) foobar) `shouldBe` "{\"contents\":[{\"contents\":[\"PreText\",\"PostText\"],\"tag\":\"PrePost\"},[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"All\"}"

    it "encode All (Label Nothing" do
      encode (all foobar) `shouldBe` "{\"contents\":[null,[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"All\"}"

  describe "JSON unmarshalling" do
    let
      foobar = [leaf "foo", leaf "bar"]
      fizbaz = [leaf "fiz", leaf "baz"]

    it "decode Leaf" do
      decode "{\"contents\":\"foo\",\"tag\":\"Leaf\"}" `shouldBe` Just (leaf "foo")

    it "decode Not" do
      decode "{\"contents\":{\"contents\":\"foo\",\"tag\":\"Leaf\"},\"tag\":\"Not\"}" `shouldBe` Just (Not (leaf "foo"))

    it "decode Any (Label Pre)" $ do
      decode "{\"contents\":[{\"contents\":\"any\",\"tag\":\"Pre\"},[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"Any\"}"
      `shouldBe`
      Just (anyPre "any" foobar)

    it "decode Any (Label PrePost)" $ do
      decode "{\"contents\":[{\"contents\":[\"PreText\",\"PostText\"],\"tag\":\"PrePost\"},[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"Any\"}"
      `shouldBe`
      Just (Any (Just (PrePost "PreText" "PostText")) foobar)

    it "decode Any (Label Nothing)" $ do
      decode "{\"contents\":[null,[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"Any\"}"
      `shouldBe`
      Just (any foobar)

    it "decode Any Nothing - Missing" $ do
      decode "{\"contents\":[[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"Any\"}"
      `shouldBe`
      (Nothing::Maybe WireBoolStruct)

    it "decode All (Label Pre)" $ do
      decode "{\"contents\":[{\"contents\":\"all\",\"tag\":\"Pre\"},[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"All\"}"
      `shouldBe`
      Just (allPre "all" foobar)

    it "decode All (Label PrePost)" $ do
      decode "{\"contents\":[{\"contents\":[\"PreText\",\"PostText\"],\"tag\":\"PrePost\"},[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"All\"}"
      `shouldBe`
      Just (All (Just (PrePost "PreText" "PostText")) foobar)

    it "decode All (Label Nothing)" $ do
      decode "{\"contents\":[null,[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"All\"}"
      `shouldBe`
      Just (all foobar)

    it "decode All Nothing - Missing" $ do
      decode "{\"contents\":[[{\"contents\":\"foo\",\"tag\":\"Leaf\"},{\"contents\":\"bar\",\"tag\":\"Leaf\"}]],\"tag\":\"All\"}"
      `shouldBe`
      (Nothing::Maybe WireBoolStruct)