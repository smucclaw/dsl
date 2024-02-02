{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module LS.TokenTable
  ( MyToken (..),
    tokenTable
  )
where

import Data.Aeson (ToJSON)
import Data.HashMap.Strict qualified as Map
import Data.Hashable (Hashable)
import Data.Text qualified as Text
import Flow ((|>))
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)

data MyToken = Every | Party | TokAll
            | Who | Which | Whose
            | Must | May | Shant
            | If | When | Always | Never
            | Or | And | MPNot
            | Before | After | By | On | Eventually -- TVague is a temporal constraint but not a token
            | Means | Includes  | Is
            | Given | Giveth | Having | Upon
            | Declare | Define | OneOf | Holds
            | Decide
            | A_An
            | Deem | As | Has
            -- | AsOf -- used to evaluate a term not under the live context but at some previous time
            | TypeSeparator -- ::, TYPE, AS, shrug
            | One | Optional | List0 | List1 -- list-like modifiers, List1=NonEmpty
            |                   Set0 | Set1  -- set; set.nonempty
            | Distinct -- entity modifier in GIVEN
            | Unless
            | Hence | Lest | Fulfilled | Breach | Goto
            | Then | Else
            | TNumber Double
            | Other Text.Text
            | Do | FMap
            | TokTrue | TokFalse
            | Aka -- also known as, for AKA Receiving Party
            | Typically -- to provide default values
            | Empty | EOL
            | RuleMarker Int Text.Text
            | Expect | ScenarioTok
            | TokLT | TokLTE | TokGT | TokGTE | TokIn | TokNotIn | TokEQ | TokAnd | TokOr | TokSum | TokProduct | TokMin | TokMax
            | Notwithstanding | Despite | SubjectTo
            | Otherwise
            | SOF | EOF
            | GoDeeper | UnDeeper
            | SetPlus | SetLess -- set union and subtraction
            | Where -- like in Haskell
            | Semicolon -- rule separator
  deriving (Ord, Eq, Show, Generic, Hashable, Lift, ToJSON)

tokenTable :: Map.HashMap Text.Text [MyToken]
tokenTable =
  [ -- start a regulative rule
    (["EVERY"], [Every]),
    (["PARTY"], [Party]),
    (["ALL"], [TokAll]), -- when parties are treated as a collective, e.g. ALL diners. TokAll means "Token All"

    -- start a boolstruct
    (["ALWAYS"], [Always]),
    (["NEVER"], [Never]),

    -- qualify a subject
    (["WHO"], [Who]),
    (["WHICH"], [Which]),
    (["WHOSE"], [Whose]),

    (["WHEN"], [When]),
    (["IF"], [If]),
    (["UPON"], [Upon]),
    (["GIVEN"], [Given]),
    (["GIVETH"], [Giveth]),
    (["HAVING"], [Having]),

    (["MEANS"], [Means]),
    (["INCLUDES"], [Includes]),
    (["IS"], [Is]),

    -- boolean connectors
    (["OR"], [Or]),
    (["AND", "...", "…"], [And]), -- Elipses are CNL sugar to allow phrases to follow
    (["UNLESS", "EXCEPT", "IF NOT"], [Unless]),
    (["NOT"], [MPNot]),

    -- set operators
    (["PLUS"], [SetPlus]),
    (["LESS"], [SetLess]),

    -- deontics
    (["MUST"], [Must]),
    (["MAY"], [May]),
    (["SHANT"], [Shant]),

      -- temporals
    (["UNTIL", "BEFORE", "WITHIN"], [Before]), -- <
    (["AFTER"], [After]), -- >
    (["BY"], [By]),
    (["ON", "AT"], [On]), -- ==
    (["EVENTUALLY"], [Eventually]),

    -- the rest of the regulative rule
    (["➔", "->", "DO", "PEFORM"], [Do]),

    -- for discarding
    ([""], [Empty]),
    (["TRUE"], [TokTrue]),
    (["FALSE"], [TokFalse]),
    (["HOLDS"], [Holds]),

      -- regulative chains
    (["HENCE", "THUS"], [Hence]),

    -- alternative formulations intended to be closer to natural language
    -- for the obligation case
    (["IF FULFILLED"], [Hence]),
    (["IF NOT FULFILLED", "IF VIOLATED"], [Lest]),

    -- for the permission case
    (["IF EXERCISED", "IF EXERCIZED"], [Hence]),
    (["IF NOT EXERCISED", "IF NOT EXERCIZED"], [Lest]),

    -- for the prohibition case
    (["IF PROHIBITION VIOLATED"], [Lest]),
    ( [ "IF PROHIBITION NOT VIOLATED",
        "IF NOT PROHIBITION VIOLATED",
        "IF NOT VIOLATED"
      ],
      [Hence]
    ),

    -- mutable state variables are modified by UPON THEN ELSE
    (["THEN"], [Then]),
    (["ELSE", "OR ELSE", "XOR ELSE", "XELSE"], [Else]),

    -- trivial contracts
    (["FULFILLED"], [Fulfilled]),
    (["BREACH"], [Breach]),

    (["LEST"], [Lest]),
    (["GOTO"], [Goto]),

    ([";"], [EOL]),

    ([":", "::", "TYPE", "IS A", "IS AN", "IS THE"], [TypeSeparator, A_An]),

    -- [TODO] this is going to break entirely innocent end-user phrasing like 7 8 9 A B C D E
    (["A", "AN", "THE"], [A_An]),

    (["DECLARE"], [Declare]),
    -- [TODO] rephrase DEFINE to support DECIDE and possibly overloaded DATA?
    (["DEFINE", "DATA"], [Define]),
    (["DECIDE"], [Decide]),
    (["ONEOF", "ONE OF", "IS ONE OF", "AS ONE OF"], pure OneOf),
    (["DEEM"], [Deem]),
    (["HAS"], [Has]),

    (["ONE"], [One]),
    (["OPTIONAL"], [Optional]),

    (["LIST0"], [List0]),
    (["LIST", "LIST1", "LISTOF", "LIST OF"], [List1]),

    (["SET0"], [Set0]),
    (["SET", "SET1", "SETOF", "SET OF"], [Set1]),

    (["MAP"], [FMap]),

    (["AKA"], [Aka]),
    (["TYPICALLY"], [Typically]),

    (["CLAUSE", "SECTION"], [RuleMarker 1 "§"]),

    (["SCENARIO"], [ScenarioTok]),
    (["EXPECT"], [Expect]),

    (["<"], [TokLT]),
    (["MIN", "MIN OF"], [TokMin]),
    (["=<", "<="], [TokLTE]),
    ([">"], [TokGT]),
    (["MAX", "MAX OF"], [TokMax]),
    ([">="], [TokGTE]),
    (["&&"], [TokAnd]),
    (["||"], [TokOr]),
    (["SUM", "SUM OF"], [TokSum]),
    (["PRODUCT", "PRODUCT OF"], [TokProduct]),
    (["==", "==="], [TokEQ]),

    (["IN"], [TokIn]),
    (["NOT IN"], [TokNotIn]),

    -- rule priority interactions and "defeasibility"
    (["SUBJECT TO"], [SubjectTo]),
    (["DESPITE"], [Despite]),
    (["NOTWITHSTANDING"], [Notwithstanding]),

    (["OTHERWISE"], [Otherwise]),
    (["WHERE"], [Where]),
    ([";;"], [Semicolon])
  ]
    |> foldMap \(keys, tokens) -> [(key, tokens) | key <- keys]
    |> Map.fromList