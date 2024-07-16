## Questions

```haskell
data SimpleHlike a b c =
  MkSimpleHL { shClauseGiven :: a
             , shClauseReturnVars :: b
             , shBaseClauses :: BaseHL
             , shRuleSrcRef :: SrcRef
             -- ^ may want to parametrize this
             , shRuleLabel :: c
             }
  deriving stock (Eq, Show, Generic)
```

What is `shClauseReturnVars`? The Giveth of the rule?
