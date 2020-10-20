abstract SAFE = Action ** {
  flags startcat = Action ;
  fun
    -------------
    -- Actions --
    -------------

    -- Direct object
    Raise,                             -- raise capital
    Issue,                             -- issue stock
    Sell                               -- sell stock
      : Action_Dir ;

    -- Indirect object
    IssueAt,                           -- issue stock at fixed valuation
    SellAt
      : Action_Dir_Indir ;

    PursuantTo                         -- add indirect object
      : Action -> Action_Indir ;

    ----------------
    -- Properties --
    ----------------

    Fixed,
    PreMoney,
    PostMoney,
    BonaFide,
    Voluntary : Property ;


    ForBenefit   -- general assignment for the benefit of the Company's creditors
      : Term -> Property ;

    WithPurpose   -- transaction with the purpose of raising capital
      : Action -> Property ;

    -----------
    -- Kinds --
    -----------
    Event,
    Capital,

    DissolutionEvent,
    Termination,
    GeneralAssignment,

    LiquidityEvent,
    ChangeOfControl,
    DirectListing,
    InitialPublicOffering,

    EquityFinancing,
    Transaction,
    PreferredStock,
    ConversionPrice,
    PurchaseAmount,
    Valuation : Kind ;

    -- These should be used together with ComplKind : Kind -> Term -> Kind
    -- to get "liquidation of the company" etc.
    -- TODO: see if we need to split this into a new category
    Liquidation,
    Dissolution,
    WindingUp
      : Kind ;

    KWhetherOr  -- dissolution event, whether voluntary or involuntary
      : [Property] -> Kind -> Kind ;

    SingleOrSeries : Kind -> Kind ;

    -----------
    -- Terms --
    -----------

    Company : Term ;

    Creditors : Term -> Term ; -- the Company's creditors

    TExcluding, -- liquidation of the Company, excluding a Liquidity Event
    TIncluding  -- fixed valuation, including a pre-money or post-money valuation
      : Determiner ->
      Kind -> Term ->
      Term ;

    --Series,   -- a series of transactions



}
