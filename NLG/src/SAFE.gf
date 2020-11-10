abstract SAFE = Action ** {
  flags startcat = Action ;
  fun
    -------------
    -- Actions --
    -------------

    -- Direct object
    Raise,                             -- raise capital
    Issue,                             -- issue stock
    Sell,                              -- sell stock
    Offer,
    Transfer,
    Pledge,
    Hypothecate
      : Action_Dir ;

    -- Indirect object
    -- IssueAt,
    -- SellAt
    --   : Action_Dir_Indir ;

    At,                          -- issue stock [at fixed valuation]
    PursuantTo                   -- raise capital [pursuant to _]
      : Action -> Action_Indir ;

    ----------------
    -- Properties --
    ----------------

    Fixed,
    PreMoney,
    PostMoney,
    BonaFide,
    Voluntary
      : Property ;


    ForBenefit   -- general assignment for the benefit of the Company's creditors
      : Term -> Property ;

    WithPurpose   -- transaction with the purpose of raising capital
      : ActionGerund -> Property ;

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
    Security,
    DiscountRate,
    Valuation : Kind ;



    -- These should be used together with ComplKind : Kind -> Term -> Kind
    -- to get "liquidation of the company" etc.
    -- TODO: see if we need to split this into a new category
    Shares,           -- shares of the Company's preferred stock
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

    Company,
    Investor : Term ;

    RightTo,            -- the right to the shares
    Creditors         -- the Company's creditors
      : Term -> Term ;

    TExcluding, -- liquidation of the Company, excluding a Liquidity Event
    TIncluding  -- fixed valuation, including a pre-money or post-money valuation
      : Determiner ->
      Kind -> Term ->
      Term ;

    --Series,   -- a series of transactions



}
