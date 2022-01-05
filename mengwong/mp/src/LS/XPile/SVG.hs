module LS.XPile.SVG where

-- just draw SVGs


-- initially let's just draw the state diagram in a manner typical of GraphViz:


{-

subgraph states {
  initial;
  is_aware;
  assess_hence;
  assess_lest;
  pdpc_informed;
  users_informed;
  users_ignorant;
  fulfilled;
  breach;
}

subgraph transitions {
  aware     [ label = "upon becoming aware a data breach may have occurred" ];
  assess    [ label = "assess if it is a Notifiable Data Breach\nwithin 30 days" ];
  notifiable_yes;
  notifiable_no;
  users_notifiable_yes;
  users_notifiable_no;
}

initial -> aware -> is_aware -> assess -> assess_hence;
                                assess -> assess_lest;

assess_hence -> notifiable_yes;
assess_hence -> notifiable_no -> fulfilled;

have to notify PDPC but not end user if we have taken technological measures

notifiable_yes -> pdpc_informed -> users_notifiable_yes -> users_informed -> fulfilled;
                                   users_notifiable_yes -> users_ignorant -> breach;
                  pdpc_informed -> users_notifiable_no                    -> fulfilled;



-}
