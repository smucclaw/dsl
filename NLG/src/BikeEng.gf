concrete BikeEng of Bike = ActionEng ** open
  Prelude,
  ParadigmsEng,
  SyntaxEng,
  (WN=WordNetEng) in {
  lin
    -- : Action_Dir ;
    Send = mkDir send_V2 ;
    Deliver = mkDir deliver_V2 ;
    Receive = mkDir receive_V2 ;

    -- : Action -> Action_Indir ;
    From a =  a ** {
      intrans = a.s ;
      indir = whether_from_Prep ;
      dir = \\_ => emptyAdv
      } ;

    -- : Kind ;
    Order = kind "order" ;
    Delivery = kind "delivery" ;
    Payment = kind "payment" ;
    Buyer = kind "buyer" ;
    Seller = kind "seller" ;
    Bike = kind "bike" ;
    Item = kind "item" ;
    Inventory = kind "inventory" ;


  oper
    whether_from_Prep : PrepPol =
     prepPol
      {s = "from" ; post = [] ; redupl = False}
      {s = ", whether from" ; post = "or from any source" ; redupl = False} ;


    send_V2 : V2 = <WN.send_V3 : V2> ;
    deliver_V2 : V2 = mkV2 WN.deliver_2_V ;
    receive_V2 : V2 = WN.receive_1_V2 ;

}
