abstract Bike = Action ** {
  fun
    Send,
      Deliver,
      Receive
      : Action_Dir ;

    From
      : Action -> Action_Indir ;

    Order,
      Delivery,
      Payment,
      Buyer,
      Seller,
      Bike,
      Item,
      Inventory
      : Kind ;
}
