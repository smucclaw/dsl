abstract Bike = Action ** {
  fun
    Send,
      Deliver,
      Receive
      : Action_Dir ;

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
