abstract Bike = Action ** {
  fun
    Send,
      Deliver,
      Receive
      : Action_Dir ;

    Order,
      Delivery,
      Payment,
      Bike,
      Item,
      Inventory
      : Kind ;

    Eur : Int -> Term ;
}
