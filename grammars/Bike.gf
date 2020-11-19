abstract Bike = Action ** {
  fun
    Send,
      Deliver,
      Receive,
      Return,
      RefundV2
      : Action_Dir ;

    Order,
      Delivery,
      Payment,
      Sale,
      Bike,
      Item,
      Amount,
      Inventory
      : Kind ;

    Eur : Int -> Term ;
}
