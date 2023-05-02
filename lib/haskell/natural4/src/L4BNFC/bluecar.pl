qualifies(Car, Day) :- blue(Car), tuesday(Day).

blue(Car) :- blue = Car.colour.

mycar(Car) :- Car = car{ colour: blue
                       , name: "sbs1234a"
                       , engineSize: volume{ value: 1.6 , unit:  litres }
                       }.

tuesday(Day) :- Day = tuesday.

tuesday(tuesday).
