class City {
  Name : String;
  isTaxhaven : Boolean;
}

class Person {
  Name : String;
}

class Corporation {
  Owner : Person;
  hasHQ : City;
  profitable : Boolean;
}

// mustPayCorpTax scope
const mustPayCorpTax_p_c  = value : "mustPayCorpTax_p_c"

const GLOBALS = [c, mustPayCorpTax_p_c, p];
