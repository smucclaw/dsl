class Party {
  Address : String;
  Name : String;
}

class Natural_Person extends Party {
  Email : String;
}

class Corporation extends Party {
  representative : Natural_Person;
}

class Money {
  Amount : Number;
  Currency : String;
}

class US_Company extends Corporation {
  
}

// GLOBAL scope
const Amount : Money = {
  Currency: "USD",
  Amount: "100000",
  }


const Company : US_Company = {
  Name: "Unicorn Startup Pte Ltd",
  Address: "100 Ideas Way\nSmart Zone",
  representative: "Phineas Ounder",
  }


const Investor : Natural_Person = {
  Name: "Scrooge McDuck",
  Address: "1 Moneybags Lane\nRich Town",
  Email: "scrooge@example.com",
  }


const Phineas_Ounder : Natural_Person = {
  Email: "founder@example.com",
  }


const GLOBALS = [Amount, Company, Investor, Phineas_Ounder];

