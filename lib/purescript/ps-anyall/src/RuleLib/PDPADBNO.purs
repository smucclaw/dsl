module RuleLib.PDPADBNO where

import Prelude
import Data.Tuple
import Data.Map as Map
import Data.Maybe
import AnyAll.Types

-- in the future the front-end will break out the source content into these structures

ndb :: Item String
ndb =
  All (Pre "all of the following")
  [ Leaf "NOT onlyWithinOrg"
  , Any (Pre "significant harm")
    [ All (Pre "3.1.a")
      [ Any (Pre "identifier relates to") [ Leaf "full name"
                                          , Leaf "alias"
                                          , Leaf "personal identification number" ]
      , Leaf "GOTO schedule1_part1" ]
    , All (Pre "3.1.b")
      [ Leaf "organisational account identifier"
      , Any (Pre "") [ Leaf "password"
                     , Leaf "security code"
                     , Leaf "access code"
                     , Leaf "response to a security question"
                     , Leaf "biometric data"
                     , All (PrePost "other data" "the user's account") [ Any (Pre "that is") [ Leaf "used", Leaf "required" ]
                                                                       , Any (Pre "to allow") [ Leaf "access to", Leaf "use of" ] ]
                     ]
      ]
    ]
    , Any (Pre "significant scale")
      [ Any (Pre "affects") [ Leaf "not fewer than 500 individuals" ]
        ]
      ]

ndb_nl :: NLDict
ndb_nl =
  Map.fromFoldable
  [ Tuple "en" $ Map.fromFoldable
    [ Tuple "NOT onlyWithinorg" "it relates to the unauthorised access, collection, use, disclosure, copying or modification of personal data only within an organisation"
    , Tuple "significant harm"  "it results in, or is likely to result in, significant harm to an affected individual"
    , Tuple "GOTO schedule_part1" "any of the personal data or classes of personal data relating to the individual set out in Part 1 of the Schedule, subject to Part 2 of the Schedule"
    , Tuple "3.1.b" "all of the following personal data relating to an individual’s account with an organisation"
    , Tuple "organisational account identifier" "the individual's account identifier such as an account name or number (and this includes a number assigned to any account the individual has with an organisation that is a bank or finance company"
    , Tuple "significant scale" "it is, or is likely to be, of a significant scale"
    ]
  ]

{- 
it results in, or is likely to result in, significant harm to an affected individual																				
IF	FALSE	the data breach is in relation to any prescribed personal data or class of personal data relating to the individual																		
OR	FALSE	in other prescribed circumstances.																		
																				
IF	FALSE	the data breach relates to																		
			FALSE			the individual’s	FALSE	full name												
						,	FALSE	alias												
						OR	FALSE	identification number												
		AND	FALSE	any of the personal data or classes of personal data relating to the individual																
				set out in	Part 1 of the Schedule															
				subject to	Part 2 of the Schedule															
																				
OR	TRUE	all of	the following personal data relating to an individual’s account with an organisation:																	
		TRUE	the individual’s account identifier						such as an account name or number;											
									includes a number assigned to any account the individual has with an organisation that is a bank or finance company.											
		TRUE	any	FALSE	password					that is	TRUE	used		to	allow	FALSE	access to		the individual’s account.	
				FALSE	security code					or	FALSE	required			or	TRUE	use of			
				FALSE	access code															
				TRUE	response to a security question															
				FALSE	biometric data															
			or	FALSE	other data															
																				
it is, or is likely to be, of a significant scale																				
IF	FALSE	if the data breach affects not fewer than							the prescribed number of affected individuals											
OR	FALSE	in other prescribed circumstances.							IS	500	(Regs §4)

-}

schedule1_part1 :: Item String
schedule1_part1 = 
  Any (Pre "any of the following are prescribed personal data:")
       [ Leaf "1"
       , Leaf "2"
       , Leaf "3"
       , Leaf "4"
       , Any (Pre "5") [ Leaf "5.a"
                       , Leaf "5.b"
                       , Leaf "5.c"
                       , Leaf "5.d"
                       , Leaf "5.e"
                       , Leaf "5.f"
                       , Leaf "5.g"
                       , Leaf "5.h"
                       ]
       , Any (Pre "6") [ Leaf "6.a"
                       , Leaf "6.b"
                       , Leaf "6.c"
                       , Leaf "6.d"
                       , Leaf "6.e"
                       ]
       , Any (Pre "7") [ Leaf "7.a"
                       , Leaf "7.b"
                       , Leaf "7.c"
                       ]
       , Leaf "8"
       , Leaf "9"
       , Leaf "10"
       , Leaf "11"
       , Leaf "12"
       , Leaf "13"
       , Leaf "14"
       , Leaf "15"
       , Any (Pre "16") [ Leaf "16.a"
                        , Leaf "16.b"
                        ]
       , Any (Pre "17") [ Leaf "17.a"
                        , Leaf "17.b"
                        , Leaf "17.c"
                        , Leaf "17.d"
                        , Leaf "17.e"
                        ]
       , Any (Pre "18") [ Leaf "18.a"
                        , Leaf "18.b"
                        , Leaf "18.c"
                        , Leaf "18.d"
                        ]
       , Any (Pre "19") [ Leaf "19.a"
                        , Leaf "19.b"
                        ]
       , Any (Pre "20") [ Leaf "20.a"
                        , Leaf "20.b"
                        , Leaf "20.c"
                        ]
       , Leaf "21"
       , Leaf "22"
       , Any (Pre "23") [ Leaf "23.a"
                        , Leaf "23.b"
                        , Leaf "23.c"
                        , Leaf "23.d"
                        , Leaf "23.e"
                        , Leaf "23.f"
                        ]
       ]

schedule1_part1_nl :: NLDict
schedule1_part1_nl =
  Map.fromFoldable
  [ Tuple "en" $ Map.fromFoldable
    [ Tuple "1" "The amount of any wages, salary, fee, commission, bonus, gratuity, allowance or other remuneration paid or payable to the individual by any person, whether under a contract of service or a contract for services."
    , Tuple "2" "The income of the individual from the sale of any goods or property."
    , Tuple "3" "The number of any credit card, charge card or debit card issued to or in the name of the individual."
    , Tuple "4" "The number assigned to any account the individual has with any organisation that is a bank or finance company."
    , Tuple "5" "Any information that identifies, or is likely to lead to the identification of, the individual as a child or young person who —"
    , Tuple "5.a" "is or had been the subject of any investigation under the CYPA;"
    , Tuple "5.b" "is or had been arrested, on or after 1 July 2020, for an offence committed under any written law;"
    , Tuple "5.c" "is or had been taken into care or custody by the Director-General of Social Welfare, a protector, any officer generally or specially authorised in that behalf in writing by the Director-General or protector or a police officer under the CYPA;"
    , Tuple "5.d" "is attending or had attended a family programme in relation to an application to be made under section 50 of the CYPA;"
    , Tuple "5.e" "is or was the subject of an order made by a court under the CYPA; or"
    , Tuple "5.f" "is or had been concerned in any proceedings in any court or on appeal from any court, whether the individual is the person against or in respect of whom the proceedings are taken or a witness in those proceedings."
    , Tuple "6" "Any information that identifies, or is likely to lead to the identification of —"
    , Tuple "6.a" "the individual who has been or is the subject of any investigation, examination, assessment or treatment under the VAA relating to whether the individual is a vulnerable adult experiencing or at risk of abuse, neglect or self-neglect;"
    , Tuple "6.b" "the individual as a vulnerable adult who has been committed to a place of temporary care and protection or place of safety or to the care of a fit person under the VAA;"
    , Tuple "6.c" "the individual as a vulnerable adult who is the subject of an order made by a court under the VAA;"
    , Tuple "6.d" "a place of temporary care and protection or place of safety in which an individual or a vulnerable adult mentioned in sub-paragraph (a), (b) or (c) is committed, or the location of such a place of temporary care and protection or place of safety; or"
    , Tuple "6.e" "a fit person under whose care an individual or a vulnerable adult mentioned in sub-paragraph (a), (b) or (c) is placed, or the location of the premises of such a fit person."
    , Tuple "7" "Any private key of or relating to the individual that is used or may be used —"
    , Tuple "7.a" "to create a secure electronic record or secure electronic signature;"
    , Tuple "7.b" "to verify the integrity of a secure electronic record; or"
    , Tuple "7.c" "to verify the authenticity or integrity of a secure electronic signature."
    , Tuple "8" "The net worth of the individual."
    , Tuple "9" "The deposit of moneys by the individual with any organisation."
    , Tuple "10" "The withdrawal by the individual of moneys deposited with any organisation."
    , Tuple "11" "The granting by an organisation of advances, loans and other facilities by which the individual, being a customer of the organisation, has access to funds or financial guarantees."
    , Tuple "12" "The incurring by the organisation of any liabilities other than those mentioned in paragraph 11 on behalf of the individual."
    , Tuple "13" "The payment of any moneys, or transfer of any property, by any person to the individual, including the amount of the moneys paid or the value of the property transferred, as the case may be."
    , Tuple "14" "The creditworthiness of the individual."
    , Tuple "15" "The individual’s investment in any capital markets products."
    , Tuple "16" "The existence, and amount due or outstanding, of any debt —"
    , Tuple "16.a" "owed by the individual to an organisation; or"
    , Tuple "16.b" "owed by an organisation to the individual."
    , Tuple "17" "Any of the following:"
    , Tuple "17.a" "the terms and conditions of any accident and health policy or life policy (called in this item the applicable policy) of which the individual is the policy owner or under which the individual is a beneficiary;"
    , Tuple "17.b" "the premium payable by the policy owner under the applicable policy;"
    , Tuple "17.c" "the benefits payable to any beneficiary under the applicable policy;"
    , Tuple "17.d" "any information relating to any claim on, or payment under, the applicable policy, including the condition of the health of the individual and the diagnosis, treatment, prevention or alleviation of any ailment, condition, disability, disease, disorder or injury that the individual has suffered or is suffering from;"
    , Tuple "17.e" "any other information that the individual is the policy owner of, or a beneficiary under, an applicable policy."
    , Tuple "18" "The assessment, diagnosis, treatment, prevention or alleviation by a health professional of any of the following affecting the individual:"
    , Tuple "18.a" "any sexually-transmitted disease such as Chlamydial Genital Infection, Gonorrhoea and Syphilis;"
    , Tuple "18.b" "Human Immunodeficiency Virus Infection;"
    , Tuple "18.c" "schizophrenia or delusional disorder;"
    , Tuple "18.d" "substance abuse and addiction, including drug addiction and alcoholism."
    , Tuple "19" "The provision of treatment to the individual for or in respect of —"
    , Tuple "19.a" "the donation or receipt of a human egg or human sperm; or"
    , Tuple "19.b" "any contraceptive operation or procedure or abortion."
    , Tuple "20" "Any of the following:"
    , Tuple "20.a" "subject to section 4(4)(b) of the Act, the donation and removal of any organ from the body of the deceased individual for the purpose of its transplantation into the body of another individual;"
    , Tuple "20.b" "the donation and removal of any specified organ from the individual, being a living organ donor, for the purpose of its transplantation into the body of another individual;"
    , Tuple "20.c" "the transplantation of any organ mentioned in sub-paragraph (a) or (b) into the body of the individual."
    , Tuple "21" "Subject to section 4(4)(b) of the Act, the suicide or attempted suicide of the individual."
    , Tuple "22" "Domestic abuse, child abuse or sexual abuse involving or alleged to involve the individual."
    , Tuple "23" "Any of the following:"
    , Tuple "23.a" "information that the individual is or had been adopted pursuant to an adoption order made under the Adoption of Children Act (Cap. 4), or is or had been the subject of an application for an adoption order;"
    , Tuple "23.b" "the identity of the natural father or mother of the individual;"
    , Tuple "23.c" "the identity of the adoptive father or mother of the individual;"
    , Tuple "23.d" "the identity of any applicant for an adoption order;"
    , Tuple "23.e" "the identity of any person whose consent is necessary under that Act for an adoption order to be made, whether or not the court has dispensed with the consent of that person in accordance with that Act;"
    , Tuple "23.f" "any other information that the individual is or had been an adopted child or relating to the adoption of the individual."
    ]
  ]

