concrete InsLexEng of InsLex = NL4BaseEng **
  open ParadigmsEng, Prelude, GrammarEng in {
    oper mkSubj : Str -> Subj = \s -> lin Subj (ss s) ;
    lin _ignore_consider_V2 = mkV2 (mkV "ignore") (mkPrep "consider") ;
    lin _Number_PN = mkPN "Number"  ;
    lin _note_V2 = mkV2 (mkV "note")  ;
    lin _thing_N = mkN "thing"  ;
    lin _convert_to_V2 = mkV2 (mkV "convert") (mkPrep "to") ;
    lin _A_PN = mkPN "A"  ;
    lin _detail_N = mkN "detail"  ;
    lin _earn_V2 = mkV2 (mkV "earn")  ;
    lin _appearance_N = mkN "appearance"  ;
    lin _concrete_A = mkA "concrete"  ;
    lin _Pru_PN = mkPN "Pru"  ;
    lin _administrative_A = mkA "administrative"  ;
    lin _natural4_N = mkN "natural4"  ;
    lin _claim_from_V2 = mkV2 (mkV "claim") (mkPrep "from") ;
    lin _damage_N = mkN "damage"  ;
    lin _base_on_V2 = mkV2 (mkV "base") (mkPrep "on") ;
    lin _overall_A = mkA "overall"  ;
    lin _encoding_N = mkN "encoding"  ;
    lin _OverallPolicySum_PN = mkPN "OverallPolicySum"  ;
    lin _database_N = mkN "database"  ;
    lin _Amount_N = mkN "Amount"  ;
    lin _reinstatement_N = mkN "reinstatement"  ;
    lin _A_N = mkN "A"  ;
    lin _like_Prep = mkPrep "like"  ;
    lin _PruAssure_PN = mkPN "PruAssure"  ;
    lin _convert_V2 = mkV2 (mkV "convert")  ;
    lin _long_Adv = mkAdv "long"  ;
    lin _remark_N = mkN "remark"  ;
    lin _involve_VS = mkVS (mkV "involve")  ;
    lin _represent_V2 = mkV2 (mkV "represent")  ;
    lin _Damage_N = mkN "Damage"  ;
    lin _relate_to_V2 = mkV2 (mkV "relate") (mkPrep "to") ;
    lin _Pru_N = mkN "Pru"  ;
    lin _cause_V = mkV "cause"  ;
    lin _subsection_N = mkN "subsection"  ;
    lin _text_N = mkN "text"  ;
    lin _cell_N = mkN "cell"  ;
    lin _Damage_PN = mkPN "Damage"  ;
    lin _PruAssure_N = mkN "PruAssure"  ;
    lin _end_V = mkV "end"  ;
    lin _end_accord_V2 = mkV2 (mkV "end") (mkPrep "accord") ;
    lin _argument_N = mkN "argument"  ;
    lin _use_VV = mkVV (mkV "use")  ;
    lin _describe_in_V2 = mkV2 (mkV "describe") (mkPrep "in") ;
    lin _about_Prep = mkPrep "about"  ;
    lin _arise_V2 = mkV2 (mkV "arise")  ;
    lin _qualify_for_V2 = mkV2 (mkV "qualify") (mkPrep "for") ;
    lin _directly_Adv = mkAdv "directly"  ;
    lin _OverallPolicySum_N = mkN "OverallPolicySum"  ;
    lin _Number_N = mkN "Number"  ;
    lin _update_N = mkN "update"  ;
    lin _indirectly_Adv = mkAdv "indirectly"  ;
    lin _ph_PN = mkPN "ph"  ;
    lin _natural_A = mkA "natural"  ;
    lin _separate_A = mkA "separate"  ;
    lin _subsequent_A = mkA "subsequent"  ;
    lin _structure_N = mkN "structure"  ;
    lin _accord_to_V2 = mkV2 (mkV "accord") (mkPrep "to") ;
    lin _atomicity_N = mkN "atomicity"  ;
    lin _Amount_PN = mkPN "Amount"  ;
    lin _income_N = mkN "income"  ;
    lin _reward_N = mkN "reward"  ;
    lin _more_Adv = mkAdv "more"  ;
    lin _datatype_N = mkN "datatype"  ;
    lin _consider_V2 = mkV2 (mkV "consider")  ;
    lin _reach_V2 = mkV2 (mkV "reach")  ;
    lin _insurer_N = mkN "insurer"  ;
    lin _whether_Subj = mkSubj "whether"  ;
    lin _handle_V2 = mkV2 (mkV "handle")  ;
    lin _renew_V2 = mkV2 (mkV "renew")  ;
    lin _ph_N = mkN "ph"  ;
    lin _no_Adv = mkAdv "no"  ;
    lin _live_A = mkA "//,,,live"  ;
    lin _capacity_N = mkN "capacity"  ;
    lin _cover_N = mkN "cover"  ;
    lin _start_V2 = mkV2 (mkV "start")  ;
    lin _so_Adv = mkAdv "so"  ;
    lin _define_based_on_V2 = mkV2 (mkV "define") (mkPrep "based on") ;
    lin _exclude_from_V2 = mkV2 (mkV "exclude") (mkPrep "from") ;
    lin _similar_A = mkA "similar"  ;
    lin _here_Adv = mkAdv "here"  ;
    lin _demarcate_V2 = mkV2 (mkV "demarcate")  ;
    lin _underlie_V = mkV "underlie"  ;
    lin _complex_A = mkA "complex"  ;
    lin _clause_N = mkN "clause"  ;
    lin _practitioner_N = mkN "practitioner"  ;
    lin _insanity_N = mkN "insanity"  ;
    lin _plan_N = mkN "plan"  ;
    lin _amount_N = mkN "amount"  ;
    lin _SA_PN = mkPN "SA"  ;
    lin _cover_as_V2 = mkV2 (us_britishV "cover") (mkPrep "as") ;
    lin _govern_V = us_britishV "govern"  ;
    lin _auditorium_N = mkN "auditorium"  ;
    lin _Health_PN = mkPN "Health"  ;
    lin _nuclear_A = mkA "nuclear"  ;
    lin _question_N = mkN "question"  ;
    lin _permanently_Adv = mkAdv "permanently"  ;
    lin _Nipah_PN = mkPN "Nipah"  ;
    lin _hospital_N = mkN "hospital"  ;
    lin _take_in_V2 = mkV2 (us_britishV "take") (mkPrep "in") ;
    lin _asssure_V = us_britishV "asssure"  ;
    lin _Schedule_PN = mkPN "Schedule"  ;
    lin _disability_N = mkN "disability"  ;
    lin _addSA_PN = mkPN "addSA"  ;
    lin _cover_in_V2 = mkV2 (us_britishV "cover") (mkPrep "in") ;
    lin _juvenile_A = mkA "juvenile"  ;
    lin _state_N = mkN "state"  ;
    lin _tail_N = mkN "tail"  ;
    lin _home_N = mkN "home"  ;
    lin _speech_N = mkN "speech"  ;
    lin _Disease_PN = mkPN "Disease"  ;
    lin _event_N = mkN "event"  ;
    lin _box_in_V2 = mkV2 (us_britishV "box") (mkPrep "in") ;
    lin _or_Subj = mkSubj "or"  ;
    lin _martial_A = mkA "martial"  ;
    lin _b_N = mkN "b"  ;
    lin _LE_PN = mkPN "LE"  ;
    lin _Type_PN = mkPN "Type"  ;
    lin _medical_A = mkA "medical"  ;
    lin _suffer_V = us_britishV "suffer"  ;
    lin _benefit_V = us_britishV "benefit"  ;
    lin _thumb_N = mkN "thumb"  ;
    lin _that_Subj = mkSubj "that"  ;
    lin _pay_as_V2 = mkV2 (us_britishV "pay") (mkPrep "as") ;
    lin _active_A = mkA "active"  ;
    lin _into_Prep = mkPrep "into"  ;
    lin _Injury_PN = mkPN "Injury"  ;
    lin _behaviour_N = mkN "behaviour"  ;
    lin _planAF_PN = mkPN "planAF"  ;
    lin _illness_N = mkN "illness"  ;
    lin _at_Prep = mkPrep "at"  ;
    lin _unsound_A = mkA "unsound"  ;
    lin _can_claim_add_N = mkN "can_claim_add"  ;
    lin _stepupsumassure_V = us_britishV "stepupsumassure"  ;
    lin _possible_A = mkA "possible"  ;
    lin _training_N = mkN "training"  ;
    lin _step_N = mkN "step"  ;
    lin _benRA_PN = mkPN "benRA"  ;
    lin _body_N = mkN "body"  ;
    lin _tooth_N = mkN "tooth"  ;
    lin _up_Adv = mkAdv "up"  ;
    lin _strain_N = mkN "strain"  ;
    lin _war_N = mkN "war"  ;
    lin _public_A = mkA "public"  ;
    lin _totally_Adv = mkAdv "totally"  ;
    lin _reckless_A = mkA "reckless"  ;
    lin _reduction_N = mkN "reduction"  ;
    lin _removal_N = mkN "removal"  ;
    lin _sport_N = mkN "sport"  ;
    lin _weapon_N = mkN "weapon"  ;
    lin _dangerous_weapon_N = mkN "nuclear, biological or chemical weapon" ;
    lin _middle_A = mkA "middle"  ;
    lin _MAP_PN = mkPN "MAP"  ;
    lin _UPON_PN = mkPN "UPON"  ;
    lin _act_N = mkN "act"  ;
    lin _outcome_N = mkN "outcome"  ;
    lin _pregnancy_N = mkN "pregnancy"  ;
    lin _hand_V = us_britishV "hand"  ;
    lin _across_Prep = mkPrep "across"  ;
    lin _subscribe_V = us_britishV "subscribe"  ;
    lin _acc_injury_or_death_happens_within_12_mos_of_accident_N = mkN "acc_injury_or_death_happens_within_12_mos_of_accident"  ;
    lin _great_A = mkA "great"  ;
    lin _date_N = mkN "date"  ;
    lin _climbing_N = mkN "climbing"  ;
    lin _jaw_N = mkN "jaw"  ;
    lin _bus_N = mkN "bus"  ;
    lin _add_A = mkA "add"  ;
    lin _PolicyHolder_PN = mkPN "PolicyHolder"  ;
    lin _Teeth_PN = mkPN "Teeth"  ;
    lin _permanent_A = mkA "permanent"  ;
    lin _degree_N = mkN "degree"  ;
    lin _benmr_N = mkN "benmr"  ;
    lin _finger_N = mkN "finger"  ;
    lin _HAS_PN = mkPN "HAS"  ;
    lin _more_A = mkA "more"  ;
    lin _n_N = mkN "n"  ;
    lin _biological_A = mkA "biological"  ;
    lin _travel_as_V2 = mkV2 (us_britishV "travel") (mkPrep "as") ;
    lin _happen_within_V2 = mkV2 (us_britishV "happen") (mkPrep "within") ;
    lin _sumassure_V = us_britishV "sumassure"  ;
    lin _race_V = us_britishV "race"  ;
    lin _if_Subj = mkSubj "if"  ;
    lin _radioactivity_N = mkN "radioactivity"  ;
    lin _at_Adv = mkAdv "at"  ;
    lin _first_Adv = mkAdv "first"  ;
    lin _assure_V = us_britishV "assure"  ;
    lin _unnecessary_A = mkA "unnecessary"  ;
    lin _plan1_N = mkN "plan1"  ;
    lin _minimum_N = mkN "minimum"  ;
    lin _planC_PN = mkPN "planC"  ;
    lin _bicycle_N = mkN "bicycle"  ;
    lin _day_N = mkN "day"  ;
    lin _drug_N = mkN "drug"  ;
    lin _Removal_PN = mkPN "Removal"  ;
    lin _majority_N = mkN "majority"  ;
    lin _geographical_A = mkA "geographical"  ;
    lin _confinement_N = mkN "confinement"  ;
    lin _take_V2 = mkV2 (us_britishV "take")  ;
    lin _PS_PN = mkPN "PS"  ;
    lin _Accidents_PN = mkPN "Accidents"  ;
    lin _and_Subj = mkSubj "and"  ;
    lin _suffer_V2 = mkV2 (us_britishV "suffer")  ;
    lin _sum_N = mkN "sum"  ;
    lin _claim_V = us_britishV "claim"  ;
    lin _have_give_V2 = mkV2 (us_britishV "have") (mkPrep "give") ;
    lin _customer_N = mkN "customer"  ;
    lin _1013_N = mkN "10.1.3"  ;
    lin _v_N = mkN "v"  ;
    lin _valid_A = mkA "valid"  ;
    lin _maximum_A = mkA "maximum"  ;
    lin _unlawful_A = mkA "unlawful"  ;
    lin _sail_V = us_britishV "sail"  ;
    lin _pay_V = us_britishV "pay"  ;
    lin _assure_across_V2 = mkV2 (us_britishV "assure") (mkPrep "across") ;
    lin _sustain_V2 = mkV2 (us_britishV "sustain")  ;
    lin _while_Subj = mkSubj "while"  ;
    lin _register_with_V2 = mkV2 (us_britishV "register") (mkPrep "with") ;
    lin _cave_V = us_britishV "cave"  ;
    lin _rule_N = mkN "rule"  ;
    lin _regulation_N = mkN "regulation"  ;
    lin _damagetype_N = mkN "damagetype"  ;
    lin _part_N = mkN "part"  ;
    lin _Legionnaires_PN = mkPN "Legionnaires"  ;
    lin _claim_N = mkN "claim"  ;
    lin _money_N = mkN "money"  ;
    lin _dTime_PN = mkPN "dTime"  ;
    lin _birthdate_N = mkN "birthdate"  ;
    lin _1013_PN = mkPN "10.1.3"  ;
    lin _benfcpa_N = mkN "benfcpa"  ;
    lin _than_Prep = mkPrep "than"  ;
    lin _add_N = mkN "add"  ;
    lin _little_A = mkA "little"  ;
    lin _as_Subj = mkSubj "as"  ;
    lin _mind_N = mkN "mind"  ;
    lin _claim_for_V2 = mkV2 (us_britishV "claim") (mkPrep "for") ;
    lin _planaf_N = mkN "planaf"  ;
    lin _initial_A = mkA "initial"  ;
    lin _triple_A = mkA "triple"  ;
    lin _mean_V2 = mkV2 (us_britishV "mean")  ;
    lin _convenience_N = mkN "convenience"  ;
    lin _circumstancedescription_N = mkN "circumstancedescription"  ;
    lin _diving_PN = mkPN "diving"  ;
    lin _Benefit_PN = mkPN "Benefit"  ;
    lin _lose_through_V2 = mkV2 (us_britishV "lose") (mkPrep "through") ;
    lin _up_Prep = mkPrep "up"  ;
    lin _japanese_A = mkA "japanese"  ;
    lin _car_N = mkN "car"  ;
    lin _upscaled_A = mkA "upscaled"  ;
    lin _RETURN_PN = mkPN "RETURN"  ;
    lin _H7N9_PN = mkPN "H7N9"  ;
    lin _claim_V2 = mkV2 (us_britishV "claim")  ;
    lin _hand_N = mkN "hand"  ;
    lin _adjust_for_V2 = mkV2 (us_britishV "adjust") (mkPrep "for") ;
    lin _involve_V2 = mkV2 (us_britishV "involve")  ;
    lin _involving_Prep = mkPrep "involving" ;
    lin _place_N = mkN "place"  ;
    lin _injury_N = mkN "injury"  ;
    lin _self_injury_N = mkN "self-injury" ;
    lin _treatment_N = mkN "treatment"  ;
    lin _BSA_PN = mkPN "BSA"  ;
    lin _hrr_N = mkN "hrr"  ;
    lin _motocross_PN = mkPN "motocross"  ;
    lin _schema_PN = mkPN "schema"  ;
    lin _but_Subj = mkSubj "but"  ;
    lin _case_N = mkN "case"  ;
    lin _planf_N = mkN "planf"  ;
    lin _h7n9_N = mkN "h7n9"  ;
    lin _on_its_way_Adv = ParadigmsEng.mkAdv "on its way" ;
    lin _way_N = mkN "way"  ;
    lin _Medicine_PN = mkPN "Medicine"  ;
    lin _result_N = mkN "result"  ;
    lin _less_A = mkA "less"  ;
    lin _percentage_N = mkN "percentage"  ;
    lin _scuba_diving_N = mkN "scuba diving"  ;
    lin _Types_PN = mkPN "Types"  ;
    lin _revolution_N = mkN "revolution"  ;
    lin _physician_N = mkN "physician"  ;
    lin _use_N = mkN "use"  ;
    lin _third_A = mkA "third"  ;
    lin _first_A = mkA "first"  ;
    lin _circ_PN = mkPN "circ"  ;
    lin _travel_by_N2 = mkN2 (mkN "travel") (mkPrep "by") ;
    lin _burn_N = mkN "burn"  ;
    lin _shortening_N = mkN "shortening"  ;
    lin _wrestle_in_V2 = mkV2 (us_britishV "wrestle") (mkPrep "in") ;
    lin _set_in_V2 = mkV2 (us_britishV "set") (mkPrep "in") ;
    lin _Date_PN = mkPN "Date"  ;
    lin _windsurf_V = us_britishV "windsurf"  ;
    lin _travel_in_V2 = mkV2 (us_britishV "travel") (mkPrep "in") ;
    lin _Adjustment_PN = mkPN "Adjustment"  ;
    lin _benADDs_PN = mkPN "benADDs"  ;
    lin _aircraft_N = mkN "aircraft"  ;
    lin _current_A = mkA "current"  ;
    lin _husband_N = mkN "husband"  ;
    lin _Dismemberment_PN = mkPN "Dismemberment"  ;
    lin _successful_A = mkA "successful"  ;
    lin _viral_A = mkA "viral"  ;
    lin _equal_A = mkA "equal"  ;
    lin _qualifies_for_add_PN = mkPN "qualifies_for_add"  ;
    lin _contamination_N = mkN "contamination"  ;
    lin _bool_N = mkN "bool"  ;
    lin _as_Prep = mkPrep "as"  ;
    lin _virus_N = mkN "virus"  ;
    lin _parachute_V = us_britishV "parachute"  ;
    lin _low_A = mkA "low"  ;
    lin _competitive_A = mkA "competitive"  ;
    lin _hotel_N = mkN "hotel"  ;
    lin _Limit_PN = mkPN "Limit"  ;
    lin _register_V = us_britishV "register"  ;
    lin _alive_A = mkA "alive"  ;
    lin _fever_N = mkN "fever"  ;
    lin _theatre_N = mkN "theatre"  ;
    lin _claimable_A = mkA "claimable"  ;
    lin _after_Prep = mkPrep "after"  ;
    lin _normal_A = mkA "normal"  ;
    lin _declare_VS = mkVS (us_britishV "declare")  ;
    lin _with_Prep = mkPrep "with"  ;
    lin _Assured_PN = mkPN "Assured"  ;
    lin _deduce_VS = mkVS (us_britishV "deduce")  ;
    lin _relevant_A = mkA "relevant"  ;
    lin _lead_to_V2 = mkV2 (us_britishV "lead") (mkPrep "to") ;
    lin _exercise_N = mkN "exercise"  ;
    lin _head_N = mkN "head"  ;
    lin _mental_A = mkA "mental"  ;
    lin _police_N = mkN "police"  ;
    lin _Leg_PN = mkPN "Leg"  ;
    lin _section_N = mkN "section"  ;
    lin _license_V = us_britishV "license"  ;
    lin _dead_A = mkA "dead"  ;
    lin _CoV_PN = mkPN "CoV"  ;
    lin _physical_A = mkA "physical"  ;
    lin _assure_for_V2 = mkV2 (us_britishV "assure") (mkPrep "for") ;
    lin _apply_V = us_britishV "apply"  ;
    lin _basic_A = mkA "basic"  ;
    lin _policyholder_V = us_britishV "policyholder"  ;
    lin _traditional_A = mkA "traditional"  ;
    lin _Step_PN = mkPN "Step"  ;
    lin _holder_PN = mkPN "holder"  ;
    lin _lens_N = mkN "lens"  ;
    lin _die_from_V2 = mkV2 (us_britishV "die") (mkPrep "from") ;
    lin _H5N1_PN = mkPN "H5N1"  ;
    lin _surgical_A = mkA "surgical"  ;
    lin _nature_N = mkN "nature"  ;
    lin _transform_into_V2 = mkV2 (us_britishV "transform") (mkPrep "into") ;
    lin _only_Adv = mkAdv "only"  ;
    lin _benefit_V2 = mkV2 (us_britishV "benefit")  ;
    lin _fully_Adv = mkAdv "fully"  ;
    lin _MIN_PN = mkPN "MIN"  ;
    lin _step_V2 = mkV2 (us_britishV "step")  ;
    lin _terrorism_N = mkN "terrorism"  ;
    lin _commercial_A = mkA "commercial"  ;
    lin _include_V = us_britishV "include"  ;
    lin _name_N = mkN "name"  ;
    lin _hunt_V = us_britishV "hunt"  ;
    lin _PlanAF_PN = mkPN "PlanAF"  ;
    lin _operation_N = mkN "operation"  ;
    lin _333A_PN = mkPN "333A"  ;
    lin _previous_A = mkA "previous"  ;
    lin _give_V = us_britishV "give"  ;
    lin _other_A = mkA "other"  ;
    lin _addpercentageforinjury_N = mkN "addpercentageforinjury"  ;
    lin _line_N = mkN "line"  ;
    lin _activity_N = mkN "activity"  ;
    lin _adjust_V = us_britishV "adjust"  ;
    lin _eye_N = mkN "eye"  ;
    lin _Subscribed_PN = mkPN "Subscribed"  ;
    lin _x_PN = mkPN "x"  ;
    lin _hence_Adv = mkAdv "hence"  ;
    lin _occur_V = us_britishV "occur"  ;
    lin _Ontology_PN = mkPN "Ontology"  ;
    lin _start_N = mkN "start" ;
    lin _Mumps_PN = mkPN "Mumps"  ;
    lin _string_N = mkN "string"  ;
    lin _plan4_PN = mkPN "plan4"  ;
    lin _alcohol_N = mkN "alcohol"  ;
    lin _H9N2_PN = mkPN "H9N2"  ;
    lin _organise_V = us_britishV "organise"  ;
    lin _benefit_N = mkN "benefit"  ;
    lin _type_N = mkN "type"  ;
    lin _waterborne_A = mkA "waterborne"  ;
    lin _when_Subj = mkSubj "when"  ;
    lin _non_N = mkN "non"  ;
    lin _high_A = mkA "high"  ;
    lin _Event_PN = mkPN "Event"  ;
    lin _occur_at_V2 = mkV2 (us_britishV "occur") (mkPrep "at") ;
    lin _fit_N = mkN "fit"  ;
    lin _least_Adv = mkAdv "least"  ;
    lin _policysubscription_N = mkN "policysubscription"  ;
    lin _union_N = mkN "union"  ;
    lin _diagnose_with_V2 = mkV2 (us_britishV "diagnose") (mkPrep "with") ;
    lin _risk_N = mkN "risk"  ;
    lin _define_V2 = mkV2 (us_britishV "define")  ;
    lin _met_common_requirement_for_add_V2 = mkV2 (us_britishV "met_common_requirement_for_add")  ;
    lin _defect_N = mkN "defect"  ;
    lin _polo_N = mkN "polo"  ;
    lin _policyHolder_PN = mkPN "policyHolder"  ;
    lin _Cap_PN = mkPN "Cap"  ;
    lin _tcmpb_N = mkN "tcmpb"  ;
    lin _pedestrian_N = mkN "pedestrian"  ;
    lin _fire_N = mkN "fire"  ;
    lin _Accidental_PN = mkPN "Accidental"  ;
    lin _Life_PN = mkPN "Life"  ;
    lin _assure_in_V2 = mkV2 (us_britishV "assure") (mkPrep "in") ;
    lin _kind_N = mkN "kind" ;
    lin _CN_of_any_kind_CN illness = AdvCN illness (ParadigmsEng.mkAdv "of any kind") ;
    lin _double_A = mkA "double"  ;
    lin _claimant_N = mkN "claimant"  ;
    lin _suicide_N = mkN "suicide"  ;
    lin _Reductions_PN = mkPN "Reductions"  ;
    lin _occur_before_V2 = mkV2 (us_britishV "occur") (mkPrep "before") ;
    lin _on_Prep = mkPrep "on"  ;
    lin _area_N = mkN "area"  ;
    lin _Head_PN = mkPN "Head"  ;
    lin _y_PN = mkPN "y"  ;
    lin _MR_PN = mkPN "MR"  ;
    lin _by_Prep = mkPrep "by"  ;
    lin _fractured_A = mkA "fractured"  ;
    lin _benefpaidout_N = mkN "benefpaidout"  ;
    lin _Conditions_PN = mkPN "Conditions"  ;
    lin _attempt_N = mkN "attempt"  ;
    lin _ADD_PN = mkPN "ADD"  ;
    lin _hearing_N = mkN "hearing"  ;
    lin _polilcy_N = mkN "polilcy"  ;
    lin _Claim_PN = mkPN "Claim"  ;
    lin _for_Prep = mkPrep "for"  ;
    lin _assure_per_V2 = mkV2 (us_britishV "assure") (mkPrep "per") ;
    lin _radiation_N = mkN "radiation"  ;
    lin _declaration_N = mkN "declaration"  ;
    lin _occur_during_V2 = mkV2 (us_britishV "occur") _during_Prep ;
    lin _work_N = mkN "work"  ;
    lin _accidental_A = mkA "accidental"  ;
    lin _as_Adv = mkAdv "as"  ;
    lin _accident_N = mkN "accident"  ;
    lin _give_V2 = mkV2 (us_britishV "give")  ;
    lin _Republic_PN = mkPN "Republic"  ;
    lin _hear_in_V2 = mkV2 (us_britishV "hear") (mkPrep "in") ;
    lin _get_from_V2 = mkV2 (us_britishV "get") (mkPrep "from") ;
    lin _Plan14_PN = mkPN "Plan14"  ;
    lin _reasoner_N = mkN "reasoner"  ;
    lin _limit_N = mkN "limit"  ;
    lin _H7N7_V = us_britishV "H7N7"  ;
    lin _timeyear_N = mkN "timeyear"  ;
    lin _life_N = mkN "life"  ;
    lin _establish_V = us_britishV "establish"  ;
    lin _pland_Adv = mkAdv "pland"  ;
    lin _supervise_V = us_britishV "supervise"  ;
    lin _out_Prep = mkPrep "out"  ;
    lin _start_VS = mkVS (us_britishV "start")  ;
    lin _riot_N = mkN "riot"  ;
    lin _transport_N = mkN "transport"  ;
    lin _H7N7_N = mkN "H7N7"  ;
    lin _both_Subj = mkSubj "both"  ;
    lin _leg_N = mkN "leg"  ;
    lin _firstclaimyear_N = mkN "firstclaimyear"  ;
    lin _Section_PN = mkPN "Section"  ;
    lin _list_N = mkN "list"  ;
    lin _recognise_V = us_britishV "recognise"  ;
    lin _planF_PN = mkPN "planF"  ;
    lin _benADD_PN = mkPN "benADD"  ;
    lin _SG_PN = mkPN "SG"  ;
    lin _AIDS_PN = mkPN "AIDS"  ;
    lin _human_A = mkA "human"  ;
    lin _cover_V = us_britishV "cover"  ;
    lin _address_N = mkN "address"  ;
    lin _Address_PN = mkPN "Address"  ;
    lin _duty_N = mkN "duty"  ;
    lin _patella_N = mkN "patella"  ;
    lin _schedule_N = mkN "schedule"  ;
    lin _particular_A = mkA "particular"  ;
    lin _participate_in_V2 = mkV2 (mkV "participate") in_Prep ;
    lin _passenger_N = mkN "passenger"  ;
    lin _where_Subj = mkSubj "where"  ;
    lin _total_A = mkA "total"  ;
    lin _addsa_N = mkN "addsa"  ;
    lin _of_Prep = mkPrep "of"  ;
    lin _phalanx_N = mkN "phalanx"  ;
    lin _eligibility_N = mkN "eligibility"  ;
    lin _past_A = mkA "past"  ;
    lin _assure_V2 = mkV2 (us_britishV "assure")  ;
    lin _racing_N = mkN "racing"  ;
    lin _f_N = mkN "f"  ;
    lin _ponder_V2 = mkV2 (us_britishV "ponder")  ;
    lin _incident_N = mkN "incident"  ;
    lin _life_assured_die_N = mkN "life_assured_die"  ;
    lin _ps_N = mkN "ps"  ;
    lin _in_Prep = mkPrep "in"  ;
    lin _Expense_PN = mkPN "Expense"  ;
    lin _before_Subj = mkSubj "before"  ;
    lin _p_N = mkN "p"  ;
    lin _1014_PN = mkPN "10.1.4"  ;
    lin _canoe_V = us_britishV "canoe"  ;
    lin _pothole_V = us_britishV "pothole"  ;
    lin _exception_N = mkN "exception"  ;
    lin _limb_N = mkN "limb"  ;
    lin _between_Prep = mkPrep "between"  ;
    lin _school_N = mkN "school"  ;
    lin _logical_A = mkA "logical"  ;
    lin _sight_N = mkN "sight"  ;
    lin _policyholder_N = mkN ("policyHolder"|"policyholder")   ;
    lin _refund_V2 = mkV2 (us_britishV "refund")  ;
    lin _cost_N = mkN "cost"  ;
    lin _located_in_A2 = mkA2 (mkA "located") (mkPrep "in") ;
    lin _unsuccessful_A = mkA "unsuccessful"  ;
    lin _during_Prep = mkPrep ("during"|"DURING") ;
    lin _happen_after_V2 = mkV2 (us_britishV "happen") (mkPrep "after") ;
    lin _hfmd_A = mkA "hfmd"  ;
    lin _tsapp_N = mkN "tsapp"  ;
    lin _possibility_N = mkN "possibility"  ;
    lin _ear_N = mkN "ear"  ;
    lin _TABLE_PN = mkPN "TABLE"  ;
    lin _authority_N = mkN "authority"  ;
    lin _federation_N = mkN "federation"  ;
    lin _Insurer_PN = mkPN "Insurer"  ;
    lin _Policy_PN = mkPN "Policy"  ;
    lin _Flu_PN = mkPN "Flu"  ;
    lin _condition_N = mkN "condition"  ;
    lin _LA_PN = mkPN "LA"  ;
    lin _hang_VV = mkVV (us_britishV "hang")  ;
    lin _over_Prep = mkPrep "over"  ;
    lin _benTCM_PN = mkPN "benTCM"  ;
    lin _pay_for_V2 = mkV2 (us_britishV "pay") (mkPrep "for") ;
    lin _payable_A = mkA "payable"  ;
    lin _military_N = mkN "military"  ;
    lin _plana_N = mkN "plana"  ;
    lin _p_PN = mkPN "p"  ;
    lin _H7N_PN = mkPN "H7N"  ;
    lin _vessel_N = mkN "vessel"  ;
    lin _foot_N = mkN "foot"  ;
    lin _incurable_A = mkA "incurable"  ;
    lin _employee_N = mkN "employee"  ;
    lin _fare_N = mkN "fare"  ;
    lin _military_A = mkA "military"  ;
    lin _exception_V = us_britishV "exception"  ;
    lin _policy_N = mkN "policy"  ;
    lin _person_N = mkN "person"  ;
    lin _mean_VS = mkVS (us_britishV "mean")  ;
    lin _1012_PN = mkPN "10.1.2"  ;
    lin _include_V2 = mkV2 (us_britishV "include")  ;
    lin _within_Prep = mkPrep "within"  ;
    lin _pay_V2 = mkV2 (us_britishV "pay")  ;
    lin _Melioidosis_PN = mkPN "Melioidosis"  ;
    lin _chemical_A = mkA "chemical"  ;
    lin _N_PN = mkPN "N"  ;
    lin _pland_N = mkN "pland"  ;
    lin _cm_N = mkN "cm"  ;
    lin _intentional_A = mkA "intentional"  ;
    lin _predicate_N = mkN "predicate"  ;
    lin _dType_PN = mkPN "dType"  ;
    lin _try_VV = mkVV (us_britishV "try")  ;
    lin _result_from_V2 = mkV2 (us_britishV "result") (mkPrep "from") ;
    lin _through_Prep = mkPrep "through"  ;
    lin _registered_A = mkA "registered"  ;
    lin _expense_N = mkN "expense"  ;
    lin _premise_N = mkN "premise"  ;
    lin _premise_where_N2 = mkN2 _premise_N (mkPrep "where") ;
    lin _year_N = mkN "year"  ;
    lin _circumstance_N = mkN "circumstance"  ;
    lin _life_assured_suffers_injury_does_not_die_within_30_day_N = mkN "life_assured_suffers_injury_does_not_die_within_30_day"  ;
    lin _make_within_V2 = mkV2 (us_britishV "make") (mkPrep "within") ;
    lin _description_N = mkN "description"  ;
    lin _diagnosis_N = mkN "diagnosis"  ;
    lin _train_V = us_britishV "train"  ;
    lin _Singapore_PN = mkPN "Singapore"  ;
    lin _glide_V = us_britishV "glide"  ;
    lin _sssure_for_V2 = mkV2 (us_britishV "sssure") (mkPrep "for") ;
    lin _damageevent_N = mkN "damageevent"  ;
    lin _infection_N = mkN "infection"  ;
    lin _mean_V = us_britishV "mean"  ;
    lin _lose_V2 = mkV2 (us_britishV "lose")  ;
    lin _payout_N = mkN "payout"  ;
    lin _y_N = mkN "y"  ;
    lin _riding_N = mkN "riding"  ;
    lin _service_N = mkN "service"  ;
    lin _mean_as_V2 = mkV2 (us_britishV "mean") (mkPrep "as") ;
    lin _association_N = mkN "association"  ;
    lin _hernia_N = mkN "hernia"  ;
    lin _month_N = mkN "month"  ;
    lin _competition_N = mkN "competition"  ;
    lin _Influenza_PN = mkPN "Influenza"  ;
    lin _c_N = mkN "c"  ;
    lin _skydive_V = us_britishV "skydive"  ;
    lin _m_N = mkN "m"  ;
    lin _private_A = mkA "private"  ;
    lin _accepted_A = mkA "accepted"  ;
    lin _age_N = mkN "age"  ;
    lin _Details_PN = mkPN "Details"  ;
    lin _contract_N = mkN "contract"  ;
    lin _member_N = mkN "member"  ;
    lin _e_N = mkN "e"  ;
    lin _number_N = mkN "number"  ;
    lin _Ministry_PN = mkPN "Ministry"  ;
    lin _mall_N = mkN "mall"  ;
    lin _Yellow_PN = mkPN "Yellow"  ;
    lin _verifier_N = mkN "verifier"  ;
    lin _under_Prep = mkPrep "under"  ;
    lin _due_A = mkA "due"  ;
    lin _multiple_A = mkA "multiple"  ;
    lin _vehicle_N = mkN "vehicle"  ;
    lin _d_N = mkN "d"  ;
    lin _cover_V2 = mkV2 (us_britishV "cover")  ;
    lin _hockey_N = mkN "hockey"  ;
    lin _declare_V2 = mkV2 (us_britishV "declare")  ;
    lin _play_V2 = mkV2 (us_britishV "play")  ;
    lin _planE_PN = mkPN "planE"  ;
    lin _national_A = mkA "national"  ;
    lin _per_Prep = mkPrep "per"  ;
    lin _planB_PN = mkPN "planB"  ;
    lin _stepuppercentage_N = mkN "stepuppercentage"  ;
    lin _adjusted_A = mkA "adjusted"  ;
    lin _disease_N = mkN "disease"  ;
    lin _Triple_PN = mkPN "Triple"  ;
    lin _disabled_A = mkA "disabled"  ;
    lin _subject_A = mkA "subject"  ;
    lin _holder_N = mkN "holder"  ;
    lin _then_Adv = mkAdv "then"  ;
    lin _task_N = mkN "task"  ;
    lin _save_V2 = mkV2 (us_britishV "save")  ;
    lin _adjustment_N = mkN "adjustment"  ;
    lin _airline_N = mkN "airline"  ;
    lin _to_Prep = mkPrep "to"  ;
    lin _infectious_A = mkA "infectious"  ;
    lin _death_N = mkN "death"  ;
    lin _time_N = mkN "time"  ;
    lin _professional_A = mkA "professional"  ;
    lin _life_assured_N = mkN "life assured"  ;
    lin _form_N = mkN "form"  ;
    lin _dc_N = mkN "dc"  ;
    lin _sum_list_PN = mkPN "sum_list"  ;
    lin _have_V2 = mkV2 (us_britishV "have")  ;
    lin _toe_N = mkN "toe"  ;
    lin _soon_Adv = mkAdv "soon"  ;
    lin _plan3_PN = mkPN "plan3"  ;
    lin _Wife_PN = mkPN "Wife"  ;
    lin _tcm_N = mkN "tcm"  ;
    lin _Service_PN = mkPN "Service"  ;
    lin _loss_N = mkN "loss"  ;
    lin _Accident_PN = mkPN "Accident"  ;
    lin _Death_PN = mkPN "Death"  ;
    lin _suffer_in_V2 = mkV2 (us_britishV "suffer") (mkPrep "in") ;
    lin _from_Prep = mkPrep "from"  ;
    lin _dangerous_A = mkA "dangerous"  ;
    lin _M_PN = mkPN "M"  ;
    lin _mountaineer_V = us_britishV "mountaineer"  ;
    lin _assure_of_V2 = mkV2 (us_britishV "assure") (mkPrep "of") ;
    lin _met_common_requirement_for_add_V = us_britishV "met_common_requirement_for_add"  ;
    lin _vcjd_N = mkN "vcjd"  ;
-- some PNs as Ns
    lin _1012_N = mkN "10.1.2"  ;
    lin _1014_N = mkN "10.1.4"  ;
    lin _333A_N = mkN "333A"  ;
    lin _ADD_N = mkN "ADD"  ;
    lin _AIDS_N = mkN "AIDS"  ;
    lin _Accident_N = mkN "Accident"  ;
    lin _Accidental_N = mkN "Accidental"  ;
    lin _Accidents_N = mkN "Accidents"  ;
    lin _Address_N = mkN "Address"  ;
    lin _Adjustment_N = mkN "Adjustment"  ;
    lin _Assured_N = mkN "Assured"  ;
    lin _BSA_N = mkN "BSA"  ;
    lin _Benefit_N = mkN "Benefit"  ;
    lin _Cap_N = mkN "Cap"  ;
    lin _Claim_N = mkN "Claim"  ;
    lin _CoV_N = mkN "CoV"  ;
    lin _Conditions_N = mkN "Conditions"  ;
    lin _Date_N = mkN "Date"  ;
    lin _Death_N = mkN "Death"  ;
    lin _Details_N = mkN "Details"  ;
    lin _Disease_N = mkN "Disease"  ;
    lin _Dismemberment_N = mkN "Dismemberment"  ;
    lin _Event_N = mkN "Event"  ;
    lin _Expense_N = mkN "Expense"  ;
    lin _Flu_N = mkN "Flu"  ;
    lin _H7N9_N = mkN "H7N9"  ;
    lin _H7N_N = mkN "H7N"  ;
    lin _H9N2_N = mkN "H9N2"  ;
    lin _HAS_N = mkN "HAS"  ;
    lin _Head_N = mkN "Head"  ;
    lin _Health_N = mkN "Health"  ;
    lin _Influenza_N = mkN "Influenza"  ;
    lin _Injury_N = mkN "Injury"  ;
    lin _Insurer_N = mkN "Insurer"  ;
    lin _LA_N = mkN "LA"  ;
    lin _LE_N = mkN "LE"  ;
    lin _Leg_N = mkN "Leg"  ;
    lin _Legionnaires_N = mkN "Legionnaires"  ;
    lin _Life_N = mkN "Life"  ;
    lin _Limit_N = mkN "Limit"  ;
    lin _MAP_N = mkN "MAP"  ;
    lin _MIN_N = mkN "MIN"  ;
    lin _MR_N = mkN "MR"  ;
    lin _M_N = mkN "M"  ;
    lin _Medicine_N = mkN "Medicine"  ;
    lin _Melioidosis_N = mkN "Melioidosis"  ;
    lin _Ministry_N = mkN "Ministry"  ;
    lin _Mumps_N = mkN "Mumps"  ;
    lin _N_N = mkN "N"  ;
    lin _Nipah_N = mkN "Nipah"  ;
    lin _Ontology_N = mkN "Ontology"  ;
    lin _PS_N = mkN "PS"  ;
    lin _Plan14_N = mkN "Plan14"  ;
    lin _PlanAF_N = mkN "PlanAF"  ;
    lin _PolicyHolder_N = mkN "PolicyHolder"  ;
    lin _Policy_N = mkN "Policy"  ;
    lin _RETURN_N = mkN "RETURN"  ;
    lin _Reductions_N = mkN "Reductions"  ;
    lin _Removal_N = mkN "Removal"  ;
    lin _Republic_N = mkN "Republic"  ;
    lin _SA_N = mkN "SA"  ;
    lin _SG_N = mkN "SG"  ;
    lin _Schedule_N = mkN "Schedule"  ;
    lin _Section_N = mkN "Section"  ;
    lin _Service_N = mkN "Service"  ;
    lin _Singapore_N = mkN "Singapore"  ;
    lin _Step_N = mkN "Step"  ;
    lin _Subscribed_N = mkN "Subscribed"  ;
    lin _TABLE_N = mkN "TABLE"  ;
    lin _Teeth_N = mkN "Teeth"  ;
    lin _Triple_N = mkN "Triple"  ;
    lin _Type_N = mkN "Type"  ;
    lin _Types_N = mkN "Types"  ;
    lin _UPON_N = mkN "UPON"  ;
    lin _Wife_N = mkN "Wife"  ;
    lin _Yellow_N = mkN "Yellow"  ;
    lin _addSA_N = mkN "addSA"  ;
    lin _benADD_N = mkN "benADD"  ;
    lin _benADDs_N = mkN "benADDs"  ;
    lin _benRA_N = mkN "benRA"  ;
    lin _benTCM_N = mkN "benTCM"  ;
    lin _circ_N = mkN "circ"  ;
    lin _dTime_N = mkN "dTime"  ;
    lin _dType_N = mkN "dType"  ;
    lin _diving_N = mkN "diving"  ;
    lin _motocross_N = mkN "motocross"  ;
    lin _plan3_N = mkN "plan3"  ;
    lin _plan4_N = mkN "plan4"  ;
    lin _planAF_N = mkN "planAF"  ;
    lin _planB_N = mkN "planB"  ;
    lin _planC_N = mkN "planC"  ;
    lin _planE_N = mkN "planE"  ;
    lin _planF_N = mkN "planF"  ;
    lin _policyHolder_N = mkN "policyHolder"  ;
    lin _qualifies_for_add_N = mkN "qualifies_for_add"  ;
    lin _schema_N = mkN "schema"  ;
    lin _sum_list_N = mkN "sum_list"  ;
    lin _x_N = mkN "x"  ;
}