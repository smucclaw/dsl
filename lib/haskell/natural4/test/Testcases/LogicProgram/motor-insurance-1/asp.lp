according_to(rule_SecA_PayBreakdownVehicle, coverProvided(S, payBreakdownVehicle)) :- legally_holds(isBreakdownSituation(S)), legally_holds(situationInLocation(S, Loc)), legally_holds(distanceLessThan20(home, Loc)).
according_to(rule_VehicleBreakdown, isBreakdownSituation(S)) :- legally_holds(isVehicleInSituation(S, Veh)), legally_holds(gotStolen(Veh)).
according_to(rule_VehicleBreakdown_, isBreakdownSituation(S, lv1)) :- legally_holds(isVehicleInSituation(S, Veh)), legally_holds(gotStolen(lv2, Veh)).
according_to(rule_VehicleBreakdown, isBreakdownSituation(S)) :- legally_holds(isVehicleInSituation(S, Veh)), legally_holds(hasFlatBattery(Veh)).

explains(isBreakdownSituation(S), coverProvided(S, payBreakdownVehicle) ,_N) :- createSub(subInst_rule_SecA_PayBreakdownVehicle(Loc,S),_N).
explains(situationInLocation(S, Loc), coverProvided(S, payBreakdownVehicle) ,_N) :- createSub(subInst_rule_SecA_PayBreakdownVehicle(Loc,S),_N).
explains(distanceLessThan20(home, Loc), coverProvided(S, payBreakdownVehicle) ,_N) :- createSub(subInst_rule_SecA_PayBreakdownVehicle(Loc,S),_N).
explains(isVehicleInSituation(S, Veh), isBreakdownSituation(S) ,_N) :- createSub(subInst_rule_VehicleBreakdown(S,Veh),_N).
explains(gotStolen(Veh), isBreakdownSituation(S) ,_N) :- createSub(subInst_rule_VehicleBreakdown(S,Veh),_N).
explains(isVehicleInSituation(S, Veh), isBreakdownSituation(S, lv1) ,_N) :- createSub(subInst_rule_VehicleBreakdown_(S,Veh),_N).
explains(gotStolen(lv2, Veh), isBreakdownSituation(S, lv1) ,_N) :- createSub(subInst_rule_VehicleBreakdown_(S,Veh),_N).
explains(isVehicleInSituation(S, Veh), isBreakdownSituation(S) ,_N) :- createSub(subInst_rule_VehicleBreakdown(S,Veh),_N).
explains(hasFlatBattery(Veh), isBreakdownSituation(S) ,_N) :- createSub(subInst_rule_VehicleBreakdown(S,Veh),_N).



createSub(subInst_rule_SecA_PayBreakdownVehicle(extVar,S),_N+1) :- query(coverProvided(S, payBreakdownVehicle) ,_N), _N < M, max_ab_lvl(M).
createSub(subInst_rule_VehicleBreakdown(S,extVar),_N+1) :- query(isBreakdownSituation(S) ,_N), _N < M, max_ab_lvl(M).
createSub(subInst_rule_VehicleBreakdown_(S,extVar),_N+1) :- query(isBreakdownSituation(S, lv1) ,_N), _N < M, max_ab_lvl(M).
createSub(subInst_rule_VehicleBreakdown(S,extVar),_N+1) :- query(isBreakdownSituation(S) ,_N), _N < M, max_ab_lvl(M).

createSub(subInst_rule_SecA_PayBreakdownVehicle(V_Loc,S),_N) :- createSub(subInst_rule_SecA_PayBreakdownVehicle(V_Loc,V_S),_N), legally_holds(coverProvided(S, payBreakdownVehicle)).
createSub(subInst_rule_SecA_PayBreakdownVehicle(V_Loc,S),_N) :- createSub(subInst_rule_SecA_PayBreakdownVehicle(V_Loc,V_S),_N), legally_holds(isBreakdownSituation(S)).
createSub(subInst_rule_SecA_PayBreakdownVehicle(Loc,S),_N) :- createSub(subInst_rule_SecA_PayBreakdownVehicle(V_Loc,V_S),_N), legally_holds(situationInLocation(S, Loc)).
createSub(subInst_rule_SecA_PayBreakdownVehicle(Loc,V_S),_N) :- createSub(subInst_rule_SecA_PayBreakdownVehicle(V_Loc,V_S),_N), legally_holds(distanceLessThan20(home, Loc)).
createSub(subInst_rule_VehicleBreakdown(S,V_Veh),_N) :- createSub(subInst_rule_VehicleBreakdown(V_S,V_Veh),_N), legally_holds(isBreakdownSituation(S)).
createSub(subInst_rule_VehicleBreakdown(S,Veh),_N) :- createSub(subInst_rule_VehicleBreakdown(V_S,V_Veh),_N), legally_holds(isVehicleInSituation(S, Veh)).
createSub(subInst_rule_VehicleBreakdown(V_S,Veh),_N) :- createSub(subInst_rule_VehicleBreakdown(V_S,V_Veh),_N), legally_holds(gotStolen(Veh)).
createSub(subInst_rule_VehicleBreakdown_(S,V_Veh),_N) :- createSub(subInst_rule_VehicleBreakdown_(V_S,V_Veh),_N), legally_holds(isBreakdownSituation(S, lv1)).
createSub(subInst_rule_VehicleBreakdown_(S,Veh),_N) :- createSub(subInst_rule_VehicleBreakdown_(V_S,V_Veh),_N), legally_holds(isVehicleInSituation(S, Veh)).
createSub(subInst_rule_VehicleBreakdown_(V_S,Veh),_N) :- createSub(subInst_rule_VehicleBreakdown_(V_S,V_Veh),_N), legally_holds(gotStolen(lv2, Veh)).
createSub(subInst_rule_VehicleBreakdown(S,V_Veh),_N) :- createSub(subInst_rule_VehicleBreakdown(V_S,V_Veh),_N), legally_holds(isBreakdownSituation(S)).
createSub(subInst_rule_VehicleBreakdown(S,Veh),_N) :- createSub(subInst_rule_VehicleBreakdown(V_S,V_Veh),_N), legally_holds(isVehicleInSituation(S, Veh)).
createSub(subInst_rule_VehicleBreakdown(V_S,Veh),_N) :- createSub(subInst_rule_VehicleBreakdown(V_S,V_Veh),_N), legally_holds(hasFlatBattery(Veh)).

createSub(subInst_rule_SecA_PayBreakdownVehicle(V_Loc,S),_N) :- createSub(subInst_rule_SecA_PayBreakdownVehicle(V_Loc,V_S),_N), query(coverProvided(S, payBreakdownVehicle),L).
createSub(subInst_rule_SecA_PayBreakdownVehicle(V_Loc,S),_N) :- createSub(subInst_rule_SecA_PayBreakdownVehicle(V_Loc,V_S),_N), query(isBreakdownSituation(S),L).
createSub(subInst_rule_SecA_PayBreakdownVehicle(Loc,S),_N) :- createSub(subInst_rule_SecA_PayBreakdownVehicle(V_Loc,V_S),_N), query(situationInLocation(S, Loc),L).
createSub(subInst_rule_SecA_PayBreakdownVehicle(Loc,V_S),_N) :- createSub(subInst_rule_SecA_PayBreakdownVehicle(V_Loc,V_S),_N), query(distanceLessThan20(home, Loc),L).
createSub(subInst_rule_VehicleBreakdown(S,V_Veh),_N) :- createSub(subInst_rule_VehicleBreakdown(V_S,V_Veh),_N), query(isBreakdownSituation(S),L).
createSub(subInst_rule_VehicleBreakdown(S,Veh),_N) :- createSub(subInst_rule_VehicleBreakdown(V_S,V_Veh),_N), query(isVehicleInSituation(S, Veh),L).
createSub(subInst_rule_VehicleBreakdown(V_S,Veh),_N) :- createSub(subInst_rule_VehicleBreakdown(V_S,V_Veh),_N), query(gotStolen(Veh),L).
createSub(subInst_rule_VehicleBreakdown_(S,V_Veh),_N) :- createSub(subInst_rule_VehicleBreakdown_(V_S,V_Veh),_N), query(isBreakdownSituation(S, lv1),L).
createSub(subInst_rule_VehicleBreakdown_(S,Veh),_N) :- createSub(subInst_rule_VehicleBreakdown_(V_S,V_Veh),_N), query(isVehicleInSituation(S, Veh),L).
createSub(subInst_rule_VehicleBreakdown_(V_S,Veh),_N) :- createSub(subInst_rule_VehicleBreakdown_(V_S,V_Veh),_N), query(gotStolen(lv2, Veh),L).
createSub(subInst_rule_VehicleBreakdown(S,V_Veh),_N) :- createSub(subInst_rule_VehicleBreakdown(V_S,V_Veh),_N), query(isBreakdownSituation(S),L).
createSub(subInst_rule_VehicleBreakdown(S,Veh),_N) :- createSub(subInst_rule_VehicleBreakdown(V_S,V_Veh),_N), query(isVehicleInSituation(S, Veh),L).
createSub(subInst_rule_VehicleBreakdown(V_S,Veh),_N) :- createSub(subInst_rule_VehicleBreakdown(V_S,V_Veh),_N), query(hasFlatBattery(Veh),L).

caused_by(pos, legally_holds(isBreakdownSituation(S)), according_to(rule_SecA_PayBreakdownVehicle, coverProvided(S, payBreakdownVehicle)), _N+1) :- according_to(rule_SecA_PayBreakdownVehicle, coverProvided(S, payBreakdownVehicle)), legally_holds(isBreakdownSituation(S)), legally_holds(situationInLocation(S, Loc)), legally_holds(distanceLessThan20(home, Loc)), justify(according_to(rule_SecA_PayBreakdownVehicle, coverProvided(S, payBreakdownVehicle)), _N).
caused_by(pos, legally_holds(situationInLocation(S, Loc)), according_to(rule_SecA_PayBreakdownVehicle, coverProvided(S, payBreakdownVehicle)), _N+1) :- according_to(rule_SecA_PayBreakdownVehicle, coverProvided(S, payBreakdownVehicle)), legally_holds(isBreakdownSituation(S)), legally_holds(situationInLocation(S, Loc)), legally_holds(distanceLessThan20(home, Loc)), justify(according_to(rule_SecA_PayBreakdownVehicle, coverProvided(S, payBreakdownVehicle)), _N).
caused_by(pos, legally_holds(distanceLessThan20(home, Loc)), according_to(rule_SecA_PayBreakdownVehicle, coverProvided(S, payBreakdownVehicle)), _N+1) :- according_to(rule_SecA_PayBreakdownVehicle, coverProvided(S, payBreakdownVehicle)), legally_holds(isBreakdownSituation(S)), legally_holds(situationInLocation(S, Loc)), legally_holds(distanceLessThan20(home, Loc)), justify(according_to(rule_SecA_PayBreakdownVehicle, coverProvided(S, payBreakdownVehicle)), _N).
caused_by(pos, legally_holds(isVehicleInSituation(S, Veh)), according_to(rule_VehicleBreakdown, isBreakdownSituation(S)), _N+1) :- according_to(rule_VehicleBreakdown, isBreakdownSituation(S)), legally_holds(isVehicleInSituation(S, Veh)), legally_holds(gotStolen(Veh)), justify(according_to(rule_VehicleBreakdown, isBreakdownSituation(S)), _N).
caused_by(pos, legally_holds(gotStolen(Veh)), according_to(rule_VehicleBreakdown, isBreakdownSituation(S)), _N+1) :- according_to(rule_VehicleBreakdown, isBreakdownSituation(S)), legally_holds(isVehicleInSituation(S, Veh)), legally_holds(gotStolen(Veh)), justify(according_to(rule_VehicleBreakdown, isBreakdownSituation(S)), _N).
caused_by(pos, legally_holds(isVehicleInSituation(S, Veh)), according_to(rule_VehicleBreakdown_, isBreakdownSituation(S, lv1)), _N+1) :- according_to(rule_VehicleBreakdown_, isBreakdownSituation(S, lv1)), legally_holds(isVehicleInSituation(S, Veh)), legally_holds(gotStolen(lv2, Veh)), justify(according_to(rule_VehicleBreakdown_, isBreakdownSituation(S, lv1)), _N).
caused_by(pos, legally_holds(gotStolen(lv2, Veh)), according_to(rule_VehicleBreakdown_, isBreakdownSituation(S, lv1)), _N+1) :- according_to(rule_VehicleBreakdown_, isBreakdownSituation(S, lv1)), legally_holds(isVehicleInSituation(S, Veh)), legally_holds(gotStolen(lv2, Veh)), justify(according_to(rule_VehicleBreakdown_, isBreakdownSituation(S, lv1)), _N).
caused_by(pos, legally_holds(isVehicleInSituation(S, Veh)), according_to(rule_VehicleBreakdown, isBreakdownSituation(S)), _N+1) :- according_to(rule_VehicleBreakdown, isBreakdownSituation(S)), legally_holds(isVehicleInSituation(S, Veh)), legally_holds(hasFlatBattery(Veh)), justify(according_to(rule_VehicleBreakdown, isBreakdownSituation(S)), _N).
caused_by(pos, legally_holds(hasFlatBattery(Veh)), according_to(rule_VehicleBreakdown, isBreakdownSituation(S)), _N+1) :- according_to(rule_VehicleBreakdown, isBreakdownSituation(S)), legally_holds(isVehicleInSituation(S, Veh)), legally_holds(hasFlatBattery(Veh)), justify(according_to(rule_VehicleBreakdown, isBreakdownSituation(S)), _N).


