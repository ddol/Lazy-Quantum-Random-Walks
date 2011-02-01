(* ::Package:: *)

(* ::Input:: *)
(**)


BeginPackage["lqrw`"];
SingleLazyItteration::useage = "SingleLazyItteration[State, Coin] returns the\
state after it has been randomised by one itteration of a given three state coin."

CumulativeMean::usage = "CumulativeMean[List] returns a list where each \
value is the cumilative mean of all previous list values."; 

StatesToPositionalProbabilites::useage = "StatesToPositionalProbabilites[State]\
returns the position probabilites from a matrix of states.";

MultipleLazySteps::useage = "MultipleLazySteps[State, Coin, Steps] returns the state\
after a number of itterations using the Coin has been performed."

MultipleLazyStepsHistory::useage = "MultipleLazyStepsHistory is identical \
to MultipleLazySteps but returns all steps instead of just the final result"

MultipleLazyStepsRecursive::useage = "MultipleLazyStepsRecursive is functionally\
identical to MultipleLazySteps, but uses a recursive function."


Begin["`Private`"];

SingleLazyItteration[State0_,Coin0_] := Module[{State = State0, Coin = Coin0},
   oState = Normal[SparseArray[{Dimensions[State]->0}]];
   Steps  = Dimensions[State][[1]];

   For[i=1,i<=Steps,i++,
     oState[[i]]={
       State[[i]].Coin[[1]],
       State[[Mod[i-1,Steps,1]]].Coin[[2]],
       State[[Mod[i+1,Steps,1]]].Coin[[3]]
     };
   ];
   Return[oState];
]

StatesToPositionalProbabilites[State0_] := Module[{State=State0},
   Return[Simplify[Total[State*Conjugate[State],{2}]]]];

CumulativeMean[List0_] := Module[{List = List0}, 
   Return[Accumulate[List]/Range[Length[List]]]];

MultipleLazyStepsRecursive[State0_,Coin0_,Steps0_] := Module[
{State = State0, Coin = Coin0, Steps = Steps0},
  Return[Simplify[
    Nest[
      (SingleLazyItteration[#,Coin])&,
      State,Steps]]]]

MultipleLazySteps[State0_,Coin0_,Steps0_] := Module[
{State = State0, Coin = Coin0, Steps = Steps0},
  inState = State;
  For[Step=1,Step<=Steps,Step++,
    outState = Simplify[
      SingleLazyItteration[inState, Coin]];
    inState = outState;];
  Return[outState];
]

MultipleLazyStepsHistory[State0_,Coin0_,Steps0_] := Module[
{State = State0, Coin = Coin0, Steps = Steps0},
  returnHistory = List[];
  inState = State;
  For[Step=1,Step<=Steps,Step++,
    outState = Simplify[
      SingleLazyItteration[inState, Coin]];
    AppendTo[returnHistory, outState];
    inState = outState;];
  Return[returnHistory];
]

End[];
EndPackage[];
