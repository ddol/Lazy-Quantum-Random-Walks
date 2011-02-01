(* ::Package:: *)

(* ::Input:: *)
(**)


BeginPackage["lqrw`"];
SingleLazyItteration::useage = "SingleLazyItteration[State,Coin] returns the\
state after it has been randomised by one itteration of a given three state coin."

CumulativeMean::usage = "CumulativeMean[List] returns a list where each \
value is the cumilative mean of all previous list values."; 

StatesToPositionalProbabilites::useage = "StatesToPositionalProbabilites[State]\
returns the position probabilites from a matrix of states.";


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


End[];
EndPackage[];
