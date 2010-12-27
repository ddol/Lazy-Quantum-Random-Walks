(* ::Package:: *)

(* Quantum`Notation` 
   version 1.4 July 7, 2007
		Add palette for version 5.2 and palette for version 6.0
		Improve documentation with installation instructions
		The conjugate of a Quantum object becomes the hermitian 
   Version 1.3 June 19, 2007
   		Set error message in the case of normal (commutative)
   		multiplication of an operator times a ket or a bra.
   		The palette only opens if the version is less than 6
   Version 1.2 June 12, 2007
        Replaced SelectedNotebook by InputNotebook in SetQuantumAliases[]
   Version 1.1 June 6, 2007
   		Corrected the inner product of larger bras and kets
   		based on the work of Francisco Delgado in
   		multichannel quantum-computing gates
   Version 1.0 June 1, 2007
   		The first version that runs the first two examples
   		of the Quantum Random Walk as well as totally-symbolic
   		calculations
   
   Copyright 2007
   Jose Luis Gomez-Munoz
   ITESM-CEM, 
   Departamento de Ciencias Básicas (Matemáticas)
   Carretera Lago de Guadalupe Km. 3.5, 
   Atizapán de Zaragoza, Estado de México, 
   C.P. 52926
   Mexico
   jose.luis.gomez@itesm.mx
   http://homepage.cem.itesm.mx/lgomez/
   
   A package for Dirac Notation in Mathematica.
   This package may be freely redistributed under the condition that the   
   copyright notices (including this entire header) are not removed
   and no compensation is received.  Private, research, and institutional     
   use is free.  You may distribute modified versions of this code UNDER  
   THE CONDITION THAT THIS CODE AND ANY MODIFICATIONS MADE TO IT IN THE   
   SAME FILE REMAIN UNDER COPYRIGHT OF THE ORIGINAL AUTHOR, SOURCE
   CODE IS MADE FREELY AVAILABLE WITHOUT CHARGE, AND CLEAR    
   NOTICE IS GIVEN OF THE MODIFICATIONS.  Distribution of this code as
   part of a commercial system is permissible ONLY BY DIRECT ARRANGEMENT
   WITH THE AUTHOR.  (If you are not directly supplying this code to a
   customer, and you are instead telling them how they can obtain it for
   free, then you are not required to make any arrangement with me.)
   
   This package does NOT use the Jason Harris' Notation Add-on of Mathematica.
   
*)

(*Author: Jose Luis Gomez-Munoz*)

BeginPackage[ "Quantum`Notation`"] 

jlgmQuantumNotationVersion::start=
					"version 1.4 July 7, 2007";
SetQuantumAliases::start=
					"\nFor large calculations set $RecursionLimit=Infinity\n"<>
					"\nExecute SetQuantumAliases[] in order to be able"<>
					" to use the keytboard to enter quantum objects"<>
					" in Dirac's notation\n"<>
					"SetQuantumAliases[] must be executed again in"<>
					" each notebook that is created";
SetQuantumAliases::aliases=
					"\n[ESC]ket[ESC] ket template\n"<>
					"[ESC]bra[ESC] bra template\n"<>
					"[ESC]braket[ESC] braket template\n"<>
					"[ESC]op[ESC] operator template\n"<>
					"[ESC]on[ESC] quantum product template"<>
					" (operator application, inner product and outer product)\n"<>
					"[ESC]eket[ESC] eigenstate template\n"<>
					"[ESC]eeket[ESC] two-operators-eigenstate template\n"<>
					"[ESC]eeeket[ESC] three-operators-eigentstate template\n"<>
					"[ESC]ebra[ESC] bra of eigenstate template\n"<>
					"[ESC]eebra[ESC] bra of two-operators-eigenstate template\n"<>
					"[ESC]eeebra[ESC] bra of three-operators-eigentstate"<>
					" template\n"<>
					"[ESC]ebraket[ESC] braket of eigenstates template\n"<>
					"[ESC]eebraket[ESC] braket of two-operators-eigenstates"<>
					" template\n"<>
					"[ESC]eeebraket[ESC] braket of three-operators-eigentstate"<>
					" template\n"<>
					"[ESC]her[ESC] hermitian conjugate template\n"<>
					"[ESC]con[ESC] complex conjugate template\n\n"<>
					"SetQuantumAliases[] must be executed again in"<>
					" each notebook that is created\n";
SetQuantumAliases::usage=
					"SetQuantumAliases[] sets keyboard aliases in the"<>
					" selected notebook. SetQuantumAliases[notebook]"<>
					" sets keyboard aliases in the specified notebook."<>
					" These quantum aliases are keyboard key-combinations"<>
					" for the input of quantum objects in Dirac Notation.\n"<>
					(SetQuantumAliases::aliases);
jlgmEigenstate::uket=	
					"jlgmKet[jlgmEigenstate[jlgmOperator[H],v]] "<>
					"represents an eigenstate"<>
					" of operator H with eigenvalue v."<>
					"\nIf SetQuantumAliases[] has been executed"<>
					" in the notebook then"<>
					" eigenkets can be entered by pressing the"<>
					" keyboard keys [ESC]eket[ESC] or [ESC]eeket[ESC]";
jlgmEigenstate::ubra=	
					"jlgmBra[jlgmEigenstate[jlgmOperator[H],v]]"<>
					" represents the dual"<>
					" of an eigenstate"<>
					" of operator H with eigenvalue v."<>
					"\nIf SetQuantumAliases[] has been executed"<>
					" in the notebook then"<>
					" eigenbras can be entered by pressing the"<>
					" keyboard keys [ESC]ebra[ESC] or [ESC]eebra[ESC]";
jlgmEigenstate::usage=	
					(jlgmEigenstate::uket)<>"\n"<>(jlgmEigenstate::ubra);
Subscript::usage=	"Subscript is used for fomating of jlgmEigenstate[]\n"<>
					(jlgmEigenstate::usage);
jlgmKet::usage=		"jlgmKet[a] is the internal representation of quantum state a."<>
					"\nIf SetQuantumAliases[] has been executed"<>
					" in the notebook then"<>
					" jlgmKet[] can be entered in standard form by pressing the"<>
					" keyboard keys [ESC]ket[ESC]\n"<>(jlgmEigenstate::uket);
jlgmBra::usage=		"jlgmBra[a] is the internal representation of the dual of"<>
					" quantum state a."<>
					"\nIf SetQuantumAliases[] has been executed"<>
					" in the notebook then"<>
					" jlgmBra[] can be entered in standard form by pressing the"<>
					" keyboard keys [ESC]bra[ESC]\n"<>(jlgmEigenstate::ubra);					
jlgmBraKet::usage=		
					"jlgmBraKet[{a},{b}] is the internal representation"<>
					" of the inner product of jlgmBra[a] and jlgmKet[b]."<>
					"\nIf SetQuantumAliases[] has been executed"<>
					" in the notebook then"<>
					" jlgmBraKet[] can be entered in standard form by pressing the"<>
					" keyboard keys [ESC]braket[ESC]";
jlgmOperator::usage=	
					"jlgmOperator[H] is the internal representation of operator H."<>
					"\nIf SetQuantumAliases[] has been executed"<>
					" in the notebook then"<>
					" jlgmOperator[] can be entered in standard form"<>
					" by pressing the"<>
					" keyboard keys [ESC]op[ESC]";
OverHat::usage=		"OverHat is used for formating of jlgmOperator[]\n"<>
					(jlgmOperator::usage);
jlgmKetArgs::usage= 	"jlgmKetArgs is used for formating of Quantum objects";
jlgmBraArgs::usage= 	"jlgmBraArgs is used for formating of Quantum objects";
jlgmNonCommutativeTimes::usage=
					"jlgmNonCommutativeTimes is the internal representation"<>
					" of products of quantum objects."<>
					"\nIf SetQuantumAliases[] has been executed"<>
					" in the notebook, then"<>
					" jlgmNonCommutativeTimes can be entered in standard form"<>
					" by pressing the"<>
					" keyboard keys [ESC]on[ESC]";
jlgmHermitianConjugate::usage=
					"jlgmHermitianConjugate[qobj] gives the Hermitian conjugate"<>
					" of qobj, which must be a linear combination of bras,"<>
					" kets, operands and"<>
					" complex numbers."<>
					"\nIf SetQuantumAliases[] has been executed"<>
					" in the notebook then"<>
					" jlgmHermitianConjugate[] can be entered pressing"<>
					" [ESC]her[ESC]"; 
SuperDagger::usage=	"SuperDagger is used for formating of jlgmHermitianConjugate\n"<>
					(jlgmHermitianConjugate::usage);				
SuperStar::usage=	"SuperStar is used for formating of Conjugate\n"<>
					(Conjugate::usage);						
CenterDot::usage=	"CenterDot is used for formating of jlgmNonCommutativeTimes\n"<>
					(jlgmNonCommutativeTimes::usage);
Times::ketket = 	"ERROR: The commutative multiplication"<>
					" (asterisk * or [SPACE]) between"<>
					" quantum objects is undefined."<>
					"\nIf SetQuantumAliases[] has been executed"<>
					" in the notebook, then"<>
					" the tensor product or the application"<>
					" of an operator to a quantum object"<>
					" can be entered"<>
					" by pressing the keys [ESC]on[ESC] (it will be shown as"<>
					" the infix operator \[CenterDot])";
jlgmEigenstate::ketrepop =
					"ERROR: Each operator must appear only one time in ket or bra";

Begin["`Private`"]

Unprotect[RowBox,jlgmNonCommutativeTimes,CenterDot,SuperStar,
			SuperDagger,OverHat,Subscript,Conjugate,
			jlgmQuantumNotationVersion];

Clear[CenterDot,SuperStar,
		SuperDagger,OverHat]

(* Clear in case symbols already exist in the Global context*)
Clear[jlgmBra, jlgmBraArgs, jlgmBraKet, jlgmEigenstate, jlgmHermitianConjugate, 
		jlgmKet, jlgmKetArgs, jlgmNonCommutativeTimes, jlgmOperator, 
		SetQuantumAliase];

(* keyboard aliases [ESC]alias[ESC]*)

SetQuantumAliases[]:=
Module[{nb},
	nb:=InputNotebook[];
	SetQuantumAliases[nb]
];

SetQuantumAliases[doc_NotebookObject]:=
Module[{},
	Message[SetQuantumAliases::aliases];
	SetOptions[doc, 
	InputAliases -> {
		"on"  -> 	"\[CenterDot]",
		"op"  -> 	OverscriptBox["\[Placeholder]", "^"],
		"her"  -> 	SuperscriptBox[
						RowBox[{
							"(","\[Placeholder]",")"}], "\[Dagger]" ],		
		"con"  -> 	SuperscriptBox[
						RowBox[{
							"(","\[Placeholder]",")"}], "*" ],		
		"ket" -> 	TagBox[RowBox[{
							"\[VerticalSeparator]",
							TagBox["\[Placeholder]",
							jlgmKetArgs,Editable->True,Selectable->True],
							"\[RightAngleBracket]" }],
						jlgmKet,Editable->False,Selectable->False],
		"eket" -> 	TagBox[RowBox[{
							"\[VerticalSeparator]",
							TagBox[RowBox[{		
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]]}],
							jlgmKetArgs,Editable->True,Selectable->True],
							"\[RightAngleBracket]" }],
						jlgmKet,Editable->False,Selectable->False],
		"eeket" -> 	TagBox[RowBox[{
							"\[VerticalSeparator]",
							TagBox[RowBox[{
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]],
								",",							
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]]}],
							jlgmKetArgs,Editable->True,Selectable->True],
							"\[RightAngleBracket]" }],
						jlgmKet,Editable->False,Selectable->False],
		"eeeket" -> TagBox[RowBox[{
							"\[VerticalSeparator]",
							TagBox[RowBox[{
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]],
								",",
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]],
								",",							
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]]}],
							jlgmKetArgs,Editable->True,Selectable->True],
							"\[RightAngleBracket]" }],
						jlgmKet,Editable->False,Selectable->False],
		"braket" ->	TagBox[RowBox[{
							"\[LeftAngleBracket]",
							TagBox["\[Placeholder]",jlgmBraArgs,
							Editable->True,Selectable->True],
							"\[VerticalSeparator]", 
							TagBox["\[Placeholder]",jlgmKetArgs,
							Editable->True,Selectable->True],
							"\[RightAngleBracket]"
							}],
						jlgmBraKet,Editable->False,Selectable->False],
		"ebraket" ->	TagBox[RowBox[{
							"\[LeftAngleBracket]",
							TagBox[RowBox[{		
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]]}],
							jlgmBraArgs,Editable->True,Selectable->True],
							"\[VerticalSeparator]", 
							TagBox[RowBox[{		
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]]}],
							jlgmKetArgs,Editable->True,Selectable->True],
							"\[RightAngleBracket]"
							}],
						jlgmBraKet,Editable->False,Selectable->False],						
		"eebraket" ->	TagBox[RowBox[{
							"\[LeftAngleBracket]",
							TagBox[RowBox[{
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]],
								",",							
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]]}],
							jlgmBraArgs,Editable->True,Selectable->True],
							"\[VerticalSeparator]", 
							TagBox[RowBox[{
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]],
								",",							
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]]}],
							jlgmKetArgs,Editable->True,Selectable->True],
							"\[RightAngleBracket]"
							}],
						jlgmBraKet,Editable->False,Selectable->False],						
		"eeebraket" ->	TagBox[RowBox[{
							"\[LeftAngleBracket]",
							TagBox[RowBox[{
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]],
								",",
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]],
								",",							
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]]}],
							jlgmBraArgs,Editable->True,Selectable->True],
							"\[VerticalSeparator]", 
							TagBox[RowBox[{
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]],
								",",
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]],
								",",							
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]]}],
							jlgmKetArgs,Editable->True,Selectable->True],
							"\[RightAngleBracket]"
							}],
						jlgmBraKet,Editable->False,Selectable->False],												
		"bra" -> 	TagBox[RowBox[{
							"\[LeftAngleBracket]",
							TagBox["\[Placeholder]",jlgmBraArgs,
							Editable->True,Selectable->True],
							"\[VerticalSeparator]" }],
						jlgmBra,Editable->False,Selectable->False],
		"ebra" -> 	TagBox[RowBox[{
							"\[LeftAngleBracket]",
							TagBox[RowBox[{		
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]]}],
							jlgmBraArgs,Editable->True,Selectable->True],
							"\[VerticalSeparator]" }],
						jlgmBra,Editable->False,Selectable->False],
		"eebra" -> 	TagBox[RowBox[{
							"\[LeftAngleBracket]",
							TagBox[RowBox[{
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]],
								",",							
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]]}],
							jlgmBraArgs,Editable->True,Selectable->True],
							"\[VerticalSeparator]" }],
						jlgmBra,Editable->False,Selectable->False],
		"eeebra" -> TagBox[RowBox[{
							"\[LeftAngleBracket]",
							TagBox[RowBox[{
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]],
								",",
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]],
								",",							
								SubscriptBox["\[Placeholder]",
									OverscriptBox["\[Placeholder]", "^"]]}],
							jlgmBraArgs,Editable->True,Selectable->True],
							"\[VerticalSeparator]" }],
						jlgmBra,Editable->False,Selectable->False]			
}]];

(*Correct parenthesis*)

RowBox[{"(", TagBox[arg_, jlgmKet, opts___], ")"}] := 
	TagBox[arg, jlgmKet, opts];
RowBox[{"(", TagBox[arg_, jlgmBra, opts___], ")"}] := 
	TagBox[arg, jlgmBra, opts];
RowBox[{"(", TagBox[arg_, jlgmBraKet, opts___], ")"}] := 
	TagBox[arg, jlgmBraKet, opts];

(*Output and input of ket*)

jlgmKet /: MakeBoxes[jlgmKet[x__], form_] := 
	TagBox[	RowBox[{
				"\[VerticalSeparator]",
					TagBox[RowBox[
						Drop[Flatten[Map[{",",MakeBoxes[#, form]}&, {x}]],1]],
						jlgmKetArgs,Editable->True,Selectable->True],
				"\[RightAngleBracket]" }],
			jlgmKet,Editable->False,Selectable->False];

MakeExpression[
	TagBox[	RowBox[{
				"\[VerticalSeparator]",
				TagBox[x__,jlgmKetArgs,opts1___],
				"\[RightAngleBracket]" }],
			jlgmKet,opts2___], form_] := 
	MakeExpression[RowBox[{"jlgmKet", "[", x, "]"}], form]

(*Output and input of bra*)

jlgmBra /: MakeBoxes[jlgmBra[x__], form_] := 

	TagBox[	RowBox[{
				"\[LeftAngleBracket]",
				TagBox[RowBox[
						Drop[Flatten[Map[{",",MakeBoxes[#, form]}&, {x}]],
							1]],
					jlgmBraArgs,Editable->True,Selectable->True],
				 "\[VerticalSeparator]"}],
			jlgmBra,Editable->False,Selectable->False];

MakeExpression[
	TagBox[	RowBox[{
				"\[LeftAngleBracket]",
				TagBox[x__,jlgmBraArgs,opts1___],
				"\[VerticalSeparator]" }],
			jlgmBra,opts2___], form_] := 
	MakeExpression[RowBox[{"jlgmBra", "[", x, "]"}], form];

(*Output and input of braket*)

jlgmBraKet /: MakeBoxes[jlgmBraKet[{x___},{y___}], form_] := 
	TagBox[	RowBox[{
				"\[LeftAngleBracket]",
				TagBox[RowBox[
						Drop[Flatten[Map[{",",MakeBoxes[#, form]}&, {x}]],
							1]],
					jlgmBraArgs,Editable->True,Selectable->True],
				 "\[VerticalSeparator]",
				 TagBox[RowBox[
						Drop[Flatten[Map[{",",MakeBoxes[#, form]}&, {y}]],
							1]],
					jlgmKetArgs,Editable->True,Selectable->True],
				"\[RightAngleBracket]"
				 }],
			jlgmBraKet,Editable->False,Selectable->False];

MakeExpression[
	TagBox[	RowBox[{
				"\[LeftAngleBracket]",
				TagBox[x__,jlgmBraArgs,optsbra___],
				"\[VerticalSeparator]",
				TagBox[y__,jlgmKetArgs,optsket___],
				"\[RightAngleBracket]"
			}],
			jlgmBraKet,optsbraket___], form_] := 
	MakeExpression[
		RowBox[{
			"jlgmBraKet", 
			"[", 	
				RowBox[{
					RowBox[{"{", x, "}"}], ",",
					RowBox[{"{", y, "}"}]
				}], 
			"]"    
    	}], 
    form];

(*Output and input of jlgmNonCommutativeTimes*)
(*IMPORTANT: The internal representation jlgmNonCommutativeTimes has
the arguments in "Reverse" order, as compared with the external representation.
This improves evaluation speed by one or two orders of magnitude,
because it avoids that operators grow too large before being applied
to a ket*)

jlgmNonCommutativeTimes /: MakeBoxes[jlgmNonCommutativeTimes[x__], form_] := 
	RowBox[
		Drop[Flatten[Map[{"\[CenterDot]",MakeBoxes[#, form]}&, 
			Reverse[{x}]
		]],1]];

CenterDot[x___] := 
	jlgmNonCommutativeTimes[Apply[Sequence,Reverse[{x}]]];

(*Output and input of operator*)

jlgmOperator /: MakeBoxes[jlgmOperator[x_], form_] := 
	OverscriptBox[MakeBoxes[x,form], "^"];

OverHat[x_] := jlgmOperator[x];

(*Output and input of jlgmHermitianConjugate*)

jlgmHermitianConjugate /: Format[jlgmHermitianConjugate[x_]] :=
	PrecedenceForm[SuperDagger[x],1];

MakeExpression[SuperscriptBox[x_, "\[Dagger]"], form_] := 
	MakeExpression[RowBox[{"jlgmHermitianConjugate", "[", x, "]"}], form];

(*Output and input of Conjugate*)

Conjugate /: Format[Conjugate[x_]] :=
	PrecedenceForm[SuperStar[x],1];

MakeExpression[SuperscriptBox[x_, "*"], form_] := 
	MakeExpression[RowBox[{"Conjugate", "[", x, "]"}], form];

(*Output and input of eigenstate*)

jlgmEigenstate /: MakeBoxes[jlgmEigenstate[jlgmOperator[op_],ev_], form_] := 
	SubscriptBox[MakeBoxes[ev,form],MakeBoxes[jlgmOperator[op],form]];

Subscript[ev_, jlgmOperator[op_]] := jlgmEigenstate[jlgmOperator[op],ev] ;

(*Calculation rules*)

(* Hermitian conjugation rules *)		

jlgmKet /: Conjugate[jlgmKet[a__]] := 
		jlgmHermitianConjugate[jlgmKet[a]];

jlgmBra /: Conjugate[jlgmBra[a__]] := 
		jlgmHermitianConjugate[jlgmBra[a]];

jlgmBraKet /: Conjugate[jlgmBraKet[a__]] := 
		jlgmHermitianConjugate[jlgmBraKet[a]];

jlgmHermitianConjugate[jlgmNonCommutativeTimes[a__]] := 
	jlgmNonCommutativeTimes[Apply[Sequence, 
	                              Reverse[Map[jlgmHermitianConjugate, {a}]]]];
jlgmHermitianConjugate[a_ + b_] :=
	jlgmHermitianConjugate[a] + jlgmHermitianConjugate[b];
jlgmHermitianConjugate[jlgmKet[a__]] :=
	jlgmBra[a];
jlgmHermitianConjugate[jlgmBra[a__]] :=
	jlgmKet[a];
jlgmHermitianConjugate[jlgmBraKet[a__, b__]]  :=
	jlgmBraKet[b, a];
jlgmHermitianConjugate[a_?ConstQ * b_] :=
	Conjugate[a]*jlgmHermitianConjugate[b];		
jlgmHermitianConjugate[a_?ConstQ] :=
	Conjugate[a];		
 
(*ket, jlgmBra, braket and eigenstate properties and rules*)

SetAttributes[jlgmKet,Orderless];
SetAttributes[jlgmBra,Orderless];

jlgmKet[command_[args1___,jlgmEigenstate[jlgmOperator[op_],val_],args2___],
	other___] := 
	jlgmKet[jlgmEigenstate[jlgmOperator[op],command[args1,val,args2]],other];

jlgmBra[command_[args1___,jlgmEigenstate[jlgmOperator[op_],val_],args2___],
	other___] := 
	jlgmBra[jlgmEigenstate[jlgmOperator[op],command[args1,val,args2]],other];

jlgmKet[jlgmEigenstate[jlgmOperator[opSame_], evket1_],
		jlgmEigenstate[jlgmOperator[opSame_], evket2_],
        rest___]:= 
	Module[
      {},
      Message[jlgmEigenstate::ketrepop];
      $Failed
	];

jlgmBra[jlgmEigenstate[jlgmOperator[opSame_], evket1_],
		jlgmEigenstate[jlgmOperator[opSame_], evket2_],
        rest___]:= 
	Module[
      {},
      Message[jlgmEigenstate::ketrepop];
      $Failed
	];

jlgmBraKet[{a___},{b___}] := jlgmNonCommutativeTimes[jlgmKet[b],jlgmBra[a]];

(*jlgmNonCommutativeTimes rules*)
(*IMPORTANT: The internal representation jlgmNonCommutativeTimes has
the arguments in "Reverse" order, as compared with the external representation.
This improves evaluation speed by one or two orders of magnitude,
because it avoids that operators grow too large before being applied
to a ket*)

jlgmNonCommutativeTimes[] := 1;

SetAttributes[jlgmNonCommutativeTimes,{Flat,OneIdentity}];

(* Operator acting on its eigenket*)
(* Remember that operands order is reversed in internal representation*)
jlgmNonCommutativeTimes[
		after___,
		jlgmKet[jlgmEigenstate[jlgmOperator[op_], ev_],restket___], 
		jlgmOperator[op_],
		before___] :=
	ev*jlgmNonCommutativeTimes[
				after,
				jlgmKet[jlgmEigenstate[jlgmOperator[op], ev],restket],
				before];

(* Eigenstates of the same operator are orthonormal *)
(* Remember that operands order is reversed in internal representation*)
jlgmNonCommutativeTimes[
		expr2___,
		jlgmKet[jlgmEigenstate[jlgmOperator[opSame_], evket_],restket___],
		jlgmBra[jlgmEigenstate[jlgmOperator[opSame_], evbra_],restbra___],
		expr1___
		] := 
	KroneckerDelta[evbra-evket]*
	jlgmNonCommutativeTimes[
		expr2,
		jlgmKet[restket],
		jlgmBra[restbra],
		expr1
		];

(* This completes the orthonormality, see previous definiton*)
jlgmNonCommutativeTimes[
		expr2___,
		jlgmKet[],
		expr1___
		] := 
	jlgmNonCommutativeTimes[
		expr2,
		expr1
		];

(* This completes the orthonormality, see previous definitons*)
jlgmNonCommutativeTimes[
		expr2___,
		jlgmBra[],
		expr1___
		] := 
	jlgmNonCommutativeTimes[
		expr2,
		expr1
		];

(* Eigenstates of different operators live in different subspaces*)
(* in other words, their operators are assumed to commute*)
(* Remember that operands order is reversed in internal representation*)
jlgmNonCommutativeTimes[
		jlgmKet[jlgmEigenstate[jlgmOperator[opket_], evket_]],
		jlgmBra[jlgmEigenstate[jlgmOperator[opbra_], evbra_]]
		] := 
	jlgmNonCommutativeTimes[
		jlgmBra[jlgmEigenstate[jlgmOperator[opbra], evbra]],
		jlgmKet[jlgmEigenstate[jlgmOperator[opket], evket]]
		] /; (opket =!= opbra);

jlgmNonCommutativeTimes[jlgmKet[ketcontent__jlgmEigenstate], 
  jlgmBra[bracontent__jlgmEigenstate]] :=
 Module[{},
  ketcontentlist = List[ketcontent];
  bracontentlist = List[bracontent];
  ketlist = Map[jlgmKet, ketcontentlist];
  bralist = Map[jlgmBra, bracontentlist];
  alllist = Join[ketlist, bralist];
  Apply[jlgmNonCommutativeTimes, alllist]
  ];


(* Tensor product of kets. No checking is done*)
jlgmNonCommutativeTimes[jlgmKet[e1__],jlgmKet[e2__]] :=  jlgmKet[e1,e2];

(* Tensor product of bras. No checking is done*)
jlgmNonCommutativeTimes[jlgmBra[e1__],jlgmBra[e2__]] :=  jlgmBra[e1,e2];

(* Linearity 1*)
jlgmNonCommutativeTimes[r___, (a_ + b_), l___] := 
		jlgmNonCommutativeTimes[r, a, l] + jlgmNonCommutativeTimes[r, b, l];

(* Linearity 2*)  
jlgmNonCommutativeTimes[r___, (a_?ConstQ *b_), l___] := 
		a*jlgmNonCommutativeTimes[r, b, l] /; ({l, r} =!= {});

(* Linearity 3*)  
jlgmNonCommutativeTimes[a_?ConstQ, before__] := 
		a*jlgmNonCommutativeTimes[before];

(* Linearity 4*)  
jlgmNonCommutativeTimes[after__,a_?ConstQ] := 
		a*jlgmNonCommutativeTimes[after];

(* "Normal" commutative multiplication of quantum objects is undefined*)
jlgmKet /: Times[jlgmOperator[a__], jlgmKet[b__]] := Module[
      {},
      Message[Times::ketket];
      $Failed
      ];

(* "Normal" commutative multiplication of quantum objects is undefined*)
jlgmBra /: Times[jlgmOperator[a__], jlgmBra[b__]] := Module[
      {},
      Message[Times::ketket];
      $Failed
      ];

(* "Normal" commutative multiplication of quantum objects is undefined*)
jlgmKet /: Times[jlgmKet[a__], jlgmKet[b__]] := Module[
      {},
      Message[Times::ketket];
      $Failed
      ];

(* "Normal" commutative multiplication of quantum objects is undefined*)
jlgmKet /: Times[jlgmKet[a__], jlgmBra[b__]] := Module[
      {},
      Message[Times::ketket];
      $Failed
      ];

(* "Normal" commutative multiplication of quantum objects is undefined*)
jlgmBra /: Times[jlgmBra[a__], jlgmBra[b__]] := Module[
      {},
      Message[Times::ketket];
      $Failed
      ];

(* "Normal" commutative multiplication of quantum objects is undefined*)      
jlgmBra /: Times[jlgmBra[a__], jlgmKet[b__]] := Module[
      {},
      Message[Times::ketket];
      $Failed
      ];

(* Symbols are asumed to represent complex constants*)

  ConstQ[t_Times] := And @@ ConstQ /@ List @@ t;
  ConstQ[p_Plus] := And @@ ConstQ /@ List @@ p;
  ConstQ[a_^n_] := And[ConstQ[a],ConstQ[n]];
  ConstQ[Conjugate[a_]]:=ConstQ[a];
  ConstQ[a_?NumberQ] := True;
  ConstQ[a_Symbol] := True;
  ConstQ[_] := False;

SetOptions[SelectedNotebook[], 
			AutoStyleOptions -> {"UnmatchedBracketStyle" -> None}];

jlgmQuantumNotationVersion=jlgmQuantumNotationVersion::start;

Protect[jlgmNonCommutativeTimes,CenterDot,SuperDagger,SuperStar,
		jlgmKetArgs,jlgmBraArgs, jlgmHermitianConjugate, 
		jlgmBra,  jlgmBraKet,  jlgmEigenstate, 
		jlgmKet,  jlgmOperator, OverHat, Conjugate, RowBox, 
		SetQuantumAliases, jlgmQuantumNotationVersion
	 (*,Subscript*)];

(* Open palette *)
If[	$VersionNumber<6.0,
	NotebookOpen["QuantumNotationPaletteV52.nb"],
	NotebookOpen["QuantumNotationPaletteV60.nb"]
];
Message[jlgmQuantumNotationVersion::start];
Message[SetQuantumAliases::start];	

End[]

EndPackage[]
              

