(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["FFI`"];


DefineFamily::usage = "DefineFamily[name, prop, isp, propmom, loop, leg, nullmom, replace] defines a family."
FamilyInfo::usage = "FamilyInfo[name] gives the information of a family."


Begin["`Private`"]


(* ::Section:: *)
(*Family Functions*)


(* ::Subsection:: *)
(*DefineFamily*)


Options[DefineFamily] = {"SquareProp" -> True};

DefineFamily[name_, prop_, isp_, propmom_, loop_, leg_, nullmom_, replace_, OptionsPattern[]]:=
Module[
	{},
	name["Prop"] = If[OptionValue["SquareProp"],
						SquareProp[#, loop, replace]&/@prop, 
						prop];
	name["Isp"] = isp;
	name["PropMom"] = propmom;
	name["Loop"] = loop;
	name["Leg"] = leg;
	name["NullMom"] = nullmom;
	name["Replace"] = replace;
	
	FamilyQ[name] = True;
	Protect[name];
	
	Switch[FFI`$PrintLevel,
		0,
		Print[name, ": "];
		Print["Prop: ", name["Prop"]];
		Print["Isp: ", isp];
		Print["PropMom: ", propmom];
		Print["Loop: ", loop];
		Print["Leg: ", leg];
		Print["NullMom: ", nullmom];
		Print["Replace: ", replace];,
		_,
		Return[];
	];
]


(* ::Subsection::Closed:: *)
(*FamilyInfo*)


FamilyInfo[name_?FamilyQ]:= 
{
	"Prop" -> name["Prop"],
	"Isp" -> name["Isp"],
	"PropMom" -> name["PropMom"],
	"Loop" -> name["Loop"],
	"Leg" -> name["Leg"],
	"NullMom" -> name["NullMom"],
	"Replace" -> name["Replace"]
};


(* ::Section:: *)
(*End*)


End[]


EndPackage[];
