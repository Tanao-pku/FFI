(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["FFI`"];


AsyRegion::usage = "AsyRegion[family] computes the IR regions of family using Asy2.m"


Begin["`Private`"]


(* ::Section:: *)
(*SingularAPI*)


(* ::Subsection:: *)
(*AsyRegion*)


AsyRegion[family_?FamilyQ]:= Module[
	{rule, dir, mma = "wolfram", time, asytemplate},
	rule = <|"loop" -> ToString[InputForm[family["Loop"]]], "prop" -> ToString[InputForm[family["Prop"] - Global`y]], 
	    "replace" -> ToString[InputForm[family["Replace"]]], "asy" -> $AsyPath|>;
	dir = FileNameJoin[{NotebookDirectory[], "cache", ToString[family], "Region"}];
	
	(*AsyTemplate*)
	asytemplate = StringJoin[Riffle[
	{
		"If[$FrontEnd===Null,$InputFileName,NotebookFileName[]]//DirectoryName//SetDirectory;",
		"Get[\"`asy`\"];",
		"regions = AlphaRepExpand[`loop`, `prop`, `replace`, {y->x}];",
		"Put[regions, \"regions\"];",
		"Quit[]"
	},
	"\n"
	]];
	
	If[!DirectoryQ[dir], CreateDirectory[dir]];
	
	FileTemplateApply[asytemplate, rule, FileNameJoin[{dir, "region.wl"}]];
	
	Print["Using Asy to compute regions..."];
	(*There cannot be space in args*)
	time = AbsoluteTiming[RunProcess[{mma ,"-noprompt", "-script", FileNameJoin[{dir, "region.wl"}]}]][[1]];
	Print["Time used: ", time, "s"];
	
	Return[Get[FileNameJoin[{dir, "regions"}]]];
]


(* ::Section:: *)
(*End*)


End[];


EndPackage[];
