(* ::Package:: *)

BeginPackage["FFI`"];


(*RegionIdeal::usage = "Run Singular to calculate the ideal of a region";
RunSingular::usage = "Run the file 'singin' with Singular and return the result in the file 'singout'";
RunSingularFile::usage = "Run a file with Singular program.";
SingularDefineIdeal::usage = "Generate the code of defining an ideal in Singular";
SingularDefineRing::usage = "Generate the code of defining a ring in Singular";
SingularEliminate::usage = "Generate the code of using 'eliminate' function in Singular";
SingularGroebner::usage = "Generate the code of using 'groebner' function in Singular";
SingularIntersect::usage = "Generate the code of using 'intersect' function in Singular";
StringToIdeal::usage = "Turn the string result of Singular into an ideal expression in Mathematica";
ToSingularExpression::usage = "Turn a Mathematica expression into a Singular expression";
WriteSingularCode::usage = "Write the code into file 'singin'";*)


Begin["`Private`"]


(* ::Section:: *)
(*Run Singular Program*)


(* ::Subsection:: *)
(*RunSingular*)


RunSingular[]:=Module[
	{},
	RunSingularFile["singin"];
	Return[ReadString["singout"]];
];


(* ::Subsection:: *)
(*RunSingularFile*)


RunSingularFile[file_String]:= If[$OperatingSystem == "Windows", Run["wsl Singular " <> file], RunProcess[{Global`$SingularPath, file}]];
(*
For windows users:
If the Singular is installed in WSL, the code should be modified to 'Run["wsl Singular "<>file]'
*)


(* ::Subsection:: *)
(*WriteSingularCode*)


WriteSingularCode[code_String]:=Module[
	{file},
	file=OpenWrite["singin"];
	WriteString[file,code];
	Close[file];
];
WriteSingularCode[code__String]:=WriteSingularCode[StringJoin[code]];


(* ::Section:: *)
(*Generate Singular Code*)


(* ::Subsection:: *)
(*ToSingularExpression*)


(*turn Mathematica expression into Singular expression*)
ToSingularExpression[exp_]:=StringReplace[ToString[exp,InputForm],{"["->"(","]"->")","\""->""}];


(* ::Subsection:: *)
(*SingularDefineRing*)


(*generate the code of defining a ring in Singular*)
(*e.g.: SingularDefineRing[x,y,{z,5}], output: "ring R=0,(x,y,z(1..5)),dp;\n"*)
Options[SingularDefineRing] = {"Parameters" -> {}, "Weight" -> None};

SingularDefineRing[vars__, OptionsPattern[]]:=Module[
	{str, varstr="", ordstr="dp", parastr="0"},
	str="ring R=(`1`),(`2`),`3`;";
	
	(*deal with option Weight*)
	If[OptionValue["Weight"] =!= None, 
		ordstr = "wp"<>StringReplace[ToString[OptionValue["Weight"]], {"{" -> "(", "}" -> ")"}]
	];
	
	Do[
		parastr = parastr <> "," <> ToString[OptionValue["Parameters"][[i]]],
		{i, 1, Length[OptionValue["Parameters"]]}
	];
	Do[
		If[Head[{vars}[[i]]]===List,
			varstr=varstr<>TemplateApply["`1`(1..`2`)",{{vars}[[i]][[1]],{vars}[[i]][[2]]}]<>",",
			varstr=varstr<>ToString[{vars}[[i]]]<>","
		],
		{i, 1, Length[{vars}]}
	];
	varstr=StringTake[varstr,StringLength[varstr]-1];
	Return[TemplateApply[str, {parastr, varstr, ordstr}]<>"\n"];
];


(* ::Subsection:: *)
(*SingularDefineIdeal*)


(*generate the code of defining an ideal in Singular*)
(*e.g.: SingularDefineIdeal[i,{z[1]z[2],z[3]-z[1]}], output: "ideal i = z(1)*z(2),-z(1) + z(3);\n"*)
SingularDefineIdeal[name_,polys_List]:=Module[
	{str,polystr=""},
	str="ideal `1` = `2`;\n";
	Do[
		polystr=polystr<>ToSingularExpression[polys[[i]]]<>",",
		{i,1,Length[polys]}
	];
	polystr=StringTake[polystr,StringLength[polystr]-1];
	Return[TemplateApply[str,{ToSingularExpression[name],polystr}]];
];


(* ::Subsection:: *)
(*SingularIntersect*)


(*generate the code of computing ideals' intersection in Singular*)
(*e.g.: SingularIntersect[i[1],i[2]], output: "intersect(i(1),i(2));\n"*)
SingularIntersect[ideals__]:=Module[
	{str="intersect("},
	Do[
		str=str<>ToSingularExpression[{ideals}[[ii]]]<>If[ii==Length[{ideals}],"",","],
		{ii,1,Length[{ideals}]}
	];
	str=str<>");\n";
	Return[str]
];


(* ::Subsection:: *)
(*SingularEliminate*)


(*generate the code of the function 'eliminate' in Singular*)
(*e.g.: SingularEliminate[i[1],{x[1],x[2]}], output: "eliminate(i(1),x(1)*x(2));\n"*)
SingularEliminate[ideal_,vars_List]:=Module[
	{str,varstr},
	str="eliminate(`1`,`2`);\n";
	varstr=ToSingularExpression[Times@@vars ];
	Return[TemplateApply[str,{ToSingularExpression[ideal],varstr}]]
];


(* ::Subsection:: *)
(*SingularGroebner*)


(*generate the code of the function 'groebner' in Singular*)
(*e.g.: SingularGroebner[i[1]], output: "groebner(i(1));\n"*)
SingularGroebner[ideal_]:=Module[
	{str},
	str="groebner("<>ToSingularExpression[ideal]<>");\n";
	Return[str]
];


(* ::Section:: *)
(*Read Singular Result*)


(* ::Subsection:: *)
(*StringToIdeal*)


StringToIdeal[ideal_String]:=Module[
	{res},
	res=StringReplace[ideal, {"("~~a:DigitCharacter..~~")" :> "[" <> a <> "]", "\n" -> ""}];
	res=StringSplit[res, ","];
	Return[ToExpression[res]];
]


(* ::Section:: *)
(*End*)


End[];


EndPackage[];
