(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["FFI`"];

MasterRecurrence::usage = "MasterRecurrence[family] gives the recurrence relations of master integrals from 6-2eps dimension to 4-2eps dimension.";
RaisingRecurrence::usage = "RaisingRecurrence[family] gives the recurrence relations of master integrals from 4-2eps dimension to 6-2eps dimension.";


Begin["`Private`"]


(* ::Section:: *)
(*DRR*)


(* ::Subsection::Closed:: *)
(*Blade finding masters*)


Options[BladeMaster] = {"Thread" -> 10};

BladeMaster[family_?FamilyQ, OptionsPattern[]]:= Module[
    {template, rule, mma = "wolfram", dir},
    
    template = StringJoin[Riffle[
	{
		"If[$FrontEnd===Null,$InputFileName,NotebookFileName[]]//DirectoryName//SetDirectory;",
		"Get[\"`blade`\"];",
		"family = `family`;",
		"dimension = 4 - 2 eps;",
		"loop = `loop`;",
		"leg = `leg`;",
		"conservation = {};",
		"replacement = `replace`;",
		"propagator = `prop`;",
		"topsector = `topsector`;",
		"numeric = {};",
		"BLNthreads = `thread`;",
		"BLFamilyDefine[family,dimension, propagator,loop,leg,conservation, replacement,topsector,numeric];",
		"master = BLMaximalCutMasters[];",
		"Put[master/.BL[_, {k__}]:>F[k], \"masters\"];",
		"Quit[];"
	},
	"\n"
	]];
	
	rule = <|"family"->family, "loop" -> family["Loop"], "leg" -> family["Leg"], "prop" -> ToString[Join[family["Prop"], family["Isp"]], InputForm], "replace"->ToString[family["Replace"], InputForm], 
	         "topsector"->Join[Table[1, {i, 1, Length[family["Prop"]]}], Table[0, {i, 1, Length[family["Isp"]]}]], "blade" -> Global`$BladePath, "thread" -> OptionValue["Thread"]|>;
	         
	dir = FileNameJoin[{CurrentDir[], "cache", ToString[family], "DRR"}];
	If[!DirectoryQ[dir], CreateDirectory[dir]];
	         
	FileTemplateApply[template, rule, FileNameJoin[{dir, "master.wl"}]];
	RunProcess[{mma ,"-noprompt", "-script", FileNameJoin[{dir, "master.wl"}]}];
]


(* ::Subsection::Closed:: *)
(*Recurrence for masters*)


Options[BladeMasterRecurrence] = {"Thread" -> 10};

BladeMasterRecurrence[family_?FamilyQ, OptionsPattern[]]:= Module[
    {template, rule, mma = "wolfram", dir, master, Bmaster, reduced, factor, L, e, res},
    
    dir = FileNameJoin[{CurrentDir[], "cache", ToString[family], "DRR"}];
	If[!DirectoryQ[dir], CreateDirectory[dir]];
	
	master = Get[FileNameJoin[{dir, "masters"}]];
	Bmaster = Expand[BaikovPoly[family] * master];
	Put[Bmaster, FileNameJoin[{dir, "Bmasters"}]];
    
    template = StringJoin[Riffle[
	{
		"If[$FrontEnd===Null,$InputFileName,NotebookFileName[]]//DirectoryName//SetDirectory;",
		"Get[\"`blade`\"];",
		"family = `family`;",
		"dimension = 4 - 2 eps;",
		"loop = `loop`;",
		"leg = `leg`;",
		"conservation = {};",
		"replacement = `replace`;",
		"propagator = `prop`;",
		"topsector = `topsector`;",
		"numeric = {};",
		"BLNthreads = `thread`;",
		"BLFamilyDefine[family,dimension, propagator,loop,leg,conservation, replacement,topsector,numeric];",
		"target = Get[\"Bmasters\"]/.F[i__]:>BL[family, {i}];",
		"res = BLReduce[target,\"ReadCacheQ\"->False];",
		"Put[res/.BL[_, {k__}]:>F[k], \"ibp\"];",
		"Quit[];"
	},
	"\n"
	]];
	
	rule = <|"family"->family, "loop" -> family["Loop"], "leg" -> family["Leg"], "prop" -> ToString[Join[family["Prop"], family["Isp"]], InputForm], "replace"->ToString[family["Replace"], InputForm], 
	         "topsector"->Join[Table[1, {i, 1, Length[family["Prop"]]}], Table[0, {i, 1, Length[family["Isp"]]}]], "blade" -> Global`$BladePath, "thread" -> OptionValue["Thread"]|>;
	         
	FileTemplateApply[template, rule, FileNameJoin[{dir, "ibp.wl"}]];
	RunProcess[{mma ,"-noprompt", "-script", FileNameJoin[{dir, "ibp.wl"}]}];
	
	reduced = Get[FileNameJoin[{dir, "ibp"}]];
	
	L = Length[family["Loop"]];
	e = Length[family["Leg"]];
	factor = (-2)^L Gram[family["Leg"], family]^(-1)/ Pochhammer[4-2Global`eps-e-L+1, L];
	
	res = Thread[master -> (factor * reduced)];
	Put[res, FileNameJoin[{dir, "recurrence"}]];
	
	Return[res];
]


MasterRecurrence[family_?FamilyQ, opt:OptionsPattern[]]:= Module[
    {time, res},
    Print["Using Blade to find master integrals..."];
    time = AbsoluteTiming[BladeMaster[family, opt]][[1]];
    Print["Time elapsed: ", time, "s."];
    Print["Using Blade to reduce Bmaster integrals..."];
    {time, res} = AbsoluteTiming[BladeMasterRecurrence[family, opt]];
    Print["Time elapsed: ", time, "s."];
    
    Return[res];
]


(* ::Subsection::Closed:: *)
(*Dimension recursive relation*)


(*From d+2k to d dimension*)
(*Ignore coefficients independent of d*)
DRR[Fexpr_, d_, k_Integer, family_?FamilyQ]:= Module[
    {res, poh},
    res = Expand[PolyB[family]^k * Fexpr];
    
    res
    
    (*TODO: UV subtraction*)
];


(* ::Section:: *)
(*Raising DRR*)


(* ::Subsection::Closed:: *)
(*RaisingF*)


RaisingF[mono_Times, Fexpr_F]:= Module[
    {inversemono, zvar, index, res = Fexpr},
    
    (*z[i]'s in mono*)
    zvar = Cases[mono, _z, Infinity];
    
    (*z[i]->1/z[i]*)
    inversemono = mono /. FFI`z[i_]:>1/FFI`z[i];
    
    (*Raising coefficients*)
    Do[
        index = zvar[[i, 1]];
        res = res * Fexpr[[index]],
        {i, Length[zvar]}
    ];
    
    Return[res * inversemono];
];


RaisingF[poly_Plus, Fexpr_F]:= Module[
    {monos},
    
    (*Monomials*)
    monos = List@@poly;
    
    Return[Plus@@(RaisingF[#, Fexpr]&/@monos)];
];


(* ::Subsection::Closed:: *)
(*RaisingDRR*)


RaisingDRR[Fexpr_, family_?FamilyQ]:= (-1)^Length[family["Loop"]] * RaisingF[Expand[FFI`Private`UPoly[family]], Fexpr];


(* ::Subsection:: *)
(*Recurrence for masters*)


Options[BladeRaisingRecurrence] = {"Thread" -> 10};

BladeRaisingRecurrence[family_?FamilyQ, OptionsPattern[]]:= Module[
    {template, rule, mma = "wolfram", dir, master, Rmaster, reduced, factor, L, e, res},
    
    dir = FileNameJoin[{CurrentDir[], "cache", ToString[family], "DRR"}];
	If[!DirectoryQ[dir], CreateDirectory[dir]];
	
	master = Get[FileNameJoin[{dir, "masters"}]];
	Rmaster = RaisingDRR[#, family]&/@master;
	Put[Rmaster, FileNameJoin[{dir, "Rmasters"}]];
    
    template = StringJoin[Riffle[
	{
		"If[$FrontEnd===Null,$InputFileName,NotebookFileName[]]//DirectoryName//SetDirectory;",
		"Get[\"`blade`\"];",
		"family = `family`;",
		"dimension = 4 - 2 eps;",
		"loop = `loop`;",
		"leg = `leg`;",
		"conservation = {};",
		"replacement = `replace`;",
		"propagator = `prop`;",
		"topsector = `topsector`;",
		"numeric = {};",
		"BLNthreads = `thread`;",
		"BLFamilyDefine[family,dimension, propagator,loop,leg,conservation, replacement,topsector,numeric];",
		"target = Get[\"Rmasters\"]/.F[i__]:>BL[family, {i}];",
		"res = BLReduce[target,\"ReadCacheQ\"->False];",
		"Put[res/.BL[_, {k__}]:>F[k], \"Ribp\"];",
		"Quit[];"
	},
	"\n"
	]];
	
	rule = <|"family"->family, "loop" -> family["Loop"], "leg" -> family["Leg"], "prop" -> ToString[Join[family["Prop"], family["Isp"]], InputForm], "replace"->ToString[family["Replace"], InputForm], 
	         "topsector"->Join[Table[1, {i, 1, Length[family["Prop"]]}], Table[0, {i, 1, Length[family["Isp"]]}]], "blade" -> Global`$BladePath, "thread" -> OptionValue["Thread"]|>;
	         
	FileTemplateApply[template, rule, FileNameJoin[{dir, "Ribp.wl"}]];
	RunProcess[{mma ,"-noprompt", "-script", FileNameJoin[{dir, "Ribp.wl"}]}];
	
	reduced = Get[FileNameJoin[{dir, "Ribp"}]];
	
	Return[Thread[master -> reduced]];
]


RaisingRecurrence[family_?FamilyQ, opt:OptionsPattern[]]:= Module[
    {time, res},
    Print["Using Blade to find master integrals..."];
    time = AbsoluteTiming[BladeMaster[family, opt]][[1]];
    Print["Time elapsed: ", time, "s."];
    Print["Using Blade to reduce Bmaster integrals..."];
    {time, res} = AbsoluteTiming[BladeRaisingRecurrence[family, opt]];
    Print["Time elapsed: ", time, "s."];
    
    Return[res];
]


(* ::Section:: *)
(*End*)


End[]


EndPackage[];
