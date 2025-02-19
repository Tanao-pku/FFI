(* ::Package:: *)

BeginPackage["FFI`"];


Begin["`Private`"]


(* ::Section:: *)
(*Reduce Finite Relation*)


(* ::Subsection:: *)
(*BladeIBP*)


Options[BladeIBP] = {"Thread" -> 10};

(*use Blade to reduce target integrals*)
BladeIBP[family_?FamilyQ, workFile_String, targetFile_String, resultFile_String, masterFile_String, OptionsPattern[]]:=Module[
	{template, rule, mma = "wolfram"},
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
		"target = Get[\"`targetFile`\"]/.F[i__]:>BL[family, {i}];",
		"If[Union[target]==={0}, Put[Table[0, {i, Length[target]}], \"`resultFile`\"];Put[{}, \"`masterFile`\"];Quit[]];",
		"res = BLReduce[target,\"ReadCacheQ\"->False];",
		"Put[res/.BL[family,{i__}]:>F[i], \"`resultFile`\"]",
		"Put[BLFamilyInf[family][\"Masters\"]/.BL[family,{i__}]:>F[i], \"`masterFile`\"]",
		"Quit[];"
	},
	"\n"
	]];
	
	rule = <|"family"->family, "loop" -> family["Loop"], "leg" -> family["Leg"], "prop" -> ToString[Join[family["Prop"], family["Isp"]], InputForm], "replace"->ToString[family["Replace"], InputForm], 
	         "topsector"->Join[Table[1, {i, 1, Length[family["Prop"]]}], Table[0, {i, 1, Length[family["Isp"]]}]], "blade" -> $BladePath, "targetFile" -> targetFile, "resultFile" -> resultFile, "masterFile" -> masterFile, 
	         "thread" -> OptionValue["Thread"]|>;
	         
	FileTemplateApply[template, rule, workFile];
	RunProcess[{mma ,"-noprompt", "-script", workFile}];
]


(* ::Subsection:: *)
(*ReduceFinite*)


Options[ReduceFinite] = {"Thread" -> 10};

ReduceFinite[family_?FamilyQ, opt:OptionsPattern[]]:= Module[
	{rule, dir, mma = "wolfram", time},
	rule = <|"family"->family, "loop" -> family["Loop"], "leg" -> family["Leg"], "prop" -> ToString[Join[family["Prop"], family["Isp"]], InputForm], "replace"->ToString[family["Replace"], InputForm], 
	         "topsector"->Join[Table[1, {i, 1, Length[family["Prop"]]}], Table[0, {i, 1, Length[family["Isp"]]}]], "blade" -> $BladePath|>;
	         
	dir = FileNameJoin[{CurrentDir[], "cache", ToString[family], "ExpIBP"}];
	If[!DirectoryQ[dir], CreateDirectory[dir]];
	
	Print["Using Blade to reduce finite integrals..."];
	time = AbsoluteTiming[
		ParallelDo[
			If[i==0, 
				Print[family, ": ", AbsoluteTiming[BladeIBP[family, FileNameJoin[{dir, "ibp.wl"}], "finite", "ibp", "master", "Thread"->OptionValue["Thread"]]][[1]], "s"],
				Print[family["UVFamily"][[i]], ": ", AbsoluteTiming[BladeIBP[family["UVFamily"][[i]], FileNameJoin[{dir, UVSymbol[family, i]<>"ibp.wl"}], "ct"<>UVSymbol[family, i], UVSymbol[family, i]<>"ibp", UVSymbol[family, i]<>"master", "Thread"->OptionValue["Thread"]]][[1]], "s"];
			],
			{i, 0, Length[family["UVFamily"]]},
			DistributedContexts -> All
		]
	][[1]];
	Print["Time used: ", time, "s"];
	Return[Get[FileNameJoin[{dir, "ibp"}]]];
]


(* ::Subsection:: *)
(*ReducedTo4d*)


ReducedTo4d[family_?FamilyQ, opt:OptionsPattern[]]:= Module[
    {dir, time, Fs, dinfo},
    
    (*working directory*)
	dir = FileNameJoin[{CurrentDir[], "cache", ToString[family], "ExpIBP"}];
	If[!DirectoryQ[dir], CreateDirectory[dir]];
    
    (*Init*)
    Fs = Table[0, {i, Length[family["UVFamily"]] + 1}];
    dinfo = Get[FileNameJoin[{CurrentDir[], "cache", ToString[family], "ExpIBP", "drrseedinfo"}]];
    
    Print["Turn reduced finite integrals into 4d..."];
	time = AbsoluteTiming[
		Do[
			If[i==0, 
			
				Print[family, ": ", 
				      AbsoluteTiming[
				          Fs[[i+1]] = To4dFList[Get[FileNameJoin[{dir, "ibp"}]], dinfo, family];
				          Put[Fs[[i+1]], FileNameJoin[{dir, "ibp"}]]
				      ][[1]], 
				      "s"],
			    				
				Print[family["UVFamily"][[i]], ": ", 
				      AbsoluteTiming[
				          Fs[[i+1]] = To4dFList[Get[FileNameJoin[{dir, UVSymbol[family, i]<>"ibp"}]], dinfo, family["UVFamily"][[i]]];
				          Put[Fs[[i+1]], FileNameJoin[{dir, UVSymbol[family, i]<>"ibp"}]]
				      ][[1]], 
				      "s"];
			],
			{i, 0, Length[family["UVFamily"]]}
		]
	][[1]];
	Print["Time used: ", time, "s"];
]


(* ::Subsection:: *)
(*GenExpEq*)


Options[GenExpEq] = {"IncludeEvanescent" -> False, "Level" -> -1};

GenExpEq[family_?FamilyQ, OptionsPattern[]]:= Module[
	{dir, ibp, ibp2, ibpexp, master, exponent, exprule, expvar, time, uvct, uvmaster, uporder = OptionValue["Level"], evaelenum = 0},
	
	(*working directory*)
	dir = FileNameJoin[{CurrentDir[], "cache", ToString[family], "ExpIBP"}];
	If[!DirectoryQ[dir], CreateDirectory[dir]];
	
	(*deal with eva*)
	(*expand the master up to -1 or 0 order, depending on weather include the evanescent integrals*)
	If[OptionValue["IncludeEvanescent"], 
		(*expand the master up to -1 or 0 order, depending on weather include the evanescent integrals*)
		(*uporder = 0;*)
		evaelenum = Length[Get[FileNameJoin[{dir, "evaele"}]]];
		(*Print["evaelenum: ", evaelenum];*)
	];
	
	(*get reduced IR finite relations and masters (evanescent relations are included)*)
	ibp = Get[FileNameJoin[{dir, "ibp"}]]/.F[k__]:>F[family, {k}];
	master = Get[FileNameJoin[{dir, "master"}]]/.F[k__]:>F[family, {k}];
	
	(*get reduced UV counter terms and masters*)
	Do[
		uvmaster[i] = Get[FileNameJoin[{dir, UVSymbol[family, i]<>"master"}]]/.F[k__]:>F[UVFamilySymbol[family, i], {k}];
		uvct[i] = Get[FileNameJoin[{dir, UVSymbol[family, i]<>"ibp"}]]/.F[k__]:>F[UVFamilySymbol[family, i], {k}],
		{i, Length[family["UVFamily"]]}
	];
	
	(*summarize*)
	master = Flatten[{Table[uvmaster[i], {i, Length[family["UVFamily"]]}], master}];
	ibp = ibp - Sum[uvct[i], {i, Length[family["UVFamily"]]}];
	(*ibp = Select[ibp, #=!=0&];*)
	(*debug*)
	(*Return[ibp];*)
	
	(*calculate the minimal exponent of eps of each master integral*)
	time = AbsoluteTiming[exponent = Table[Min[Exponent[Series[Coefficient[ibp, master[[i]]], {Global`eps, 0, 0}] + Pi, Global`eps, Min]], {i, 1, Length[master]}];
	Do[
		If[exponent[[i]] == Infinity, exponent[[i]] = 0],
		{i, 1, Length[exponent]}
	];][[1]];
	(*debug*)
	(*Print["Eps exponents computing used time: ", time, "s"]*);
	
	exprule = Table[master[[i]] -> Sum[Global`eps^j(F[master[[i, 1]], j, {##}]&@@master[[i, 2]]), {j, -2*Length[family["Loop"]], uporder - exponent[[i]]}], {i, 1, Length[master]}];
	
	expvar = Join[List@@(#[[2]])&/@(exprule/.Global`eps->1)]//Flatten//Reverse;
	Put[expvar, FileNameJoin[{dir, "expvar"}]];
	
	Print["Expanding ibp relations..."];
	time = AbsoluteTiming[
		ibp2 = ibp/.exprule;
		ibpexp = Table[Coefficient[Series[ibp2[[i]], {Global`eps, 0, 0}], Global`eps, j], {i, Length[ibp2]}, {j, Exponent[Series[ibp[[i]], {Global`eps, 0, 0}]+Pi, Global`eps, Min]-4, If[i<=Length[ibp2]-evaelenum, -1, 0]}]//Flatten;
	][[1]];
	Print["Time used: ", time, "s"];
	
	Print["Simplifying expanded ibp relations..."];
	time = AbsoluteTiming[
		ibpexp = Collect[ibpexp, expvar, Together];
	][[1]];
	Print["Time used: ", time, "s"];
	Put[ibpexp, FileNameJoin[{dir, "ibpexp"}]];
	
	Return[ibpexp];
]


(* ::Section:: *)
(*Solve Expanded Equations*)


(* ::Subsection:: *)
(*ExpandedMasters*)


Options[ExpandedMasters] = {"IncludeEvanescent" -> False, "Level" -> -1};

ExpandedMasters[family_?FamilyQ, OptionsPattern[]]:= Module[
	{template, dir, time, mma = "wolfram", uporder},
	
	uporder = OptionValue["Level"];
	
	(*IncludeEvanescent Option*)
	(*If[OptionValue["IncludeEvanescent"], uporder = 0];*)
	
	template = StringJoin[Riffle[
	{
		"If[$FrontEnd===Null,$InputFileName,NotebookFileName[]]//DirectoryName//SetDirectory;",
		"Get[\"`finiteflow`\"];",
		"eqexp=Get[\"ibpexp\"];",
		"vars=Get[\"expvar\"];",
		"expmaster=FFSparseSolve[Thread[eqexp==0],vars,\"IndepVarsOnly\"->True,\"NeededVars\"->Cases[vars,F["<>ToString[family]<>",n_,{__}]/;n<=`uporder`]];",
		"Put[expmaster,\"expmaster\"];",
		"Quit[];"
	},
	"\n"
	]];
	
	dir = FileNameJoin[{CurrentDir[], "cache", ToString[family], "ExpIBP"}];
	If[!DirectoryQ[dir], CreateDirectory[dir]];
	
	FileTemplateApply[template, <|"finiteflow" -> $FiniteFlowPath, "uporder" -> uporder|>, FileNameJoin[{dir, "expmaster.wl"}]];
	
	Print["Using FiniteFlow to solve expanded masters..."];
	time = AbsoluteTiming[
		RunProcess[{mma ,"-noprompt", "-script", FileNameJoin[{dir, "expmaster.wl"}]}]
	][[1]];
	Print["Time used: ", time, "s"];
	
	Unprotect[family];
	family["DivMaster"] = Get[FileNameJoin[{dir, "expmaster"}]];
	Protect[family];
	
	Return[Get[FileNameJoin[{dir, "expmaster"}]]];
]


(* ::Subsection:: *)
(*ExpandedReduce*)


Options[ExpandedReduce] = {"IncludeEvanescent" -> False, "Level" -> -1};

ExpandedReduce[family_?FamilyQ, OptionsPattern[]]:= Module[
	{template, dir, time, mma = "wolfram", uporder},
	
	uporder = OptionValue["Level"];
	
	(*IncludeEvanescent Option*)
	(*If[OptionValue["IncludeEvanescent"], uporder = 0];*)
	
	template = StringJoin[Riffle[
	{
		"If[$FrontEnd===Null,$InputFileName,NotebookFileName[]]//DirectoryName//SetDirectory;",
		"Get[\"`finiteflow`\"];",
		"eqexp=Get[\"ibpexp\"];",
		"vars=Get[\"expvar\"];",
		"needed=Cases[vars,F["<>ToString[family]<>",n_,{__}]/;n<=`uporder`];",
		"expsol=FFSparseSolve[Thread[eqexp==0],vars,\"NeededVars\"->needed];",
		"finalsol=Thread[needed->(needed/.expsol)];",
		"expmaster=Cases[Variables[finalsol[[All,2]]],_F];",
		"Put[expmaster,\"expmaster\"];",
		"Put[finalsol,\"expsol\"];",
		"Quit[];"
	},
	"\n"
	]];
	
	dir = FileNameJoin[{CurrentDir[], "cache", ToString[family], "ExpIBP"}];
	If[!DirectoryQ[dir], CreateDirectory[dir]];
	
	FileTemplateApply[template, <|"finiteflow" -> $FiniteFlowPath, "uporder" -> uporder|>, FileNameJoin[{dir, "expreduce.wl"}]];
	
	Print["Using FiniteFlow to solve expanded equations..."];
	time = AbsoluteTiming[
		RunProcess[{mma ,"-noprompt", "-script", FileNameJoin[{dir, "expreduce.wl"}]}]
	][[1]];
	Print["Time used: ", time, "s"];
	
	Unprotect[family];
	family["DivMaster"] = Get[FileNameJoin[{dir, "expmaster"}]];
	Protect[family];
	
	Return[Get[FileNameJoin[{dir, "expsol"}]]];
]


(* ::Section:: *)
(*Automatic Functions*)


(* ::Subsection:: *)
(*DivergentMasters*)


Options[DivergentMasters] = {"FRank"->4, "UVMass"->Global`m, "IncludeEvanescent" -> False, "HasAsyResult" -> False, "Level" -> -1, "OBIdeal" -> False, "DegBound" -> 0, "Thread" -> 10, 
"Approach"->"DRR", "DRRRank"->2, "DRRDots"->1, "DimensionBound"->8};

DivergentMasters[family_?FamilyQ, OptionsPattern[]]:= Module[
	{time},
	
	(*DRR approach*)
	If[OptionValue["Approach"] == "DRR",
	    Print["Generating IR information..."];
	    time = AbsoluteTiming[BurnIR[family]][[1]];
	    Print["Time used: ", time, "s"];
	
	    Print["Generating UV information..."];
	    time = AbsoluteTiming[BurnUV[family, OptionValue["UVMass"]]][[1]];
	    Print["Time used: ", time, "s"];
	    
	    Print["Generating DRR..."];
	    time = AbsoluteTiming[GenDRR[family]][[1]];
	    Print["Time used: ", time, "s"];
	    
	    Print["Generating finite integral relations..."];
	    time = AbsoluteTiming[GenDRRFiniteRelation[family, OptionValue["DRRRank"], OptionValue["DRRDots"], "DimensionBound" -> OptionValue["DimensionBound"]]][[1]];
	    Print["Time used: ", time, "s"];
	    
	    ReduceFinite[family, "Thread" -> OptionValue["Thread"]];
	    ReducedTo4d[family];
	    GenExpEq[family, "Level" -> OptionValue["Level"]];
	    Return[ExpandedMasters[family, "Level" -> OptionValue["Level"]]];
	];
	
	(*4d approach*)
	If[OptionValue["OBIdeal"],
		OBFiniteIdeal[family, "HasAsyResult" -> OptionValue["HasAsyResult"], "DegBound" -> OptionValue["DegBound"]],
		FiniteIdeal[family, "HasAsyResult" -> OptionValue["HasAsyResult"], "DegBound" -> OptionValue["DegBound"]]
	];
	
	Print["Generating UV information..."];
	time = AbsoluteTiming[BurnUV[family, OptionValue["UVMass"]]][[1]];
	Print["Time used: ", time, "s"];
	
	Print["Generating finite integral relations..."];
	time = AbsoluteTiming[GenFiniteRelation[family, Table[1, {i, 1, Length[family["Prop"]]}], OptionValue["FRank"], "IncludeEvanescent" -> OptionValue["IncludeEvanescent"]]][[1]];
	Print["Time used: ", time, "s"];
	
	ReduceFinite[family, "Thread" -> OptionValue["Thread"]];
	GenExpEq[family, "IncludeEvanescent" -> OptionValue["IncludeEvanescent"], "Level" -> OptionValue["Level"]];
	ExpandedMasters[family, "IncludeEvanescent" -> OptionValue["IncludeEvanescent"], "Level" -> OptionValue["Level"]]
]


(* ::Subsection:: *)
(*DivergentReduce*)


Options[DivergentReduce] = {"FRank"->4, "UVMass"->Global`m, "IncludeEvanescent" -> False, "HasAsyResult" -> False, "Level" -> -1, "OBIdeal" -> False, "DegBound" -> 0, "Thread" -> 10, 
"Approach"->"DRR", "DRRRank"->2, "DRRDots"->1, "DimensionBound"->8};

DivergentReduce[family_?FamilyQ, OptionsPattern[]]:= Module[
	{time},
	
	(*DRR approach*)
	If[OptionValue["Approach"] == "DRR",
	    Print["Generating IR information..."];
	    time = AbsoluteTiming[BurnIR[family]][[1]];
	    Print["Time used: ", time, "s"];
	
	    Print["Generating UV information..."];
	    time = AbsoluteTiming[BurnUV[family, OptionValue["UVMass"]]][[1]];
	    Print["Time used: ", time, "s"];
	    
	    Print["Generating DRR..."];
	    time = AbsoluteTiming[GenDRR[family]][[1]];
	    Print["Time used: ", time, "s"];
	    
	    Print["Generating finite integral relations..."];
	    time = AbsoluteTiming[GenDRRFiniteRelation[family, OptionValue["DRRRank"], OptionValue["DRRDots"], "DimensionBound" -> OptionValue["DimensionBound"]]][[1]];
	    Print["Time used: ", time, "s"];
	    
	    ReduceFinite[family, "Thread" -> OptionValue["Thread"]];
	    ReducedTo4d[family];
	    GenExpEq[family, "Level" -> OptionValue["Level"]];
	    Return[ExpandedReduce[family, "Level" -> OptionValue["Level"]]];
	];
	
	(*4d approach*)
	If[OptionValue["OBIdeal"],
		OBFiniteIdeal[family, "HasAsyResult" -> OptionValue["HasAsyResult"], "DegBound" -> OptionValue["DegBound"]],
		FiniteIdeal[family, "HasAsyResult" -> OptionValue["HasAsyResult"], "DegBound" -> OptionValue["DegBound"]]
	];
	
	Print["Generating UV information..."];
	time = AbsoluteTiming[BurnUV[family, OptionValue["UVMass"]]][[1]];
	Print["Time used: ", time, "s"];
	
	Print["Generating finite integral relations..."];
	time = AbsoluteTiming[GenFiniteRelation[family, Table[1, {i, 1, Length[family["Prop"]]}], OptionValue["FRank"], "IncludeEvanescent" -> OptionValue["IncludeEvanescent"]]][[1]];
	Print["Time used: ", time, "s"];
	
	ReduceFinite[family, "Thread" -> OptionValue["Thread"]];
	GenExpEq[family, "IncludeEvanescent" -> OptionValue["IncludeEvanescent"], "Level" -> OptionValue["Level"]];
	ExpandedReduce[family, "IncludeEvanescent" -> OptionValue["IncludeEvanescent"], "Level" -> OptionValue["Level"]]
]


(* ::Subsection:: *)
(*DRRReduce*)


Options[DRRReduce] = {"UVMass"->Global`m, "Thread"->10, "Level"->-1, "DRRRank"->2, "DRRDots"->1, "DimensionBound"->8};
DRRReduce[family_?FamilyQ]:= Module[
    {},
    
    Print["Generating IR information..."];
	    time = AbsoluteTiming[BurnIR[family]][[1]];
	    Print["Time used: ", time, "s"];
	
	    Print["Generating UV information..."];
	    time = AbsoluteTiming[BurnUV[family, OptionValue["UVMass"]]][[1]];
	    Print["Time used: ", time, "s"];
	    
	    Print["Generating DRR..."];
	    time = AbsoluteTiming[GenDRR[family]][[1]];
	    Print["Time used: ", time, "s"];
	    
	    Print["Generating finite integral relations..."];
	    time = AbsoluteTiming[GenDRRFiniteRelation[family, OptionValue["DRRRank"], OptionValue["DRRDots"], "DimensionBound" -> OptionValue["DimensionBound"]]][[1]];
	    Print["Time used: ", time, "s"];
	    
	    ReduceFinite[family, "Thread" -> OptionValue["Thread"]];
	    ReducedTo4d[family];
	    GenExpEq[family, "Level" -> OptionValue["Level"]];
	    Return[ExpandedReduce[family, "Level" -> OptionValue["Level"]]];
]


(* ::Section:: *)
(*End*)


End[]


EndPackage[];
