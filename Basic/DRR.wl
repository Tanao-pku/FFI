(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["FFI`"];

MasterRecurrence::usage = "MasterRecurrence[family] gives the recurrence relations of master integrals from 6-2eps dimension to 4-2eps dimension.";
RaisingRecurrence::usage = "RaisingRecurrence[family] gives the recurrence relations of master integrals from 4-2eps dimension to 6-2eps dimension.";
GenDRR::usage = "GenDRR[famlily] generates the recurrence relations for the family and its UV families";


Begin["`Private`"]


(* ::Section:: *)
(*DRR*)


(* ::Subsection::Closed:: *)
(*BladeMaster*)


Options[BladeMaster] = {"Thread" -> 10, "WorkingDir"->Automatic};

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
	         
	If[OptionValue["WorkingDir"]===Automatic,
        dir = FileNameJoin[{CurrentDir[], "cache", ToString[family], "DRR"}],
        dir = OptionValue["WorkingDir"]
    ];
	If[!DirectoryQ[dir], CreateDirectory[dir]];
	         
	FileTemplateApply[template, rule, FileNameJoin[{dir, "master.wl"}]];
	RunProcess[{mma ,"-noprompt", "-script", FileNameJoin[{dir, "master.wl"}]}];
]


(* ::Subsection::Closed:: *)
(*BladeMasterRecurrence*)


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
		"If[Length[target]==0, res = {}, res = BLReduce[target,\"ReadCacheQ\"->False]];",
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


(* ::Subsection::Closed:: *)
(*MasterRecurrence*)


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
(*DRR*)


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


RaisingF[mono_, Fexpr_]:= Module[
    {inversemono, zvar, index, res = Fexpr, FI},
    
    (*There might be SP in front of F*)
    FI = Cases[Variables[Fexpr], _F][[1]];
    
    (*z[i]'s in mono*)
    zvar = Cases[Variables[mono], _z];
    
    (*z[i]->1/z[i]*)
    inversemono = mono /. FFI`z[i_]:>1/FFI`z[i];
    
    (*Raising coefficients*)
    Do[
        index = zvar[[i, 1]];
        res = res * FI[[index]],
        {i, Length[zvar]}
    ];
    
    Return[res * inversemono];
];


RaisingF[poly_Plus, Fexpr_]:= Module[
    {monos},
    
    (*Monomials*)
    monos = List@@poly;
    
    Return[Plus@@(RaisingF[#, Fexpr]&/@monos)];
];


(* ::Subsection::Closed:: *)
(*RaisingDRR*)


RaisingDRR0[Fexpr_, family_?FamilyQ]:= (-1)^Length[family["Loop"]] * RaisingF[Expand[FFI`Private`UPoly[family]], Fexpr];


RaisingDRR[Fexpr_F, family_?FamilyQ]:= Module[
    {denoPow, originPow, lsq, lsqMat, propandisp, numList, sol, zList, res},
    
    (*The powers and coefficients of li^2 of denominators*)
    propandisp = Flatten[SPPropAndIsp[family]];
    denoPow = Table[0, {i, Length[propandisp]}];
    lsq = FFI`SP/@family["Loop"];
    lsqMat = Table[0, {i, Length[propandisp]}, {j, Length[lsq]}];
    Do[
        If[Fexpr[[i]] > 0, 
            denoPow[[i]] = Fexpr[[i]];
            lsqMat[[i]] = Coefficient[propandisp[[i]], lsq]
        ],
        {i, Length[Fexpr]}
    ];
    originPow = denoPow;
    
    (*Numerator list*)
    numList = Flatten[Table[Table[propandisp[[i]], {j, -Fexpr[[i]]}], {i, Length[propandisp]}]];
    
    (*deal with numerator list*)
    zList = Table[FFI`z[i], {i, Length[propandisp]}];
    Off[LinearSolve::nosol];
    Do[
        (*Subtract the li^2 in the numerator*)
        sol = LinearSolve[Transpose[lsqMat], Coefficient[numList[[i]], lsq]];
        If[Head[sol]=!=List, 
            numList[[i]]=0;
            Continue[]
        ];
        
        numList[[i]] = numList[[i]] - sol . propandisp + sol . zList;
        
        (*Change the lsqMat and denoPow*)
        denoPow = denoPow - Sign[sol];
        lsqMat = Transpose[Thread[lsqMat * Sign[denoPow]]]
        ,
        
        {i, Length[numList]}
    ];
    On[LinearSolve::nosol];
    
    res = Expand[(Times@@numList) * (FFI`F@@originPow)];
    If[Head[res]===Plus,
        res = RaisingDRR0[#, family]&/@res,
        res = RaisingDRR0[res, family]
    ];
    
    res = Expand[res/.SPToProp[family, "SPForm"->True]];
    
    Return[res];
];


(* ::Subsection::Closed:: *)
(*FiniteFlowInverse*)


FiniteFlowInverse[workingDir_String, target_String]:= Module[
    {template, rule, mma = "wolfram"},
    
    template = StringJoin[Riffle[
	{
		"If[$FrontEnd===Null,$InputFileName,NotebookFileName[]]//DirectoryName//SetDirectory;",
		"Get[\"`finiteflow`\"];",
		"mat = Get[\"`target`\"];",
		"sol = FFInverse[mat];",
		"Put[sol, \"inversesol\"];",
		"Quit[];"
	},
	"\n"
	]];
	
	rule = <|"finiteflow" -> Global`$FiniteFlowPath, "target" -> target|>;
	
	FileTemplateApply[template, rule, FileNameJoin[{workingDir, "inverse.wl"}]];
	RunProcess[{mma ,"-noprompt", "-script", FileNameJoin[{workingDir, "inverse.wl"}]}];
	
	Return[Get[FileNameJoin[{workingDir, "inversesol"}]]];
];


(* ::Subsection::Closed:: *)
(*BladeRaisingRecurrence*)


Options[BladeRaisingRecurrence] = {"Thread" -> 10, "WorkingDir"->Automatic};

BladeRaisingRecurrence[family_?FamilyQ, opt:OptionsPattern[]]:= Module[
    {template, rule, mma = "wolfram", dir, master, Rmaster, reduced, factor, L, e, res},
    
    If[OptionValue["WorkingDir"]===Automatic,
        dir = FileNameJoin[{CurrentDir[], "cache", ToString[family], "DRR"}],
        dir = OptionValue["WorkingDir"]
    ];
	If[!DirectoryQ[dir], CreateDirectory[dir]];
	
	master = Get[FileNameJoin[{dir, "masters"}]];
	Rmaster = RaisingDRR[#, family]&/@master;
	Put[Rmaster, FileNameJoin[{dir, "Rmasters"}]];
    
    template = StringJoin[Riffle[
	{
		"If[$FrontEnd===Null,$InputFileName,NotebookFileName[]]//DirectoryName//SetDirectory;",
		"Get[\"`blade`\"];",
		"family = `family`;",
		"dimension = 6 - 2 eps;",
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
		"If[Length[target]==0, res = {}, res = BLReduce[target,\"ReadCacheQ\"->False]];",
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


(* ::Subsection:: *)
(*RaisingRecurrence*)


Options[RaisingRecurrence] = {"Print"->True, "WorkingDir"->Automatic, "Thread"->10, "ReaddCacheQ"->True};

RaisingRecurrence[family_?FamilyQ, opt:OptionsPattern[]]:= Module[
    {dir, time, ibp, master, coeMat, sol, inverse, res},
    
    If[OptionValue["WorkingDir"]===Automatic,
        dir = FileNameJoin[{CurrentDir[], "cache", ToString[family], "DRR"}],
        dir = OptionValue["WorkingDir"]
    ];
	If[!DirectoryQ[dir], CreateDirectory[dir]];
	If[OptionValue["ReaddCacheQ"]===True && FileExistsQ[FileNameJoin[{dir, "recurrence"}]],
	    Return[Get[FileNameJoin[{dir, "recurrence"}]]];
	];
    
    If[OptionValue["Print"], Print["Using Blade to find master integrals..."]];
    time = AbsoluteTiming[BladeMaster[family, FilterRules[{opt}, Options[BladeMaster]]]][[1]];
    If[OptionValue["Print"], Print["Time elapsed: ", time, "s."]];
    If[OptionValue["Print"], Print["Using Blade to reduce Rmaster integrals..."]];
    {time, ibp} = AbsoluteTiming[BladeRaisingRecurrence[family, FilterRules[{opt}, Options[BladeRaisingRecurrence]]]];
    If[OptionValue["Print"], Print["Time elapsed: ", time, "s."]];
    
    master = Cases[Variables[ibp[[All, 2]]], _F];
    sol = master/.ibp;
    coeMat = Table[Coefficient[sol[[i]], master], {i, Length[sol]}];
    Put[coeMat, FileNameJoin[{dir, "mat"}]];
    
    If[OptionValue["Print"], Print["Using FiniteFlow to inverse the matrix..."]];
    {time, inverse} = AbsoluteTiming[FiniteFlowInverse[dir, FileNameJoin[{dir, "mat"}]]];
    If[OptionValue["Print"], Print["Time elapsed: ", time, "s."]];
    
    res = Thread[master->(inverse . master)];
    Put[res, FileNameJoin[{dir, "recurrence"}] ];
    
    Return[res];
]


(* ::Section:: *)
(*Generate DRR relations for family and its UV family*)


(* ::Subsection:: *)
(*GenDRR*)


Options[GenDRR] = {"UVMass" -> Global`m};

GenDRR[family_?FamilyQ, opt:OptionsPattern[]]:= Module[
    {uvfamily, dir},
    
    dir = CurrentDir[];
    
    uvfamily = FFI`BurnUV[family, OptionValue["UVMass"]];
    
    Do[
		If[i==0, 
			Print[family, ": ", AbsoluteTiming[RaisingRecurrence[family, "WorkingDir"->FileNameJoin[{dir, "cache",  ToString[family], "DRR"}], "Print"->False]][[1]], "s"],
			Print[uvfamily[[i]], ": ", AbsoluteTiming[RaisingRecurrence[uvfamily[[i]], "WorkingDir"->FileNameJoin[{dir, "cache",  ToString[uvfamily[[i]]], "DRR"}], "Print"->False]][[1]], "s"];
		],
		{i, 0, Length[uvfamily]}(*,
		DistributedContexts -> All*)
	];
];


(* ::Section:: *)
(*Express high dimensional integrals into 4d*)


(* ::Subsection:: *)
(*To4dF*)


(*Fexpr is d dimensional integrals*)
To4dF[Fexpr_, d_Integer, family_?FamilyQ]:= Module[
    {index, recurrence, res = Fexpr},
    
    index = (d - 4) / 2;
    
    If[index <= 0, Return[Fexpr]];
    
    (*6d to 4d recurrence*)
    recurrence = Get[FileNameJoin[{CurrentDir[], "cache", ToString[family], "DRR", "recurrence"}]];
    
    res = res /. Global`eps -> Global`eps - index;
    While[index > 0,
          res = Collect[res /. (recurrence /. Global`eps -> Global`eps - index + 1), _FFI`F, Together];
          index--;
    ];
    
    Return[res]
];


(* ::Subsection:: *)
(*To4dFList*)


(*
To4dFList[Flist, dinfo, family] transforms a list of integrals of different dimension into
4 dimension and returns the list of 4-dimension integrals.

The parameter 'Flist' is a list of F[k1, k2, ...].

The parameter 'dinfo' is of the form {d1->num1, d2->num2, ...}, which means 1 to num1 integrals are in dimension d1, 
num1 + 1 to num1 + num2 integrals are in dimension d2, ...
*)
To4dFList[Flist_List, dinfo_List, family_?FamilyQ]:= Module[
    {start = 0, res},
    
    res = Table[0, {i, Length[Flist]}];
    
    Do[
        res[[start + 1 ;; start + dinfo[[i, 2]]]] = To4dF[Flist[[start + 1 ;; start + dinfo[[i, 2]]]], dinfo[[i, 1]], family];
        start += dinfo[[i, 2]],
        {i, Length[dinfo]}
    ];
    
    Return[res];
]


(* ::Section:: *)
(*End*)


End[]


EndPackage[];
