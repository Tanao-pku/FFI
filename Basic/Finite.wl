(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["FFI`"];


GenFiniteRelation::usage = "GenFiniteRelation[family, deno, rank] generates the finite relation up to a given rank. IR finite ideal has to be calculated first.
GenFiniteRelation[family, deno, eles] generates the finite relation with respect to the eles which is expected to be a list of IR finite numerators";
GenEvaEle::usage = "GenEvaEle[family, rank] generates the evanescent numerators up to a given rank. It simply uses \!\(\*OverscriptBox[\(li\), \(~\)]\)\[CenterDot]\!\(\*OverscriptBox[\(lj\), \(~\)]\) as the generators, otherwise you provide the generators by change the optional parameter \"Generator\"";
GenDRRFiniteRelation::usage = "";


Begin["`Private`"]


(* ::Section:: *)
(*Generate Finite Relation*)


(* ::Subsection:: *)
(*GenFiniteRelation*)


Options[GenFiniteRelation] = {"IncludeEvanescent" -> False, "SubtractUV" -> True};

GenFiniteRelation[family_?FamilyQ, deno_List, rank_Integer, opt: OptionsPattern[]]:= Module[
	{dir, gens, zvars, eles, finite, uvcttable, uvpowercounting, degree0, spprop, para, time},
	
	gens = Get[FileNameJoin[{CurrentDir[], "cache", ToString[family], "Ideal", "result"}]];
	zvars = Table[FFI`z[i], {i, 1, Length[family["Prop"]] + Length[family["Isp"]]}];
	para = FindParameter[family];
	
	(*total elements*)
	eles = GenIdealElement[gens, zvars, rank, "Parameters"->para];
	
	(*evanescent elements, stored in file ExpIBP/evaele*)
	If[OptionValue["IncludeEvanescent"], 
		(*rank of evanescent elements needn't to be to high*)
		GenEvaEle[family, Ceiling[rank / 2]];
		eles = Join[eles, Get[FileNameJoin[{CurrentDir[], "cache", ToString[family], "ExpIBP", "evaele"}]]];
	];
	
	GenFiniteRelation[family, deno, eles, opt];
]

(*override*)
(*eles is a list of numerators, in form of polynomials of z[i]'s*)
GenFiniteRelation[family_?FamilyQ, deno_List, eles_List, opt: OptionsPattern[]]:= Module[
	{dir, finite, spprop, time, uvcttable},
	
	dir = FileNameJoin[{CurrentDir[], "cache", ToString[family], "ExpIBP"}];
	If[!DirectoryQ[dir], CreateDirectory[dir]];
	
	(*IR finite*)
	finite = Expand[FFI`F@@Join[deno, Table[0, {i, 1, Length[family["Isp"]]}]] * eles];
	Put[finite, FileNameJoin[{dir, "finite"}]];
	
	spprop = Flatten[SPPropAndIsp[family]];
	
	(*deal with option "SubtractUV"*)
	If[OptionValue["SubtractUV"] =!= True, Return[]];
	
	(*UV counter term*)
	time = AbsoluteTiming[uvcttable = Table[FFI`UVCounterTerm[(eles[[i]]/.FFI`z[k_]:>spprop[[k]]) * FFI`F@@Join[deno, Table[0, {ii, Length[family["Isp"]]}]], family, "ListedByFamily"->True], {i, Length[finite]}]][[1]];
	(*Print["UV CT using time: ", time, "s"];*)
	time = AbsoluteTiming[Do[
		Put[Expand[(#[[i]]&/@uvcttable)/.SPToProp[family["UVFamily"][[i]], "SPForm"->True]], FileNameJoin[{dir, "ct"<>UVSymbol[family, i]}]],
		{i, Length[family["UVFamily"]]}
	]][[1]];
	(*Print["Putting UV CT in file using time: ", time, "s"];*)
]


(* ::Subsection::Closed:: *)
(*GenEvaEle*)


Options[GenEvaEle] = {"Generator"->Automatic};

(*lazy version*)
GenEvaEle[family_?FamilyQ, rank_Integer, opt: OptionsPattern[]]:= Module[
	{dir, gens, eles, loop, out, zvars, para},
	
	dir = FileNameJoin[{CurrentDir[], "cache", ToString[family], "ExpIBP"}];
	If[!DirectoryQ[dir], CreateDirectory[dir]];
	
	If[OptionValue["Generator"]===Automatic && Length[family["Leg"]] < 4, Put[{}, FileNameJoin[{dir, "evaele"}]];Return[{}]];
	
	loop = family["Loop"];
	out = family["Leg"];
	
	(*generators*)
	If[OptionValue["Generator"]===Automatic,
		gens = Flatten[Table[Gram[Flatten[{loop[[i]], out[[1;;4]]}], Flatten[{loop[[j]], out[[1;;4]]}], family], {i, 1, Length[loop]}, {j, i, Length[loop]}]],
		gens = OptionValue["Generator"];
	];
	
	zvars = Table[FFI`z[i], {i, 1, Length[family["Prop"]] + Length[family["Isp"]]}];
	para = FindParameter[family];
	
	(*total elements*)
	eles = GenIdealElement[gens, zvars, rank, "Parameters"->para];
	
	Put[eles, FileNameJoin[{dir, "evaele"}]];
	Return[eles]
]


(* ::Subsection::Closed:: *)
(*GenEvaIdeal*)


(*UNFINISHED!!!*)


Options[GenEvaIdeal] = {"Generator"->Automatic};

(*generate the evanescent ideal*)
(*lazy version*)
GenEvaIdeal[family_?FamilyQ, opt: OptionsPattern[]]:= Module[
	{fideal, dir, code, gens, loop, out},
	
	loop = family["Loop"];
	out = family["Leg"];
	
	(*data directory*)
	dir = FileNameJoin[{CurrentDir[], "cache", ToString[family], "Ideal"}];
	
	(*whether the number of legs is less than 4*)
	If[OptionValue["Generator"]===Automatic && Length[family["Leg"]] < 4, Put[{}, FileNameJoin[{dir, "evaideal"}]];Return[{}]];
	
	(*get the finite ideal*)
	fideal = Get[FileNameJoin[{dir, "result"}]];
	
	(*original eva generators*)
	If[OptionValue["Generator"]===Automatic,
		(*li lj*)
		gens = Flatten[Table[Gram[Flatten[{loop[[i]], out}], Flatten[{loop[[j]], out}], family], {i, 1, Length[loop]}, {j, i, Length[loop]}]];
		(*vi lj*)
		(*gens = Join[gens, Flatten[Table[Gram[out/.out[[i]]->loop[[j]], out], {i, Length[out]}, {j, Length[loop]}]]]*),
		
		gens = OptionValue["Generator"];
	];
	
	(*generate Singular code*)
	code = SingularDefineRing[{"z", Length[family["Prop"]] + Length[family["Isp"]]}, "Parameters" -> FindParameter[family]];
	code = code <> SingularDefineIdeal["ide1", fideal];
	code = code <> "";
]


(* ::Section:: *)
(*Generate DRR Finite Relation*)


(* ::Subsection:: *)
(*GenDRRFiniteRelation*)


GenDRRFiniteRelation[family_?FamilyQ, rank_Integer]:= Module[
    {dir, finite, uvcttable},
    
    dir = FileNameJoin[{CurrentDir[], "cache", ToString[family], "ExpIBP"}];
	If[!DirectoryQ[dir], CreateDirectory[dir]];
	
	(*IR finite*)
	finite = GenSingleSeeds[family, rank];
	Put[finite, FileNameJoin[{dir, "finite"}]];
    
    (*UV counterterms*)
    uvcttable = Table[FFI`UVCounterTerm[finite[[i]], family, "ListedByFamily"->True, "Dimension"->6], {i, Length[finite]}];
    Do[
		Put[(#[[i]]&/@uvcttable), FileNameJoin[{dir, "ct"<>UVSymbol[family, i]}]],
		{i, Length[family["UVFamily"]]}
	];
];


(* ::Subsection:: *)
(*GenSingleSeeds*)


(*Generate 6d(for now) finite integrals*)
(*TODO: add dots and higher dimensional seeds*)
GenSingleSeeds[family_?FamilyQ, rank_Integer]:= Module[
    {dir, zlist, ele, finite, uvcttable},
    
    dir = FileNameJoin[{CurrentDir[], "cache", ToString[family], "ExpIBP"}];
	If[!DirectoryQ[dir], CreateDirectory[dir]];
    
    (*z[i]'s*)
    zlist = Table[FFI`z[i], {i, Length[family["Prop"]] + Length[family["Isp"]]}];
    
    (*Numerators*)
    (*TODO: modify GenIdealElement*)
    ele = Join[{1}, GenIdealElement[zlist, zlist, rank]];
    
    (*Single integrals*)
    finite = ele * (F@@Join[Table[1, {i, Length[family["Prop"]]}], Table[0, {i, Length[family["Isp"]]}]]);
    
    Return[finite];
];


(* ::Subsection:: *)
(*IRFiniteQ*)


IRFiniteQ[Fexpr_F, negregions_List, regionloops_List, dimension_Integer:4]:= Module[
    {res = True, proppow},
    
    (*Powers of the propagators*)
    proppow = Fexpr[[1;;Length[negregions[[1]]]]];
    
    Do[
        If[regionloops[[i]] * dimension/2 + proppow . negregions[[i]] < 0, res = False;Break[]],
        {i, Length[negregions]}
    ];
    
    Return[res];
];


(* ::Section:: *)
(*End*)


End[];


EndPackage[];
