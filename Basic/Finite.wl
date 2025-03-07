(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["FFI`"];


Begin["`Private`"]


(* ::Section:: *)
(*Generate Finite Relation*)


(* ::Subsection:: *)
(*GenFiniteRelation*)


Options[GenFiniteRelation] = {"IncludeEvanescent" -> False, "SubtractUV" -> True};

GenFiniteRelation[family_?FamilyQ, deno_List, rank_Integer, opt: OptionsPattern[]]:= Module[
	{dir, gens, zvars, eles, finite, uvcttable, uvpowercounting, degree0, spprop, para, time},
	
	gens = Get[FileNameJoin[{CurrentDir[], "cache", ToString[family], "Ideal", "result"}]];
	zvars = Table[z[i], {i, 1, Length[family["Prop"]] + Length[family["Isp"]]}];
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
	finite = Expand[F@@Join[deno, Table[0, {i, 1, Length[family["Isp"]]}]] * eles];
	Put[finite, FileNameJoin[{dir, "finite"}]];
	
	spprop = Flatten[SPPropAndIsp[family]];
	
	(*deal with option "SubtractUV"*)
	If[OptionValue["SubtractUV"] =!= True, Return[]];
	
	(*UV counter term*)
	time = AbsoluteTiming[uvcttable = Table[UVCounterTerm[(eles[[i]]/.z[k_]:>spprop[[k]]) * F@@Join[deno, Table[0, {ii, Length[family["Isp"]]}]], family, "ListedByFamily"->True], {i, Length[finite]}]][[1]];
	(*Print["UV CT using time: ", time, "s"];*)
	time = AbsoluteTiming[Do[
		Put[Expand[(#[[i]]&/@uvcttable)/.SPToProp[family["UVFamily"][[i]], "SPForm"->True]], FileNameJoin[{dir, "ct"<>UVSymbol[family, i]}]],
		{i, Length[family["UVFamily"]]}
	]][[1]];
	(*Print["Putting UV CT in file using time: ", time, "s"];*)
]


(* ::Subsection:: *)
(*GenEvaEle*)


GenEvaEle[family_?FamilyQ, rank_Integer, opt: OptionsPattern[]]:= Module[
	{dir, gens, eles, loop, out, zvars, para},
	
	dir = FileNameJoin[{CurrentDir[], "cache", ToString[family], "ExpIBP"}];
	If[!DirectoryQ[dir], CreateDirectory[dir]];
	
	If[Length[family["Leg"]] < 4, Put[{}, FileNameJoin[{dir, "evaele"}]];Return[{}]];
	
	loop = family["Loop"];
	out = family["Leg"];
	
	(*generators*)
	gens = GenEvaIdeal[family];
	
	zvars = Table[z[i], {i, 1, Length[family["Prop"]] + Length[family["Isp"]]}];
	para = FindParameter[family];
	
	(*total elements*)
	eles = GenIdealElement[gens, zvars, rank, "Parameters"->para];
	
	Put[eles, FileNameJoin[{dir, "evaele"}]];
	Return[eles]
]


(* ::Subsection:: *)
(*GenEvaIdeal*)


(*generate the evanescent ideal*)
GenEvaIdeal[family_?FamilyQ]:= Module[
	{dir, code, gens, loop, out, regions, res},
	
	loop = family["Loop"];
	out = family["Leg"];
	
	regions = Get[FileNameJoin[{CurrentDir[], "cache", ToString[family], "Region", "regions"}]];
	
	(*data directory*)
	dir = FileNameJoin[{CurrentDir[], "cache", ToString[family], "Ideal"}];
	
	(*whether the number of legs is less than 4*)
	If[Length[family["Leg"]] < 4, Put[{}, FileNameJoin[{dir, "evaideal"}]];Return[{}]];
	
	(*eva generators*)
	(*li lj*)
	gens = Flatten[Table[Gram[Flatten[{loop[[i]], out}], Flatten[{loop[[j]], out}], family], {i, 1, Length[loop]}, {j, i, Length[loop]}]];
		
	(*generate Singular code*)
	code = SingularDefineRing[{"z", Length[family["Prop"]] + Length[family["Isp"]]}, "Parameters" -> FindParameter[family]];
	Do[
	    code = code <> SingularDefineIdeal["id"[i], Get[FileNameJoin[{dir, "ideal"<>ToString[i]}]]],
	    {i, Length[regions]}
	];
	code = code <> SingularDefineIdeal["id"[Length[regions] + 1], gens];
	code = code <> "ideal result = " <> SingularIntersect@@Table["id"[i], {i, Length[regions] + 1}];
	code = code <> "result = slimgb(result);write(\":w singout\",result);exit;";
	WriteSingularCode[code];
	res = StringToIdeal[RunSingular[]];
	Put[res, FileNameJoin[{dir, "evaideal"}]];
	Return[res];
]


(* ::Section:: *)
(*Generate DRR Finite Relation*)


(* ::Subsection:: *)
(*GenDRRFiniteRelation*)


Options[GenDRRFiniteRelation] = {"DimensionBound" -> 8};

(*
GenDRRFiniteRelation[family, rank, dots] will generate the finite relations (O(eps^0) relations) 
with seeding method GenTrapezoidSeeds.

The seed integrals will be stored in 'cache/family/ExpIBP/finite'. 
The UV counterterm integrals of each UV family will be stored in 'cache/family/ExpIBP/ctuvfamily'.

The number of integrals in each dimension will be stored in 'cache/family/ExpIBP/drrseedinfo', 
which is of the form {d1->num1, d2->num2, ...}.
*)
GenDRRFiniteRelation[family_?FamilyQ, rank_Integer, dots_Integer: 1, OptionsPattern[]]:= Module[
    {seeds, baseF},
    
    baseF = F @@ Join[Table[1, {i, Length[family["Prop"]]}], Table[0, {i, Length[family["Isp"]]}]];
    seeds = GenTrapezoidSeeds[baseF, family, rank, dots];
    seeds = SeedsDimensionFilter[seeds, OptionValue["DimensionBound"], family];
    
    GenDRRFiniteRelation[family, seeds];
];


(*
GenDRRFiniteRelation[family, seeds] will generate the finite relations (O(eps^0) relations) 
with respect to the given seed integrals, by applying UV subtraction to them.

The seed integrals will be stored in 'cache/family/ExpIBP/finite'. 
The UV counterterm integrals of each UV family will be stored in 'cache/family/ExpIBP/ctuvfamily'.

The parameter 'seeds' should be in the form {d1->{F11, F12, ...}, d2->{F21, F22, ...}, ...}. 
Fij is a IR finite integral in dimension d1 with form F[k1, k2, ...].

The number of integrals in each dimension will be stored in 'cache/family/ExpIBP/drrseedinfo', 
which is of the form {d1->num1, d2->num2, ...}.
*)
GenDRRFiniteRelation[family_?FamilyQ, seeds_List]:= Module[
    {dir, ds, dnum, finite, uvcttable = {}},
    
    If[Head[seeds[[1]] =!= Rule],
        Print["GenDRRFiniteRelation: The formalism of the seeds is not correct."];
        Return[$Aborted];
    ];
    
    dir = FileNameJoin[{CurrentDir[], "cache", ToString[family], "ExpIBP"}];
	If[!DirectoryQ[dir], CreateDirectory[dir]];
	
	ds = seeds[[All, 1]];
	
	Put[Flatten[seeds[[All, 2]]], FileNameJoin[{dir, "finite"}]];
	Put[Thread[ds -> (Length /@ seeds[[All, 2]])], FileNameJoin[{dir, "drrseedinfo"}]];
	
	(*UV counterterms*)
	Do[
	    uvcttable = Join[uvcttable, UVCounterTerm[#, family, "ListedByFamily"->True, "Dimension"->ds[[i]]]& /@ seeds[[i, 2]]],
	    {i, Length[ds]}
	];
	Do[
		Put[(#[[i]]& /@ uvcttable), FileNameJoin[{dir, "ct"<>UVSymbol[family, i]}]],
		{i, Length[family["UVFamily"]]}
	];
]


(* ::Subsection:: *)
(*GenRankSeeds*)


(*
GenRankSeeds[baseF_List, rank_Integer] returns a list of F[k1, k2, ...], 
which is generated from baseF by raising its rank from 0 to 'rank' one by one.

The parameter 'baseF' is a list of F[k1, k2, ...]'s.
*)
GenRankSeeds[baseF_List, family_?FamilyQ, rank_Integer]:= Module[
    {zvars, mono},
    
    zvars = Table[z[i], {i, Length[family["Prop"]] + Length[family["Isp"]]}];
    
    mono = Flatten[Table[GenMono[i, zvars], {i, 0, rank}]];
    
    Return[Flatten[Table[baseF[[i]] * mono, {i, Length[baseF]}]]];
]


(*
GenRankSeeds[baseF_F, rank_Integer] returns a list of F[k1, k2, ...], 
which is generated from baseF by raising its rank from 0 to 'rank' one by one.

The parameter 'baseF' is a F[k1, k2, ...].
*)
GenRankSeeds[baseF_F, family_?FamilyQ, rank_Integer]:= GenRankSeeds[{baseF}, family, rank]


(* ::Subsection:: *)
(*GenDotsSeeds*)


(*
GenDotsSeeds[baseF_List, dots_Integer] returns a list of F[k1, k2, ...], 
which is generated from baseF by raising its dots from 0 to 'dots' one by one.

The parameter 'baseF' is a list of F[k1, k2, ...]'s.
*)
GenDotsSeeds[baseF_List, family_?FamilyQ, dots_Integer]:= Module[
    {zvars, mono},
    
    zvars = Table[z[i], {i, Length[family["Prop"]]}];
    
    mono = Flatten[Table[GenMono[i, zvars], {i, 0, dots}]]^(-1);
    
    Return[Flatten[Table[baseF[[i]] * mono, {i, Length[baseF]}]]];
]


(*
GenDotsSeeds[baseF_F, rank_Integer] returns a list of F[k1, k2, ...], 
which is generated from baseF by raising its dots from 0 to 'rank' one by one.

The parameter 'baseF' is a F[k1, k2, ...].
*)
GenDotsSeeds[baseF_F, family_?FamilyQ, dots_Integer]:= GenRankSeeds[{baseF}, family, dots]


(* ::Subsection:: *)
(*GenTrapezoidSeeds*)


(*
GenDotsSeeds[baseF_List, dots_Integer] returns a list of F[k1, k2, ...], 
which is generated from baseF by raising its rank and dots.
Seeds' ranks and dots are distributed like a trapezoid.

The parameter 'baseF' is a F[k1, k2, ...].
*)
GenTrapezoidSeeds[baseF_F, family_?FamilyQ, rank_Integer, dots_Integer, grad_Integer: 1]:= Module[
    {zvars, dotsBase},
    
    zvars = Table[z[i], {i, Length[family["Prop"]]}];
    
    dotsBase = Table[baseF * GenMono[i, zvars]^(-1), {i, 0, dots}];
    
    Return[Flatten[Table[GenRankSeeds[dotsBase[[i + 1]], family, rank - i * grad], {i, 0, dots}]]];
]


(* ::Subsection:: *)
(*GenSingleSeeds*)


(*
GenSingleSeeds[baseF_List, rank_Integer] returns a list of F[k1, k2, ...], 
which is generated from baseF by raising its rank from 0 to 'rank' one by one.
*)
GenSingleSeeds[baseF_List, family_?FamilyQ, rank_Integer]:= Module[
    {zvars, mono},
    
    zvars = Table[z[i], {i, Length[family["Prop"]] + Length[family["Isp"]]}];
    
    mono = Flatten[Table[GenMono[i, zvars], {i, 0, rank}]];
    
    Return[Flatten[Table[baseF[[i]] * mono, {i, Length[baseF]}]]];
]


(*Generate 6d(for now) finite integrals*)
(*TODO: add dots and higher dimensional seeds*)
GenSingleSeeds[family_?FamilyQ, rank_Integer]:= Module[
    {dir, zlist, ele, finite, uvcttable},
    
    dir = FileNameJoin[{CurrentDir[], "cache", ToString[family], "ExpIBP"}];
	If[!DirectoryQ[dir], CreateDirectory[dir]];
    
    (*z[i]'s*)
    zlist = Table[z[i], {i, Length[family["Prop"]] + Length[family["Isp"]]}];
    
    (*Numerators*)
    (*TODO: modify GenIdealElement*)
    ele = Join[{1}, GenIdealElement[zlist, zlist, rank]];
    
    (*Single integrals*)
    finite = ele * (F@@Join[Table[1, {i, Length[family["Prop"]]}], Table[0, {i, Length[family["Isp"]]}]]);
    
    Return[finite];
];


(* ::Subsection:: *)
(*IRFiniteQ*)


(*
IRFiniteQ[Fexpr, negregions, regionloops, dimension_Integer] returns whether 'Fexpr' is a IR finite integral.

'negregions' is a list of IR regions (asy results) that Fexpr has and each region in the list should have been
shifted to satified that its minimun element is -1.

"regionloops" is a list of integers which are the loop numbers of the regions correspond to the 'negregions'.

'dimension' is the dimension of the integral 'Fexpr' whose default value is 4.
*)
IRFiniteQ[Fexpr_F, negregions_List, regionloops_List, dimension_Integer:4]:= Module[
    {res = True, proppow},
    
    (*Powers of the propagators*)
    proppow = List@@Fexpr[[1;;Length[negregions[[1]]]]];
    
    Do[
        If[regionloops[[i]] * dimension/2 + proppow . negregions[[i]] <= 0, res = False;Break[]],
        {i, Length[negregions]}
    ];
    
    Return[res];
];


(*
IRFiniteQ[Fexpr, family, dimension] returns whether 'Fexpr' of family 'family' is a IR finite integral.

'dimension' is the dimension of the integral 'Fexpr' whose default value is 4.
*)
IRFiniteQ[Fexpr_F, family_?FamilyQ, dimension_Integer:4]:= Module[
    {},
    
    If[IRBurnQ[family] =!= True, 
        Print["Please call BurnIR[" <> ToString[family] <> "] first."];
        Return[$Failed];
    ];
    
    Return[IRFiniteQ[Fexpr, family["AsyRegion"], Length /@ family["IRRegion"], dimension]];
]


(* ::Subsection:: *)
(*SeedsDimensionFilter*)


(*
SeedsDimensionFilter[seeds_List, dBound_Integer, family_?FamilyQ] returns a list of seeds 
which is reorganized from 'seeds' by dimension: {d1->seeds1, d2->seeds2, ...}.
'seedsi' is a list of F[k1, k2, ...] which are exactly IR-finite in dimensino 'di'.

The parameter 'seeds' is a list of F[k1, k2, ...] to be filtered.

The parameter 'dBound' is the up bound of 'di's. If a F in seeds is not IR-finite in dimension
that is not larger than 'dBound', it will be dropped.
*)
SeedsDimensionFilter[seeds_List, dBound_Integer, family_?FamilyQ]:= Module[
    {d = 4, res = {}, remain = seeds, temp},
    
    While[d <= dBound,
        temp = Select[remain, IRFiniteQ[#, family, d]&];
        AppendTo[res, d -> temp];
        remain = Complement[remain, temp];
        d += 2;
    ];
    
    Return[res];
]


(* ::Section:: *)
(*End*)


End[];


EndPackage[];
