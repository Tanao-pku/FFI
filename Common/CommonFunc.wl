(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["FFI`"];


Begin["`Private`"]


(* ::Section::Closed:: *)
(*Linear Independence*)


(* ::Subsection:: *)
(*IndependentVector*)


(*Find the independent vectors*)
(*Return the list of indices of the independent vectors*)
IndependentVector[vec_List]:=Module[
	{res={}, tvec, i=1, j=1},
	tvec=Transpose[vec];
	tvec=RowReduce[tvec];
	While[i<=Length[tvec] && j<=Length[tvec[[1]]],
		If[tvec[[i, j]]==0,
			j++,
			AppendTo[res, j];i++;
		]
	];
	Return[res]
]


(* ::Subsection:: *)
(*IndependentCombination*)


(*Find maximal linear independent elements*)
(*Return the list of independent elements*)
IndependentCombination[cb_List]:=Module[
	{var, vec, ind},
	var=Variables[cb];
	vec=Table[Coefficient[cb[[i]],var[[j]]], {i, Length[cb]}, {j, Length[var]}];
	ind=IndependentVector[vec];
	Return[cb[[#]]&/@ind]
];
IndependentCombination[{}]:={};


(* ::Subsection::Closed:: *)
(*IndependentPoly*)


(*Find independent polynomials of a degree*)
(*Return the list of independent polynomials*)
Options[IndependentPoly] = {"Parameters"->{}};

IndependentPoly[polys_List, vars_List, deg_Integer, OptionsPattern[]]:=Module[
	{mono, A, inds, numericpolys},
	
	(*deal with special cases*)
	If[Length[polys]==0, Return[{}]];
	If[deg==0, Return[{polys[[1]]}]];
	
	(*monomials of degree deg*)
	mono = FFI`GenMono[deg, vars];
	
	(*numeric polys*)
	numericpolys = polys/.Thread[OptionValue["Parameters"]->RandomReal[{0, 1}, Length[OptionValue["Parameters"]]]];
	
	(*coefficient matrix*)
	A = Table[Coefficient[numericpolys[[i]], mono[[j]]], {i, Length[polys]}, {j, Length[mono]}];
	
	(*independent indices*)
	inds = IndependentVector[A];
	
	Return[polys[[#]]&/@inds]
]


(* ::Section:: *)
(*SP And Prop*)


(* ::Subsection::Closed:: *)
(*GenSP*)


GenSP[p_List]:=Flatten[Table[p[[i]]*p[[j]], {i, 1, Length[p]}, {j, i, Length[p]}]];
GenSP[p_List, q_List]:=Flatten[Table[p[[i]]*q[[j]], {i, 1, Length[p]}, {j, 1, Length[q]}]];


(* ::Subsection::Closed:: *)
(*SPToProp*)


(*express the sp's to prop's\:ff0cprop's don't need to be complete*)

Options[SPToProp] = {"SPForm"->False};

SPToProp[prop_,loop_,out_,OptionsPattern[]]:=Module[
	{sp,Sp,s,slist,zlist,sptos,sol},
	sp=Flatten[{Table[loop[[i]]loop[[j]],{i,1,Length[loop]},{j,i,Length[loop]}],Table[loop[[i]]out[[j]],{i,1,Length[loop]},{j,1,Length[out]}]}];
	Sp=Flatten[{Table[FFI`SP[loop[[i]],loop[[j]]],{i,1,Length[loop]},{j,i,Length[loop]}],Table[FFI`SP[loop[[i]],out[[j]]],{i,1,Length[loop]},{j,1,Length[out]}]}];
	slist=Table[s[i],{i,Length[sp]}];
	zlist=Table[FFI`z[i],{i,Length[prop]}];
	sptos=Thread[sp->slist];
	sol=Flatten[Solve[Thread[(Expand[prop]/.sptos)==zlist],slist]];
	If[OptionValue["SPForm"], Return[sol/.s[i_]:>Sp[[i]]], Return[sol/.s[i_]:>sp[[i]]]];
];

SPToProp[family_?FamilyQ, OptionsPattern[]]:= SPToProp[Expand[Join[family["Prop"], family["Isp"]]]/.family["Replace"], family["Loop"], family["Leg"], "SPForm"->OptionValue["SPForm"]];


(* ::Subsection::Closed:: *)
(*PropToProp*)


PropToProp[fam1_?FamilyQ, fam2_?FamilyQ]:= Module[
	{res, prop1},
	prop1 = Expand[Join[fam1["Prop"], fam1["Isp"]]]/.fam1["Replace"];
	res = Table[FFI`z[i]->prop1[[i]], {i, Length[prop1]}]/.SPToProp[Join[fam2["Prop"], fam2["Isp"]], fam2["Loop"], fam2["Leg"]];
	Return[res];
]


(* ::Subsection::Closed:: *)
(*SquareProp*)


(*express the propagators in form of q^2-m^2*)
SquareProp[prop_, loop_List, replace_List]:= Module[
	{lvar, q},
	
	(*find out which loop this prop contains*)
	lvar = Intersection[loop, Variables[prop]];
	
	(*expression of q*)
	q = Expand[D[prop, lvar[[1]]]/2];
	
	(*result*)
	Return[(q^2 + Expand[prop - q^2])/.replace]
]


(* ::Subsection::Closed:: *)
(*SPPropAndIsp*)


(*express prop and isp in form of SP*)
SPPropAndIsp[family_?FamilyQ]:= Module[
	{tosp, loop, out},
	loop = family["Loop"];
	out = family["Leg"];
	tosp = Flatten[{Table[loop[[i]]*loop[[j]]->SP[loop[[i]], loop[[j]]], {i, 1, Length[loop]}, {j, i, Length[loop]}], Table[loop[[i]]*out[[j]]->SP[loop[[i]], out[[j]]], {i, 1, Length[loop]}, {j, 1, Length[out]}]}];
	Return[Expand[{family["Prop"], family["Isp"]}]/.family["Replace"]/.tosp]
]


(* ::Section::Closed:: *)
(*Gram Determinant*)


Gram[p_List, q_List, family_?FamilyQ]:= Module[
	{A},
	
	(*matrix*)
	A = Table[p[[i]]*q[[j]], {i, Length[p]}, {j, Length[q]}];
	A = A/.SPToProp[family]/.family["Replace"];
	
	Return[Expand[Det[A]]]
]

Gram[p_List, family_?FamilyQ]:= Gram[p, p, family]


(* ::Section::Closed:: *)
(*Set*)


(* ::Subsection:: *)
(*PowerSet*)


(*Return the set {(a1,a2,...,an)|ai\[Element]S} with a given set S and an integer n*)
PowerSet[list_List, n_Integer]:=Module[
	{A, lastres},
	lastres=PowerSet[list, n-1];
	A=Table[Table[Append[lastres[[j]], list[[i]]], {j, 1, Length[lastres]}], {i, 1, Length[list]}];
	Return[Join@@A]
]
PowerSet[list_List, 1]:=Table[{list[[i]]}, {i, 1, Length[list]}]


(* ::Section::Closed:: *)
(*Find Parameters Of A Family*)


(*find all parameters*)
FindParameter[family_?FamilyQ]:=Module[
	{para},
	para = Variables[{family["Prop"], #[[2]]&/@family["Replace"]}];
	para = Complement[para, Join[family["Loop"], family["Leg"]]];
	Return[para]
]


(* ::Section:: *)
(*Polynomial Functions*)


(* ::Subsection::Closed:: *)
(*Deg*)


(*Deg for monomials*)
Deg[mono_, vars_List]:= Plus@@Table[Exponent[mono, vars[[i]]], {i, 1, Length[vars]}];

(*Deg for polynomials*)
Deg[poly_Plus, vars_List]:= Max[Table[Deg[poly[[i]], vars], {i, 1, Length[poly]}]];


(* ::Subsection::Closed:: *)
(*GenMono*)


GenMono[deg_, vars_List]:= Module[
	{sol, res},
	sol = NonNegEq[deg, Length[vars]];
	res = Table[Times@@Thread[vars ^ sol[[i]]], {i, 1, Length[sol]}];
	Return[res];
]


(* ::Subsection::Closed:: *)
(*NonNegEq*)


NonNegEq[x_, 1]:= {{x}};
NonNegEq[x_, n_]:= Module[
	{sol, res = {}},
	Do[
		sol = NonNegEq[x - i, n - 1];
		res = Join[res, Table[Join[{i}, sol[[j]]], {j, 1, Length[sol]}]],
		{i, 0, x}
	];
	Return[res];
]


(* ::Subsection::Closed:: *)
(*GenIdealElement*)


Options[GenIdealElement] = {"Parameters"->{}};

(*generate linear elements of an ideal up to a given degree*)
GenIdealElement[gens_List, vars_List, rank_Integer, OptionsPattern[]]:=Module[
	{gen, ele},
	
	(*generators of each degree*)
	Do[gen[k] = Select[gens, Deg[#, vars] == k&], {k, 0, rank}];
	
	Do[
		(*elements of deg i*)
		ele[i] = {};
		Do[
			ele[i] = Join[ele[i], Flatten[Outer[Times, GenMono[i-j, vars], gen[j]]]],
			{j, 0, i}
		];
		ele[i] = IndependentPoly[ele[i], vars, i, "Parameters"->OptionValue["Parameters"]],
		{i, 0, rank}
	];
	
	Return[Flatten[Table[ele[i], {i, rank}]]]
];


(* ::Subsection::Closed:: *)
(*BaikovPoly*)


BaikovPoly[family_?FamilyQ]:= Module[
    {q},
    
    q = Join[family["Loop"], family["Leg"]];
    
    Return[Table[q[[i]]*q[[j]]/.FFI`Private`SPToProp[family]/.family["Replace"], {i, Length[q]}, {j, Length[q]}] //Det //Expand];
]


(* ::Subsection::Closed:: *)
(*UPoly*)


UPoly[prop_List, loop_List]:= Module[
    {A, sumprop},
    
    (*Sum and expand the prop*)
    sumprop = Expand[Table[FFI`z[i], {i, Length[prop]}] . prop];
    
    (*Coefficient matrix*)
    A = Table[Coefficient[sumprop, loop[[i]] * loop[[j]]] * If[i == j, 1, 1/2], {i, Length[loop]}, {j, Length[loop]}];
    
    Return[Det[A]];
];


UPoly[family_?FamilyQ]:= UPoly[family["Prop"], family["Loop"]];


(* ::Section:: *)
(*Directory*)


(* ::Subsection:: *)
(*CurrentDir*)


CurrentDir[]:= DirectoryName[If[$FrontEnd===Null,$InputFileName,NotebookFileName[]]];


(* ::Section:: *)
(*End*)


End[]


EndPackage[];
