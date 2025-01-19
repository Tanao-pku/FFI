(* ::Package:: *)

(* ::Input:: *)
(*$Failed*)


(* ::Section:: *)
(*Begin*)


BeginPackage["FFI`"];


FindUVRegion::usage="FindUVRegion[family] gives the UV region of a family. 
FindUVRegion[loopcb_List, loop_List] gives the UV region that corresponds to the given loop momenta and their combinations";
GenUVFamily::usage="GenUVFamily[family, m] defines all UV family with respect to a given family, and m is the mass added to the propagator";
BurnUV::usage="BurnUV[family, m] will generate all information for UV subtraction which includes UV family and so on, and m is the mass added to the propagator. It returns the list of UV family name"
UVCounterTerm::usge="UVCounterTerm[Fexpr, family] gives the UV counter term of the given Fexpr which is linear combinations of some F[__], and these F's must be in
in the same family";
UVDegree::usage="UVDegree[zpoly, family] gives the power counting result of zpoly in each family's UV region, zpoly is a polynomial of z[i]'s"


Begin["`Private`"]


(* ::Section:: *)
(*Analyze UV Region*)


(* ::Subsection::Closed:: *)
(*FindUVRegion*)


FindUVRegion[loopcb_List, loop_List]:=Module[
	{result={},subset,solvedloop,sol},
	subset=Subsets[loopcb];
	Do[
		Off[Solve::svars];
		sol=Flatten[Solve[Thread[subset[[i]]==0],loop]];
		solvedloop=#[[1]]&/@sol;
		On[Solve::svars];
		If[Length[solvedloop]<Length[loop] && !MemberQ[Complement[loopcb,subset[[i]]]/.sol, 0],
			AppendTo[result, {Complement[loop, solvedloop], subset[[i]]}]
		],
	{i, Length[subset]}
	];
	Return[result//Reverse]
];

FindUVRegion[family_?FamilyQ]:= Module[
	{loopcb, lvar},
	loopcb = family["PropMom"]/.Thread[family["Leg"]->0];
	
	Do[
		lvar = Intersection[family["Loop"], Variables[loopcb[[i]]]];
		If[Coefficient[loopcb[[i]], lvar[[1]]] < 0, loopcb[[i]] = -loopcb[[i]]],
		{i, Length[loopcb]}
	];
	loopcb = Union[loopcb];
	
	Return[FindUVRegion[loopcb, family["Loop"]]]
]


(* ::Subsection::Closed:: *)
(*GenUVChain*)


(*generate all chains like r1<r2<r3...*)
GenUVChain[region_List, L_Integer]:=Module[
	{chain},
	Do[
		If[i==1, chain[1]=Table[{j}, {j, Length[region]}];Continue[]];
		chain[i]={};
		Do[
			If[SubsetQ[region[[chain[i-1][[j, -1]], 2]],region[[k, 2]]] && k!=chain[i-1][[j, -1]],
				AppendTo[chain[i], Append[chain[i-1][[j]], k]]
			],
			{j,Length[chain[i-1]]},
			{k,Length[region]}
		],
	{i, L}
	];
	Return[Join@@Table[chain[i], {i, L}]]
];


(* ::Subsection::Closed:: *)
(*UVExpOffset*)


UVExpOffset[family_?FamilyQ, m_]:= Module[
	{fbp, lsp, loop, A, offset},
	
	loop = family["Loop"];
	
	(*full bubble prop*)
	fbp = Union[Expand[Power[#, 2]&/@(family["PropMom"]/.Thread[family["Leg"]->0])]];
	(*generate all loop's sp*)
	lsp = Flatten[Table[loop[[i]]*loop[[j]],{i, 1, Length[loop]}, {j, i, Length[loop]}]];
	
	(*introduce the same mass for all props*)
	(*coefficient matrix of sp's*)
	A = Table[Coefficient[Expand[fbp[[i]]], lsp[[j]]], {i, 1, Length[fbp]}, {j, 1, Length[lsp]}];
	(*expansion offset*)
	offset = LinearSolve[A, Table[-m^2, {i, Length[fbp]}]];
	
	(*li^2->li^2-m^2, li.lj->li.lj*)
	(*offset = Flatten[Table[If[i==j, -m^2, 0],{i, 1, Length[loop]}, {j, i, Length[loop]}]];*)
	
	Return[Thread[lsp->offset]];
]


(* ::Subsection::Closed:: *)
(*UVScaleProp*)


(*express the propagators and isp's in form of large parameter Y in a UV region*)
UVScaleProp[UVregion_List, prop_List, isp_List, Y_, offset_]:=Module[
	{loop, largel, restl, restlrule, d, dlist, scalerule, scaleprop, scaleisp, small, offset1, offset2, lsp, llsp},
	(*loop momenta*)
	loop = Variables[UVregion];
	largel = UVregion[[1]];
	restl = Complement[loop, largel];
	small = IndependentCombination[UVregion[[2]]];
	
	(*d[i]'s*)
	dlist = Table[d[i], {i, Length[small]}];
	
	(*turn restl into d[i]'s*)
	restlrule = Flatten[Solve[Thread[small==dlist], restl]];

	(*generate all loop's sp*)
	lsp = Flatten[Table[loop[[i]]*loop[[j]],{i, 1, Length[loop]}, {j, i, Length[loop]}]];
	
	(*this step is to calculate the substitution rule of sp's that include largel*)
	(*generate sp of largel and d[i]*)
	llsp = Flatten[{Table[largel[[i]]*largel[[j]],{i, 1, Length[largel]}, {j, i, Length[largel]}], Table[largel[[i]]*d[j],{i, 1, Length[largel]}, {j, 1, Length[dlist]}]}];
	(*expansion offset of llsp*)
	offset1 = lsp/.offset;
	offset2 = Table[Coefficient[Expand[llsp[[i]]/.Thread[dlist->small]], lsp[[j]]], {i, Length[llsp]}, {j, Length[lsp]}] . offset1;
	
	(*write large parameter Y explicitly*)
	scaleprop = Expand[prop/.restlrule]/.Thread[llsp->(llsp-offset2)]/.Thread[largel->Y*largel]/.Thread[llsp->(llsp+offset2)];
	scaleisp=Expand[isp/.restlrule]/.Thread[llsp->(llsp-offset2)]/.Thread[largel->Y*largel]/.Thread[llsp->(llsp+offset2)];
	Return[Expand[{scaleprop,scaleisp}/.Thread[dlist->small]]];
];


(* ::Subsection::Closed:: *)
(*UVScaleRule*)


UVScaleRule[UVregion_List, Y_]:= Module[
	{loop, largel, restl, restlrule, small, dlist, d, resright},
	
	(*loop momenta*)
	loop = Variables[UVregion];
	largel = UVregion[[1]];
	restl = Complement[loop, largel];
	small = IndependentCombination[UVregion[[2]]];
	
	(*d[i]'s*)
	dlist = Table[d[i], {i, Length[small]}];
	
	(*turn restl into d[i]'s*)
	restlrule = Flatten[Solve[Thread[small==dlist], restl]];
	
	resright = loop/.restlrule/.Thread[largel->Y*largel]/.d[i_]:>small[[i]];
	
	Return[Thread[loop->resright]]
];


(* ::Subsection::Closed:: *)
(*UVNewProp*)


(*determine the propagators and isp's according to the result of UVScakeRule*)
UVNewProp[scaleprop_List, loop_List, out_List, Y_]:=Module[
	{sp, leftprop, Yexplist, solvedsp, isp},
	
	(*all SP's*)
	sp=Flatten[{Table[loop[[i]]loop[[j]],{i,1,Length[loop]},{j,i,Length[loop]}],Table[loop[[i]]out[[j]],{i,1,Length[loop]},{j,1,Length[out]}]}];
	(*exponents of Y in scaleprop*)
	Yexplist=Exponent[scaleprop,Y];
	(*propagators that will be left in the UV region*)
	leftprop=Union[Expand[Table[Coefficient[scaleprop[[i]],Y,Yexplist[[i]]],{i,Length[scaleprop]}]]];
	
	(*complete the isp*)
	Off[Solve::svars];
	solvedsp=#[[1]]&/@SPToProp[leftprop,loop,out];
	On[Solve::svars];
	isp=Complement[sp, solvedsp];
	
	Return[{leftprop,isp}]
]


(* ::Subsection::Closed:: *)
(*ToUVProp*)


(*transform a family's prop and isp into a UV family's prop and isp*)
ToUVProp[family_?FamilyQ, uvfam_?UVFamilyQ, Y_, offset_]:=Module[
	{scaleprop, scaleisp, sptouvprop, result},
	
	{scaleprop, scaleisp} = UVScaleProp[uvfam["UVInfo"], family["Prop"], family["Isp"], Y, offset]/.family["Replace"];
	
	sptouvprop = SPToProp[Join[uvfam["Prop"], uvfam["Isp"]], uvfam["Loop"], uvfam["Leg"]]/.uvfam["Replace"];
	
	result = Expand[{scaleprop, scaleisp}/.sptouvprop];
	
	(*test*)
	Return[result(*/.Y^2family["UVMass"]^2->family["UVMass"]^2/.Y family["UVMass"]^2->family["UVMass"]^2*)];
]


(* ::Section:: *)
(*Generate UV Family*)


(* ::Subsection::Closed:: *)
(*GenUVFamily*)


Options[GenUVFamily] = {"SquareProp" -> True};

GenUVFamily[family_?FamilyQ, m_, OptionsPattern[]]:=Module[
	{uvregions, scaleprop, uvprop, uvisp, scaleisp, prop, isp, loop, out, replace, Y, offset, chains, c},
	
	(*find UV regions and generate UV chains*)
	uvregions = FindUVRegion[family];
	Unprotect[family];
	family["UVChain"] = GenUVChain[family["UVRegion"], Length[family["Loop"]]];
	Protect[family];
	
	prop = family["Prop"];
	isp = family["Isp"];
	loop = family["Loop"];
	out = family["Leg"];
	replace = family["Replace"];
	
	Unprotect[family];
	family["UVRegion"] = uvregions;
	family["UVMass"] = m;
	Protect[family];
	
	offset = UVExpOffset[family, m];
	
	(*it's necessary to define a UV family for each UV chain*)
	Unprotect[family];
	family["ChainFamily"] = <||>;
	family["UVFamily"] = {};
	
	(*UV family for chains of length 1*)
	Do[
		{scaleprop, scaleisp} = UVScaleProp[uvregions[[i]], prop, isp, Y, offset]/.replace;
		{uvprop, uvisp} = UVNewProp[scaleprop, loop, out, Y];
		If[FFI`$PrintLevel == 0,
			Print["------"];
			Print["UV region "<>ToString[i]<>" :"<>" (chain {"<>ToString[i]<>"})"];
			Print["Large loop momenta: ", uvregions[[i, 1]]];
			Print["Small momenta: ", uvregions[[i, 2]]];
		];
		UVFamilySymbol[family, i]["UVInfo"] = uvregions[[i]];
		UVFamilyQ[UVFamilySymbol[family, i]] = True;
		UVFamilySymbol[family, i]["UVMass"] = m;
		UVFamilySymbol[family, i]["Chain"] = {i};
		AppendTo[family["ChainFamily"], {i}->UVFamilySymbol[family, i]];
		AppendTo[family["UVFamily"], UVFamilySymbol[family, i]];
		DefineFamily[UVFamilySymbol[family, i], uvprop, uvisp, {}, loop, out, family["NullMom"], replace, "SquareProp"->OptionValue["SquareProp"]],
		{i, Length[uvregions]}
	];
	
	(*UV family for chains of length larger than 1*)
	Do[
		chains = Select[family["UVChain"], Length[#]==i&];
		Do[
			c = chains[[j]];
			{scaleprop, scaleisp} = UVScaleProp[uvregions[[c[[-1]]]], family["ChainFamily"][c[[1;;-2]]]["Prop"], family["ChainFamily"][c[[1;;-2]]]["Isp"], Y, offset]/.replace;
			{uvprop, uvisp} = UVNewProp[scaleprop, loop, out, Y];
			
			(*check if this is a new family*)
			If[SubsetQ[Expand[family["ChainFamily"][c[[2;;-1]]]["Prop"]]/.replace, Expand[uvprop]/.replace],
				AppendTo[family["ChainFamily"], c->family["ChainFamily"][c[[2;;-1]]]];
				Continue[];
			];
			
			(*else*)
			If[FFI`$PrintLevel == 0,
				Print["------"];
				Print["Chain: "<>ToString[c]];
			];
			UVFamilySymbol[family, c]["UVInfo"] = uvregions[[c[[-1]]]];
			UVFamilyQ[UVFamilySymbol[family, c]] = True;
			UVFamilySymbol[family, c]["UVMass"] = m;
			UVFamilySymbol[family, c]["Chain"] = c;
			AppendTo[family["ChainFamily"], c->UVFamilySymbol[family, c]];
			AppendTo[family["UVFamily"], UVFamilySymbol[family, c]];
			DefineFamily[UVFamilySymbol[family, c], uvprop, uvisp, {}, loop, out, family["NullMom"], replace, "SquareProp"->OptionValue["SquareProp"]];,
			{j, Length[chains]}
		]
		,
		{i, 2, Length[family["Loop"]]}
	];
	
	UVFamilyGenQ[family] = True;
	Protect[family];
	
	Return[family["UVFamily"]];
]


(* ::Subsection::Closed:: *)
(*UVFamilySymbol*)


UVFamilySymbol[family_?FamilyQ, id_Integer]:=Module[
	{},
	Evaluate[Symbol["Global`"<>ToString[family]<>"uv"<>ToString[id]]]
];

UVFamilySymbol[family_?FamilyQ, chain_List]:=Module[
	{str},
	(*length 1*)
	If[Length[chain]==1, Return[UVFamilySymbol[family, chain[[1]]]]];
	
	(*length larger than 1*)
	str = "Global`"<>ToString[family]<>"uv"<>ToString[chain[[1]]];
	Do[
		str = str<>"to"<>ToString[chain[[i]]],
		{i, 2, Length[chain]}
	];
	Return[Evaluate[Symbol[str]]]
];


(* ::Subsection::Closed:: *)
(*UVSymbol*)


UVSymbol[family_?FamilyQ, id_Integer]:= StringDrop[ToString[family["UVFamily"][[id]]], StringLength[ToString[family]]];


(* ::Subsection:: *)
(*BurnUV*)


Options[BurnUV] = {"SquareProp" -> True};

(*generate all information for UV subtraction*)
BurnUV[family_?FamilyQ, m_, OptionsPattern[]]:=Module[
	{chain1, chain2, c, offset},
	
	If[UVBurnQ[family] === True, Return[family["UVFamily"]]];
	
	(*generate uv family first*)
	If[UVFamilyGenQ[family] =!= True, GenUVFamily[family, m, "SquareProp"->OptionValue["SquareProp"]]];
	
	(*compute offset*)
	offset = UVExpOffset[family, family["UVMass"]];
	
	(*select the chain whose length is 1*)
	chain1 = Table[{i}, {i, Length[family["UVRegion"]]}];
	
	(*generate all the transformation from a uv propagator to another*)
	Unprotect[family];
	family["UVTrans"] = <||>;
	(*chain of length 1*)
	Do[
		AppendTo[family["UVTrans"], chain1[[i]]->ToUVProp[family, UVFamilySymbol[family, chain1[[i, 1]]], FFI`Y, offset]],
		{i, Length[chain1]}
	];
	(*chain of length larger than 1*)
	Do[
		(*select the chain whose length is larger than 1*)
		chain2 = Select[family["UVChain"], Length[#]==i&];
		Do[
			c = chain2[[j]];
			AppendTo[family["UVTrans"], c->ToUVProp[family["ChainFamily"][c[[1;;-2]]], family["ChainFamily"][c], FFI`Y, offset]],
			{j, Length[chain2]}
		],
		{i, 2, Length[family["Loop"]]}
	];
	
	UVBurnQ[family] = True;
	Protect[family];
	
	Return[family["UVFamily"]];
]


(* ::Section:: *)
(*UV Counter Term*)


(* ::Subsection:: *)
(*UVCounterTerm*)


(*Options[UVCounterTerm] = {"ListedByFamily" -> False, "UVDegree" -> {}};

UVCounterTerm[Fexpr_, family_?FamilyQ, OptionsPattern[]]:=Module[
	{CTass, chains, zexpr, num = Length[family["Prop"]] + Length[family["Isp"]], result, uvid, uvdegree, degass},
	(*check whether this family is UVBurned*)
	If[UVBurnQ[family] =!= True, Print["Please call BurnUV first!"];Return[]];
	
	(*CTass is used to restore each counter term*)
	CTass = Association[Table[family["UVChain"][[i]]->0, {i, Length[family["UVChain"]]}]];
	
	(*Turn F[i__] into products of z's*)
	zexpr = Fexpr/.FFI`F[i__]:>Times@@Table[FFI`z[k]^(-{i}[[k]]), {k, Length[{i}]}];
	
	(*how many orders should be expanded in each region*)
	degass = Association[OptionValue["UVDegree"]];
	Do[
		uvdegree[i] = If[MemberQ[Keys[degass], i], degass[i], 4*Length[family["UVRegion"][[i, 1]]]],
		{i, Length[family["UVRegion"]]}
	];
	
	(*calculate the counter term from length 1 element to length L element in uv chain*)
	Do[
		chains = Select[family["UVChain"], Length[#]==i&];
		If[
			i==1,
			Do[
				CTass[chains[[j]]] = Normal[Series[zexpr/.Thread[Table[FFI`z[k], {k, num}]->Flatten[family["UVTrans"][chains[[j]]]]], {FFI`Y, Infinity, uvdegree[chains[[j, 1]]]}]]/.FFI`Y->1,
				{j, Length[chains]}
			],
			(*i>1*)
			Do[
				CTass[chains[[j]]] = Normal[Series[CTass[chains[[j]][[1;;-2]]]/.Thread[Table[FFI`z[k], {k, num}]->Flatten[family["UVTrans"][chains[[j]][[-2;;-1]]]]], {FFI`Y, Infinity, uvdegree[chains[[j, -1]]]}]]/.FFI`Y->1,
				{j, Length[chains]}
			];
		],
		{i, Length[family["Loop"]]}
	];
	
	(*turn polynomial of z's into form of F[k__]*)
	Do[
		CTass[family["UVChain"][[i]]] = Expand[CTass[family["UVChain"][[i]]]*(FFI`F@@Table[0, {j, num}])],
		{i, Length[family["UVChain"]]}
	];
	
	(*total uv counter term*)
	If[!OptionValue["ListedByFamily"],
		result = 0;
		Do[
			result = result + ((-1)^(Length[family["UVChain"][[i]]]-1) * CTass[family["UVChain"][[i]]]/.FFI`F[k__]:>FFI`F[UVFamilySymbol[family, family["UVChain"][[i, -1]]], {k}]),
			{i, Length[family["UVChain"]]}
		];
		Return[result];
	];
	
	(*if OptionValue["ListedByFamily"]=True, return a counter term list*)
	result = Table[0, {i, Length[family["UVRegion"]]}];
	Do[
		uvid = family["UVChain"][[i, -1]];
		result[[uvid]] = result[[uvid]] + (-1)^(Length[family["UVChain"][[i]]]-1) * CTass[family["UVChain"][[i]]],
		{i, Length[family["UVChain"]]}
	];
	
	Return[result];
]*)


Options[UVCounterTerm] = {"ListedByFamily" -> False, "UVDegree" -> {}, "Dimension" -> 4};

(*Fexpr can contain SP*)
UVCounterTerm[Fexpr_, family_?FamilyQ, OptionsPattern[]]:=Module[
	{CTass, chains, zexpr, scalerule, num = Length[family["Prop"]] + Length[family["Isp"]], result, uvid, uvdegree, degass},
	(*check whether this family is UVBurned*)
	If[UVBurnQ[family] =!= True, Print["Please call BurnUV first!"];Return[]];
	
	(*CTass is used to restore each counter term*)
	CTass = Association[Table[family["UVChain"][[i]]->0, {i, Length[family["UVChain"]]}]];
	
	(*Turn F[i__] into products of z's*)
	zexpr = Fexpr/.FFI`F[i__]:>Times@@Table[FFI`z[k]^(-{i}[[k]]), {k, Length[{i}]}];
	
	(*how many orders should be expanded in each region*)
	degass = Association[OptionValue["UVDegree"]];
	Do[
		uvdegree[i] = If[MemberQ[Keys[degass], i], degass[i], OptionValue["Dimension"]*Length[family["UVRegion"][[i, 1]]]],
		{i, Length[family["UVRegion"]]}
	];
	
	(*calculate the counter term from length 1 element to length L element in uv chain*)
	Do[
		chains = Select[family["UVChain"], Length[#]==i&];
		
		If[
			i==1,
			Do[
				scalerule = UVScaleRule[family["UVRegion"][[chains[[j, 1]]]], FFI`Y];
				CTass[chains[[j]]] = Normal[Series[zexpr/.scalerule/.Thread[Table[FFI`z[k], {k, num}]->Flatten[family["UVTrans"][chains[[j]]]]], {FFI`Y, Infinity, uvdegree[chains[[j, 1]]]}]]/.FFI`Y->1,
				{j, Length[chains]}
			],
			(*i>1*)
			Do[
				scalerule = UVScaleRule[family["UVRegion"][[chains[[j, -1]]]], FFI`Y];
				CTass[chains[[j]]] = Normal[Series[CTass[chains[[j]][[1;;-2]]]/.scalerule/.Thread[Table[FFI`z[k], {k, num}]->Flatten[family["UVTrans"][chains[[j]]]]], {FFI`Y, Infinity, uvdegree[chains[[j, -1]]]}]]/.FFI`Y->1,
				{j, Length[chains]}
			];
		],
		{i, Length[family["Loop"]]}
	];
	
	(*turn polynomial of z's into form of F[k__]*)
	Do[
		CTass[family["UVChain"][[i]]] = Expand[CTass[family["UVChain"][[i]]]*(FFI`F@@Table[0, {j, num}])],
		{i, Length[family["UVChain"]]}
	];
	
	(*total uv counter term*)
	If[!OptionValue["ListedByFamily"],
		result = 0;
		Do[
			result = result + ((-1)^(Length[family["UVChain"][[i]]]-1) * CTass[family["UVChain"][[i]]]/.FFI`F[k__]:>FFI`F[family["ChainFamily"][family["UVChain"][[i]]], {k}]),
			{i, Length[family["UVChain"]]}
		];
		Return[result];
	];
	
	(*if OptionValue["ListedByFamily"]=True, return a counter term list*)
	result = Table[0, {i, Length[family["UVFamily"]]}];
	Do[
		uvid = Flatten[Position[family["UVFamily"], family["ChainFamily"][family["UVChain"][[i]]]]];
		result[[uvid]] = result[[uvid]] + (-1)^(Length[family["UVChain"][[i]]]-1) * CTass[family["UVChain"][[i]]],
		{i, Length[family["UVChain"]]}
	];
	
	Return[result];
]


(* ::Section:: *)
(*UV Power Counting*)


(* ::Subsection:: *)
(*UVDegree*)


UVDegree[zpoly_, family_?FamilyQ]:= Module[
	{spprop, degree, loop, largel, restl, small, dlist, d, restlrule, sppoly},
	degree = Table[0, {i, Length[family["UVRegion"]]}];
	spprop = Flatten[SPPropAndIsp[family]];
	
	loop = family["Loop"];
	
	sppoly = zpoly/.Table[FFI`z[i]->spprop[[i]], {i, Length[spprop]}];
	
	Do[
		largel = family["UVRegion"][[i, 1]];
		restl = Complement[loop, largel];
		small = IndependentCombination[family["UVRegion"][[i, 2]]];
	
		(*d[i]'s*)
		dlist = Table[d[i], {i, Length[small]}];
	
		(*turn restl into d[i]'s*)
		restlrule = Flatten[Solve[Thread[small==dlist], restl]];
		
		degree[[i]] = Exponent[sppoly/.restlrule/.Thread[largel->FFI`Y*largel], FFI`Y],
		{i, Length[degree]}
	];
	
	Return[degree];
];


(* ::Section::Closed:: *)
(*Expand in UV Region*)


UVExpand[FFI`F[family_?FamilyQ, inds_List], uvid_Integer]:=Module[
	{target, scaleprop, scaleisp, uvprop, uvisp, sptouvprop, Y, result, uvregion, prop, isp, m, loop, out, replace, offset},
	
	uvregion = family["UVRegion"][[uvid]];
	prop = family["Prop"];
	isp = family["Isp"];
	m = family["UVMass"];
	loop = family["Loop"];
	out = family["Leg"];
	replace = family["Replace"];
	offset = UVExpOffset[family, family["UVMass"]];
	
	{scaleprop, scaleisp} = UVScaleProp[uvregion, prop, isp, Y, offset]/.replace;
	{uvprop, uvisp} = UVNewProp[scaleprop, loop, out, Y];
	sptouvprop = SPToProp[Join[uvprop,uvisp], loop, out]/.replace;
	target=Times@@Table[1/FFI`z[i]^inds[[i]], {i, Length[inds]}];
	result=Normal[Series[target/.FFI`z[i_]:>(scaleprop[[i]]/.sptouvprop), {Y, Infinity, 4*Length[uvregion[[1]]]}]]/.Y->1;
	(*Print[Join[uvprop,uvisp]];*)
	Return[Expand[result*(FFI`F@@Table[0, {i, Length[prop] + Length[isp]}])]/.FFI`F[i__]:>FFI`F[Evaluate[Symbol[ToString[family]<>"uv"<>ToString[uvid]]],{i}]]
]


(* ::Section:: *)
(*End*)


End[]


EndPackage[];
