(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["FFI`"];


AnalyzeAsyRes::usage = "AnalyzeAsyRes[asyres, prop, propmom, loop, nullp, replace] turns the result of asy.m into a region of loop momenta. 
AnalyzeAsyRes[asyres, family] turns the result of asy.m into a region of loop momenta.";
AsyResToIdeal::usage = "AsyResToIdeal[asyres, deno, prop, isp, propmom, loop, out, nullp, replace] computes the ideal of numerator with respect to a result of asy.m and given denominator powers deno. 
AsyResToIdeal[asyres, deno, family] computes the ideal of numerator with respect to a result of asy.m and given denominator powers deno.";
FiniteIdeal::usage = "FiniteIdeal[family, deno] computes the ideal for finite Feynman integrals corresponds to the denominator powers deno. 
FiniteIdeal[family] computes the ideal corresponds to the topsector corner integral.";
OBFiniteIdeal::usage = "OBFiniteIdeal[family, deno] computes the ideal for finite Feynman integrals corresponds to the denominator powers deno, while the variables for the ideal is \!\(\*OverscriptBox[SubscriptBox[\(l\), \(i\)], \(^\)]\)\[CenterDot]\!\(\*OverscriptBox[SubscriptBox[\(l\), \(j\)], \(^\)]\) and \!\(\*SubscriptBox[\(l\), \(i\)]\)\[CenterDot]\!\(\*SubscriptBox[\(v\), \(j\)]\). It can be transformed into propagators by using function OBToProp.
OBFiniteIdeal[family] computes the ideal corresponds to the topsector corner integral.";
OBToProp::usage = "OBToProp[family] gives the transformation relations from OB basis to propagators."


Begin["`Private`"]


(* ::Section:: *)
(*Compute the ideal*)


(* ::Subsection::Closed:: *)
(*SingularEliminateIdeal*)


(*Compute the elimination ideal*)
(*eg: SingularEliminateIdeal[{y^2l2,y^2l2-2pl y},{l2,pl},y,2]\:ff0coutput: {z[1],z[2]^2}*)
Options[SingularEliminateIdeal] = {"Parameters" -> {}};

SingularEliminateIdeal[regionprop_List, regionvar_List, y_, rank_Integer, OptionsPattern[]]:=Module[
	{zlist, wlist, code, output},
	zlist = Table["z"[i], {i, 1, Length[regionprop]}];
	wlist = Table["w"[i], {i, 1, Length[regionvar]}];
	code = SingularDefineRing[{"z", Length[zlist]}, {"w", Length[wlist]}, y, "Parameters" -> OptionValue["Parameters"]];
	code = code <> SingularDefineIdeal["ide", Join[Thread[zlist-(regionprop//.Thread[regionvar->wlist])], {y^rank}]];
	code = code <> "ideal result = " <> SingularEliminate["ide",Join[wlist,{y}]];
	code = code <> "write(\":w singout\",result);exit;";
	WriteSingularCode[code];
	output = RunSingular[];
	Return[StringToIdeal[output]];
];


(* ::Subsection::Closed:: *)
(*RegionExpand*)


RegionExpand[region_List, prop_, y_, loop_List, nullp_List, replace_List]:=Module[
	{eq, eql, eqsol, x, eqlrule={}, refp, res},
	
	(*find the substitution rule of loop momentum l*)
	eq = region/.Rule->Equal;
	Do[
		If[eq[[i, 2]]===0,
			eq[[i, 2]]=y*eql[i],
			(*else*)
			refp[i, 1]=eq[[i, 2]];
			refp[i, 2]=Select[nullp, #=!=eq[[i, 2]]&][[1]];
			eq[[i, 2]]=x[i]*refp[i, 1]+y^2*refp[i, 2]+y*eql[i];
		],
		{i, 1, Length[eq]}
	];

	Off[Solve::svars];
	eqsol=Flatten[Solve[eq, loop]];
	On[Solve::svars];

	Do[
		If[region[[i, 2]]=!=0,
			(*AppendTo[eqlrule, GenPerpRule[refp[i, 1], eql[i]]];
			AppendTo[eqlrule, GenPerpRule[refp[i, 2], eql[i]]];*)
			eqlrule = Join[eqlrule, GenPerpRule[{refp[i, 1], refp[i, 2]}, eql[i], Variables[nullp]]]
		],
		{i, 1, Length[eq]}
	];
	
	res=Expand[prop/.eqsol]/.eqlrule/.replace;
	Return[res/.x[i_]:>"x"<>ToString[i]/.eql[i_]:>"\[Sigma]"<>ToString[i]]
]

(*Generate replacement relation for l\[Perpendicular]p*)
GenPerpRule[p_, l_]:=Module[
	{var, resleft, resright},
	var=Variables[p];
	resleft=var[[1]]*l;
	resright=Expand[(-p/.var[[1]]->0)*l/Coefficient[p, var[[1]]]];
	Return[resleft->resright]
]
GenPerpRule[p_List, l_, out_List]:=Module[
	{sp, eq, eqsol},
	
	(*generate equations*)
	eq = Expand[p*l]/.Thread[out*l->Table[sp[i], {i, Length[out]}]];
	
	(*solve*)
	Off[Solve::svars];
	eqsol=Flatten[Solve[Thread[eq==0], Table[sp[i], {i, Length[out]}]]];
	On[Solve::svars];
	
	Return[eqsol/.Thread[Table[sp[i], {i, Length[out]}]->out*l]]
]


(* ::Subsection::Closed:: *)
(*OBRegionExpand*)


(*expand the orthogonal basis Overscript[li, ^]Overscript[lj, ^], vilj*)
OBRegionExpand[region_List, y_, family_?FamilyQ]:=Module[
	{v, ldotl, ldotv, lv, totallvar, sp, sptostr, eq, eql, eqsol, x, stringrule, eqlrule={}, refp, res, ptov, loop = family["Loop"], out = family["Leg"], replace = family["Replace"]},

	(*find the substitution rule of loop momentum l*)
	eq = region/.Rule->Equal;
	Do[
		If[eq[[i, 2]]===0,
			eq[[i, 2]]=y*eql[i],
			(*else*)
			refp[i, 1]=eq[[i, 2]];
			refp[i, 2]=Select[family["NullMom"], #=!=eq[[i, 2]]&][[1]];
			eq[[i, 2]]=x[i]*refp[i, 1]+y^2*refp[i, 2]+y*eql[i];
		],
		{i, 1, Length[eq]}
	];

	Off[Solve::svars];
	eqsol=Flatten[Solve[eq, family["Loop"]]];
	On[Solve::svars];
	
	(*final loop l's after expanded*)
	totallvar = Join[Intersection[Variables[loop/.eqsol], loop], Union[Cases[loop/.eqsol, _eql, {0, Infinity}]]];
	
	(*sp to string*)
	sp = Join[GenSP[totallvar], GenSP[totallvar, Table[v[i], {i, 1, Length[out]}]]];
	sptostr = Thread[sp->Table["sp"<>ToString[i], {i, 1, Length[sp]}]];
	Print[sptostr/.{eql->"eql", v->"v"}];	
	
	(*relation between v[i] and out[[i]]*)
	ptov = Thread[out->Table[Sum[out[[i]]*out[[j]]*v[j],{j, 1, Length[out]}]/.replace, {i, 1, Length[out]}]];
	
	(*generate rules for eql and v, eql and p*)
	Do[
		If[region[[i, 2]]=!=0,
			(*AppendTo[eqlrule, GenPerpRule[refp[i, 1]/.ptov, eql[i]]];
			AppendTo[eqlrule, GenPerpRule[refp[i, 2]/.ptov, eql[i]]];*)
			eqlrule = Join[eqlrule, GenPerpRule[{refp[i, 1], refp[i, 2]}/.ptov, eql[i], Table[v[i], {i, Length[out]}]]];
			(*AppendTo[eqlrule, GenPerpRule[refp[i, 1], eql[i]]];
			AppendTo[eqlrule, GenPerpRule[refp[i, 2], eql[i]]];*)
			(*eqlrule = Join[eqlrule, GenPerpRule[{refp[i, 1], refp[i, 2]}, eql[i], loop]]*)
		],
		{i, 1, Length[eq]}
	];
	
	(*Print[eqlrule];*)
	
	(*Deal with ldotv*)
	Do[
		ldotv[i, j] = loop[[i]] * v[j];
		ldotv[i, j] = Expand[ldotv[i, j]/.eqsol]/.eqlrule;
		ldotv[i, j] = ldotv[i, j]/.Thread[out*v[j]->DirecVector[j, Length[out]]];
		ldotv[i, j] = ldotv[i, j]/.sptostr,
		{i, 1, Length[loop]},
		{j, 1, Length[out]}
	];

	(*Deal with ldotl*)
	Do[
		ldotl[i, j] = ((loop[[i]]/.eqsol) - Sum[ldotv[i, k]*out[[k]], {k, 1, Length[out]}])((loop[[j]]/.eqsol) - Sum[ldotv[j, k]*out[[k]], {k, 1, Length[out]}]);
		ldotl[i, j] = Expand[Expand[ldotl[i, j]]/.replace/.ptov]/.eqlrule,
		{i, 1, Length[loop]},
		{j, i, Length[loop]}
	];
	
	(*Print["OB: ", Flatten[{Table["ldotl"[i, j], {i, Length[loop]}, {j, i, Length[loop]}], Table["ldotv"[i, j], {i, 1, Length[loop]}, {j, 1, Length[out]}]}]]*);
	
	Return[Flatten[{Table[ldotl[i, j], {i, 1, Length[loop]}, {j, i, Length[loop]}], Table[ldotv[i, j], {i, 1, Length[loop]}, {j, 1, Length[out]}]}]
	       /.x[i_]:>"x"<>ToString[i]/.sptostr]
]

(*n-dimension vector with k'th element 1*)
DirecVector[k_, n_]:= Module[
	{res},
	res = Table[0, {i, 1, n}];
	res[[k]] = 1;
	Return[res];
]


(* ::Subsection:: *)
(*AnalyzeAsyRes*)


AnalyzeAsyRes[asyres_List, prop_List, propmom_List, loop_List, nullp_List, replace_List]:=Module[
	{y, negasyres, negonepropmom={}, lcoe, resq, resqind, testp, testcombine, res=$Failed},
	
	If[Max[asyres]==0 && Min[asyres]==0, Return[{}]];
	
	(*turn all elements to be non-positive, and the minimal element is -1*)
	If[Min[asyres]>-1, negasyres=asyres-Abs[-1-Min[asyres]], negasyres=asyres];
	
	Do[
		If[negasyres[[i]]==-1, AppendTo[negonepropmom, propmom[[i]]]],
		{i, 1, Length[negasyres]}
	];
	(*Print["negpropmom", negonepropmom];*)
	
	lcoe=Table[Coefficient[negonepropmom[[i]], loop[[j]]], {i, 1, Length[negonepropmom]}, {j, 1, Length[loop]}];
	(*Print["lcoe",lcoe];*)
	resqind=IndependentVector[lcoe];
	resq=Table[negonepropmom[[resqind[[i]]]], {i, 1, Length[resqind]}];
	(*Print["resq",resq];*)
	
	testp=Join[{0}, nullp];
	testcombine=PowerSet[testp, Length[resq]];
	(*Print["testcombine ", testcombine];*)
	
	Do[
		If[-Exponent[RegionExpand[Thread[resq->testcombine[[i]]], prop, y, loop, nullp, replace], y, Min]/2==negasyres,
			res=Thread[resq->testcombine[[i]]];Break[];	
		],
		{i, 1, Length[testcombine]}
	];
	
	Return[res]
]

AnalyzeAsyRes[asyres_List, family_?FamilyQ]:= AnalyzeAsyRes[asyres, family["Prop"], family["PropMom"], family["Loop"], family["NullMom"], family["Replace"]]


(* ::Subsection::Closed:: *)
(*TestAsyRes*)


TestAsyRes[asyres_List, prop_List, propmom_List, loop_List, nullp_List, replace_List]:=Module[
	{y, negasyres, negonepropmom={}, lcoe, resq, resqind, testp, testcombine, res=$Failed},
	
	If[Max[asyres]==0 && Min[asyres]==0, Return[{}]];
	
	(*turn all elements to be non-positive, and the minimal element is -1*)
	If[Min[asyres]>-1, negasyres=asyres-Abs[-1-Min[asyres]], negasyres=asyres];
	
	Do[
		If[negasyres[[i]]==-1, AppendTo[negonepropmom, propmom[[i]]]],
		{i, 1, Length[negasyres]}
	];
	(*Print["negpropmom", negpropmom];*)
	
	lcoe=Table[Coefficient[negonepropmom[[i]], loop[[j]]], {i, 1, Length[negonepropmom]}, {j, 1, Length[loop]}];
	(*Print["lcoe",lcoe];*)
	resqind=IndependentVector[lcoe];
	resq=Table[negonepropmom[[resqind[[i]]]], {i, 1, Length[resqind]}];
	(*Print["resq",resq];*)
	
	testp=Join[{0}, nullp];
	testcombine=PowerSet[testp, Length[resq]];
	(*Print["testcombine ", testcombine];*)
	
	Do[
		If[-Exponent[RegionExpand[Thread[resq->testcombine[[i]]], prop, y, loop, nullp, replace], y, Min]/2==negasyres,
			res=Thread[resq->testcombine[[i]]];(*Break[];*)	
			Print[res];
		],
		{i, 1, Length[testcombine]}
	];
	
	(*Return[res]*)
]

TestAsyRes[asyres_List, family_?FamilyQ]:= TestAsyRes[asyres, family["Prop"], family["PropMom"], family["Loop"], family["NullMom"], family["Replace"]]

TestAsyRes[family_?FamilyQ]:= Module[
	{asyres},
	
	asyres = Get[FileNameJoin[{CurrentDir[], "cache", ToString[family], "Region", "regions"}]];
	
	Do[
		Print[asyres[[i]], ": "];
		TestAsyRes[asyres[[i]], family],
		{i, Length[asyres]}
	];
]


(* ::Subsection::Closed:: *)
(*AsyResToIdeal*)


AsyResToIdeal[asyres_List, deno_List, prop_List, isp_List, propmom_List, loop_List, out_List, nullp_List, replace_List]:=Module[
	{negasyres, region, regionprop, sigma, x, pvar, xvar, var, rank, para},
	
	(*turn all elements to be non-positive, and the minimal element is -1*)
	If[Min[asyres]>-1, negasyres=asyres-Abs[-1-Min[asyres]], negasyres=asyres];
	
	region=AnalyzeAsyRes[asyres, prop, propmom, loop, nullp, replace];
	(*Debug*)
	Print[asyres, ": ", region];
	If[Length[region]==0, Return[{1}]];
	regionprop=RegionExpand[region, Join[prop, isp], "y", loop, nullp, replace];
	
	rank=-4*Length[region]-2*negasyres . deno+1;
	Print["rank of y: ",rank];
	If[rank<1, Return[{1}]];
	
	sigma=Table["\[Sigma]"<>ToString[i], {i, 1, Length[loop]}];
	x=Table["x"<>ToString[i], {i, 1, Length[loop]}];
	
	pvar=Join[Intersection[Variables[regionprop], loop], Intersection[Cases[regionprop, _String, {0, Infinity}], sigma]];
	xvar=Intersection[Cases[regionprop, _String, {0, Infinity}], x];
	(*Print[pvar];
	Print[xvar];*)
	var=Join[xvar, GenSP[pvar], GenSP[pvar, out]];(*Print["vars to eliminate: ", var];*)
	
	(*find all parameters*)
	para = Variables[{prop, #[[2]]&/@replace}];
	para = Complement[para, Join[loop, out]];
	
	Return[SingularEliminateIdeal[regionprop, var, "y", rank, "Parameters" -> para]]
]


(*TESTING!!!*)
(*Use another basis (a polynomial combination of z[i]) rather than original propagator z[i]*)
AsyResToIdeal[asyres_List, deno_List, newbasis_List, prop_List, isp_List, propmom_List, loop_List, out_List, nullp_List, replace_List]:=Module[
	{negasyres, region, regionprop, regionbasis, sigma, x, w, zlist, pvar, xvar, var, rank, para},
	
	(*turn all elements to be non-positive, and the minimal element is -1*)
	If[Min[asyres]>-1, negasyres=asyres-Abs[-1-Min[asyres]], negasyres=asyres];
	
	region=AnalyzeAsyRes[asyres, prop, propmom, loop, nullp, replace];
	(*Debug*)
	Print[asyres, ": ", region];
	If[Length[region]==0, Return[{1}]];
	regionprop=RegionExpand[region, Join[prop, isp], "y", loop, nullp, replace];
	
	rank=-4*Length[region]-2*negasyres . deno+1;
	Print["rank of y: ",rank];
	If[rank<1, Return[{1}]];
	
	sigma=Table["\[Sigma]"<>ToString[i], {i, 1, Length[loop]}];
	x=Table["x"<>ToString[i], {i, 1, Length[loop]}];
	
	pvar = Join[Intersection[Variables[regionprop], loop], Intersection[Cases[regionprop, _String, {0, Infinity}], sigma]];
	xvar = Intersection[Cases[regionprop, _String, {0, Infinity}], x];
	var = Join[xvar, GenSP[pvar], GenSP[pvar, out]];
	
	(*subsititute var (like xi, \[Sigma]\[CenterDot]p) into wi's*)
	w = Table["w"<>ToString[i], {i, 1, Length[var]}];
	regionprop = regionprop//.Thread[var->w];(*Print["regionprop: ", regionprop]*);
	
	zlist = Table[FFI`z[i], {i ,1, Length[prop] + Length[isp]}];
	regionbasis = Collect[newbasis/.Thread[zlist->regionprop], "y", Simplify];(*Print["regionbasis: ", regionbasis];*)
	
	(*find all parameters*)
	para = Variables[{prop, #[[2]]&/@replace}];
	para = Complement[para, Join[loop, out]];
	
	Return[SingularEliminateIdeal[regionbasis, w, "y", rank, "Parameters" -> para]]
]


AsyResToIdeal[asyres_List, deno_List, family_?FamilyQ]:= Module[
	{},
	Return[
		AsyResToIdeal[asyres, deno, family["Prop"], family["Isp"], family["PropMom"],
	                  family["Loop"], family["Leg"], family["NullMom"], family["Replace"]]
	];
]

AsyResToIdeal[asyres_List, family_?FamilyQ]:= Module[
	{},
	Return[
		AsyResToIdeal[asyres, Table[1, {i, 1, Length[family["Prop"]]}], family["Prop"], family["Isp"], family["PropMom"],
	                  family["Loop"], family["Leg"], family["NullMom"], family["Replace"]]
	];
]


AsyResToOBIdeal[asyres_List, family_?FamilyQ]:= Module[
	{negasyres, region, regionob, rank, x, sp, var, w, L=Length[family["Loop"]], E=Length[family["Leg"]]},
	
	(*turn all elements to be non-positive, and the minimal element is -1*)
	If[Min[asyres]>-1, negasyres=asyres-Abs[-1-Min[asyres]], negasyres=asyres];
	
	region=AnalyzeAsyRes[asyres, family];
	(*Debug*)
	Print[asyres, ": ", region];
	If[Length[region]==0, Return[{1}]];
	regionob = OBRegionExpand[region, "y", family];
	
	rank=-4*Length[region]-2*negasyres . Table[1, {i, 1, Length[family["Prop"]]}]+1;
	Print["rank of y: ",rank];
	If[rank<1, Return[{1}]];
	
	(*variables to eliminate*)
	sp = Table["sp"<>ToString[i], {i, L (L+1)/2+L*E}];
	x = Table["x"<>ToString[i], {i, L}];
	var = Intersection[Cases[regionob, _String, {0, Infinity}], Join[sp, x]];
	
	Return[SingularEliminateIdeal[regionob, var, "y", rank, "Parameters" -> FindParameter[family]]]
]


(* ::Subsection::Closed:: *)
(*FiniteIdeal*)


Options[FiniteIdeal] = {"HasAsyResult" -> False, "DegBound" -> 0};

FiniteIdeal[family_?FamilyQ, deno_List, opts: OptionsPattern[]]:= Module[
	{para, dir, regions, time, ideals = {}, code, res},
	
	dir = FileNameJoin[{CurrentDir[], "cache", ToString[family], "Ideal"}];
	If[!DirectoryQ[dir], CreateDirectory[dir]];
	
	Print["Exponents of propagators: ", deno];
	
	(*obtain asy results*)
	regions = If[OptionValue["HasAsyResult"], 
	              Get[FileNameJoin[{CurrentDir[], "cache", ToString[family], "Region", "regions"}]], 
	              AsyRegion[family]
	          ];
	Print["Regions: ", regions];
	
	Print["Using Singular to compute ideal of each region..."];
	time = AbsoluteTiming[Do[
		Print["ideal"<>ToString[i]<>": "];
		AppendTo[ideals, AsyResToIdeal[regions[[i]], deno, family]];
		Put[ideals[[-1]], FileNameJoin[{dir, "ideal"<>ToString[i]}]],
		{i,1,Length[regions]}
	]][[1]];
	Print["Time used: ", time, "s"];
	
	(*find all parameters*)
	para = Variables[{family["Prop"], #[[2]]&/@family["Replace"]}];
	para = Complement[para, Join[family["Loop"], family["Leg"]]];
	
	Print["Using Singular to compute the intersection of ideals..."];
	time = AbsoluteTiming[
		code = SingularDefineRing[{z, Length[family["Prop"]] + Length[family["Isp"]]}, "Parameters" -> para];
		If[OptionValue["DegBound"] > 0, code = code <> "degBound = " <> ToString[OptionValue["DegBound"]] <> ";\n"];
		Do[code = code <> SingularDefineIdeal["id"[i], ideals[[i]]], {i, 1, Length[ideals]}];
		code = code <> "ideal result = " <> SingularIntersect@@Table["id"[i], {i, 1, Length[ideals]}];
		code = code <> "result = slimgb(result);write(\":w singout\",result);exit;";
		WriteSingularCode[code];
		res = StringToIdeal[RunSingular[]];
		Put[res, FileNameJoin[{dir, "result"}]];
	][[1]];
	Print["Time used: ", time, "s"];
	
	Return[res];
]

FiniteIdeal[family_?FamilyQ, opts: OptionsPattern[]]:= FiniteIdeal[family, Table[1, {i, 1, Length[family["Prop"]]}], opts];


(* ::Subsection::Closed:: *)
(*OBFiniteIdeal*)


Options[OBFiniteIdeal] = {"HasAsyResult" -> False, "DegBound" -> 0};

OBFiniteIdeal[family_?FamilyQ, opts: OptionsPattern[]]:= Module[
	{para, dir, regions, time, ideals = {}, code, res, deno, L = Length[family["Loop"]], E = Length[family["Leg"]]},
	
	deno = Table[1, {i, 1, Length[family["Prop"]]}];
	
	dir = FileNameJoin[{CurrentDir[], "cache", ToString[family], "Ideal"}];
	If[!DirectoryQ[dir], CreateDirectory[dir]];
	
	Print["Exponents of propagators: ", deno];
	
	(*obtain asy results*)
	regions = If[OptionValue["HasAsyResult"], 
	              Get[FileNameJoin[{CurrentDir[], "cache", ToString[family], "Region", "regions"}]], 
	              AsyRegion[family]
	          ];
	Print["Regions: ", regions];
	
	Print["Using Singular to compute ideal of each region..."];
	time = AbsoluteTiming[Do[
		Print["ideal"<>ToString[i]<>": "];
		AppendTo[ideals, AsyResToOBIdeal[regions[[i]], family]];
		Put[ideals[[-1]], FileNameJoin[{dir, "ideal"<>ToString[i]}]],
		{i,1,Length[regions]}
	]][[1]];
	Print["Time used: ", time, "s"];
	
	(*find all parameters*)
	para = Variables[{family["Prop"], #[[2]]&/@family["Replace"]}];
	para = Complement[para, Join[family["Loop"], family["Leg"]]];
	
	Print["Using Singular to compute the intersection of ideals..."];
	time = AbsoluteTiming[
		code = SingularDefineRing[{z, Length[family["Prop"]] + Length[family["Isp"]]}, "Parameters" -> para, "Weight" -> Join[Table[2, {i, L (L+1)/2}], Table[1, {i, L*E}]]];
		If[OptionValue["DegBound"] > 0, code = code <> "degBound = " <> ToString[OptionValue["DegBound"]] <> ";\n"];
		Do[code = code <> SingularDefineIdeal["id"[i], ideals[[i]]], {i, 1, Length[ideals]}];
		code = code <> "ideal result = " <> SingularIntersect@@Table["id"[i], {i, 1, Length[ideals]}];
		code = code <> "result = slimgb(result);write(\":w singout\",result);exit;";
		WriteSingularCode[code];
		res = StringToIdeal[RunSingular[]];
		Put[res, FileNameJoin[{dir, "result"}]];
	][[1]];
	Print["Time used: ", time, "s"];
	
	Return[res/.OBToProp[family]//Expand];
]


(* ::Subsection::Closed:: *)
(*OBToProp*)


(*turn OB into prop*)
OBToProp[family_?FamilyQ]:= Module[
	{ldotl, ldotv},
	
	(*deal with ldotl*)
	Do[
		ldotl[i, j] = Gram[Join[{family["Loop"][[i]]}, family["Leg"]], Join[{family["Loop"][[j]]}, family["Leg"]], family],
		{i, Length[family["Loop"]]},
		{j, i, Length[family["Loop"]]}
	];
	
	(*deal with ldotl*)
	Do[
		ldotv[i, j] = Gram[family["Leg"]/.family["Leg"][[j]]->family["Loop"][[i]], family["Leg"], family]//Simplify,
		{i, Length[family["Loop"]]},
		{j, Length[family["Leg"]]}
	];
	
	Return[Thread[Table[FFI`z[i], {i, Length[family["Prop"]] + Length[family["Isp"]]}] -> Flatten[{Table[ldotl[i, j], {i, Length[family["Loop"]]}, {j, i, Length[family["Loop"]]}], Table[ldotv[i, j], {i, Length[family["Loop"]]}, {j, Length[family["Leg"]]}]}]]]
]


(* ::Section:: *)
(*End*)


End[];


EndPackage[];
