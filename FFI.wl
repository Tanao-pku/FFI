(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


Print["FFI.wl loaded."];


(*Load files*)
FFI`Private`$FFIFolders = {"Common", "Interface", "Basic"};

Do[
    Get /@ FileNames["*.wl", FileNameJoin[{DirectoryName[$InputFileName], FFI`Private`$FFIFolders[[i]]}], Infinity],
    {i, Length[FFI`Private`$FFIFolders]}
];


BeginPackage["FFI`"];


(* ::Section:: *)
(*Settings*)


$PrintLevel::usage = "The symbol to control printing";
$PrintLevel = 0;


(* ::Section:: *)
(*Public functions*)


(* ::Subsection::Closed:: *)
(*ZF*)


(*F::usage = "F[i, j, k, ...] denotes the integrand 1/(z[1]^i*z[2]^j*z[3]^k\[CenterDot]\[CenterDot]\[CenterDot]).";
F = FFI`Private`F;*)


(*z::usage = "z[i] denotes the i'th propagator.";
z = FFI`Private`z;*)


FRank::usage = "FRank[x] returns the rank of x which is the form of F[i, j, k, ...].";
FRank = FFI`Private`FRank;


FDots::usage = "FDots[x] returns the dots of x which is the form of F[i, j, k, ...].";
FDots = FFI`Private`FDots;


FLess::usage = "FLess[F1, F2] returns whether F1 is less than F2.";
FLess = FFI`Private`FLess;


FSort::usage = "FSort[list] sorts the list of F's by FLess and returns the sorted list.";
FSort = FFI`Private`FSort;


(* ::Subsection::Closed:: *)
(*Family*)


DefineFamily::usage = "DefineFamily[name, prop, isp, propmom, loop, leg, nullmom, replace] defines a family.";
DefineFamily = FFI`Private`DefineFamily;


FamilyInfo::usage = "FamilyInfo[name] returns the information of a family, which is a list.";
FamilyInfo = FFI`Private`FamilyInfo;


(* ::Subsection::Closed:: *)
(*CommonFunc*)


Deg::usage = "Deg[poly, vars] returns the deg of a polynomial poly.";
Deg = FFI`Private`Deg;


GenMono::usage = "GenMono[deg, vars]  returns the list of all monomials whose degrees are 'deg'.";
GenMono = FFI`Private`GenMono;


(* ::Subsection::Closed:: *)
(*DRR*)


MasterRecurrence::usage = "MasterRecurrence[family] returns the recurrence relations of master integrals from 6-2eps dimension to 4-2eps dimension.";
MasterRecurrence = FFI`Private`MasterRecurrence;


RaisingRecurrence::usage = "RaisingRecurrence[family] returns the recurrence relations of master integrals from 4-2eps dimension to 6-2eps dimension.";
RaisingRecurrence = FFI`Private`RaisingRecurrence;


GenDRR::usage = "GenDRR[famlily] generates the recurrence relations for the family and its UV families";
GenDRR = FFI`Private`GenDRR;


(* ::Subsection::Closed:: *)
(*ExpIBP*)


DivergentMasters::usage = "DivergentMasters[family] returns the masters of divergent parts of original master integrals";
DivergentMasters = FFI`Private`DivergentMasters;


DivergentReduce::usage = "DivergentReduce[family] reduces the divergent parts of original master integrals";
DivergentReduce = FFI`Private`DivergentReduce;


ReduceFinite::usage = "ReduceFinite[family] reduces the finite relations to master integrals. Finite relations must be generated first";
ReduceFinite = FFI`Private`ReduceFinite;


GenExpEq::usage = "GenExpEq[family] expands the reduced relations by eps. ReduceFinite needs to be called first";
GenExpEq = FFI`Private`GenExpEq;


ExpandedMasters::usage = "ExpandedMasters[family] returns the masters of expanded master integrals up to dovergent order";
ExpandedMasters = FFI`Private`ExpandedMasters;


ExpandedReduce::usage = "ExpandedReduce[family] returns relations of expanded master integrals up to dovergent order";
ExpandedReduce = FFI`Private`ExpandedReduce;


(* ::Subsection::Closed:: *)
(*Finite*)


GenFiniteRelation::usage = "GenFiniteRelation[family, deno, rank] generates the finite relation up to a given rank. IR finite ideal has to be calculated first.
GenFiniteRelation[family, deno, eles] generates the finite relation with respect to the eles which is expected to be a list of IR finite numerators";
GenFiniteRelation = FFI`Private`GenFiniteRelation;


GenEvaEle::usage = "GenEvaEle[family, rank] generates the evanescent numerators up to a given rank. It simply uses \!\(\*OverscriptBox[\(li\), \(~\)]\)\[CenterDot]\!\(\*OverscriptBox[\(lj\), \(~\)]\) as the generators, otherwise you provide the generators by change the optional parameter \"Generator\"";
GenEvaEle = FFI`Private`GenEvaEle;


GenDRRFiniteRelation::usage = "";
GenDRRFiniteRelation = FFI`Private`GenDRRFiniteRelation;


(* ::Subsection::Closed:: *)
(*Ideal*)


AnalyzeAsyRes::usage = "AnalyzeAsyRes[asyres, prop, propmom, loop, nullp, replace] turns the result of asy.m into a region of loop momenta. 
AnalyzeAsyRes[asyres, family] turns the result of asy.m into a region of loop momenta.";
AnalyzeAsyRes = FFI`Private`AnalyzeAsyRes;


AsyResToIdeal::usage = "AsyResToIdeal[asyres, deno, prop, isp, propmom, loop, out, nullp, replace] computes the ideal of numerator with respect to a result of asy.m and given denominator powers deno. 
AsyResToIdeal[asyres, deno, family] computes the ideal of numerator with respect to a result of asy.m and given denominator powers deno.";
AsyResToIdeal = FFI`Private`AsyResToIdeal;


FiniteIdeal::usage = "FiniteIdeal[family, deno] computes the ideal for finite Feynman integrals corresponds to the denominator powers deno. 
FiniteIdeal[family] computes the ideal corresponds to the topsector corner integral.";
FiniteIdeal = FFI`Private`FiniteIdeal;


OBFiniteIdeal::usage = "OBFiniteIdeal[family, deno] computes the ideal for finite Feynman integrals corresponds to the denominator powers deno, while the variables for the ideal is \!\(\*OverscriptBox[SubscriptBox[\(l\), \(i\)], \(^\)]\)\[CenterDot]\!\(\*OverscriptBox[SubscriptBox[\(l\), \(j\)], \(^\)]\) and \!\(\*SubscriptBox[\(l\), \(i\)]\)\[CenterDot]\!\(\*SubscriptBox[\(v\), \(j\)]\). It can be transformed into propagators by using function OBToProp.
OBFiniteIdeal[family] computes the ideal corresponds to the topsector corner integral.";
OBFiniteIdeal = FFI`Private`OBFiniteIdeal;


OBToProp::usage = "OBToProp[family] gives the transformation relations from OB basis to propagators.";
OBToProp = FFI`Private`OBToProp;


BurnIR::usage = "BurnIR[family] generates IR information of the family and returns the list of IR regions.";
BurnIR = FFI`Private`BurnIR;


(* ::Subsection::Closed:: *)
(*UV*)


FindUVRegion::usage="FindUVRegion[family] gives the UV region of a family. 
FindUVRegion[loopcb_List, loop_List] gives the UV region that corresponds to the given loop momenta and their combinations.";
FindUVRegion = FFI`Private`FindUVRegion;


GenUVFamily::usage="GenUVFamily[family, m] defines all UV family with respect to a given family, and m is the mass added to the propagator.";
GenUVFamily = FFI`Private`GenUVFamily;


BurnUV::usage="BurnUV[family, m] will generate all information for UV subtraction which includes UV family and so on, and m is the mass added to the propagator. It returns the list of UV family name.";
BurnUV = FFI`Private`BurnUV;


UVCounterTerm::usge="UVCounterTerm[Fexpr, family] gives the UV counter term of the given Fexpr which is linear combinations of some F[__], and these F's must be in the same family.";
UVCounterTerm = FFI`Private`UVCounterTerm;


UVDegree::usage="UVDegree[zpoly, family] gives the power counting result of zpoly in each family's UV region, zpoly is a polynomial of z[i]'s.";
UVDegree = FFI`Private`UVDegree;


(* ::Section:: *)
(*End*)


EndPackage[];
