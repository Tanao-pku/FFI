(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["FFI`"];


F::usage = "F[i, j, k, ...] denotes the integrand 1/(z[1]^i*z[2]^j*z[3]^k\[CenterDot]\[CenterDot]\[CenterDot]).";
z::usage = "z[i] denotes the i'th propagator.";
FRank::usage = "FRank[x] gives the rank of x which is the form of F[i, j, k, ...]."
FDots::usage = "FDots[x] gives the dots of x which is the form of F[i, j, k, ...]."
FLess::usage = "FLess[F1, F2] judges whether F1 is less than F2."
FSort::usage = "FSort[list] sorts the list of F's by FLess."
Deg::usage = "Deg[poly, {vars}] gives the deg of a polynomial poly."
GenMono::usage = "GenMono[deg, {vars}] gives all the monomials of the deg formed by variables vars."


Begin["`Private`"]


(* ::Section:: *)
(*z & F*)


ZF[F[i__], k_, n_]:= Module[{list},
	list = {i};
	list[[k]] = list[[k]] - n;
	Return[F@@list];
]
F/:z[i_]^(m_:1)F[j__]:= ZF[F[j], i, m]

Protect[F];
Protect[z];


(* ::Section:: *)
(*Rank & Dots*)


FRank[x_F]:= - Plus@@Select[List@@x, # < 0&];
FDots[x_F]:= Plus@@(Select[List@@x, # > 0&] - 1);


(* ::Section:: *)
(*Ordering*)


(* ::Subsection::Closed:: *)
(*FLess*)


FLess[x_F, y_F]:= Module[
	{},
	If[PosIndNum[x] < PosIndNum[y], Return[True]];
	If[PosIndNum[x] > PosIndNum[y], Return[False]];
	
	If[FRank[x] < FRank[y], Return[True]];
	If[FRank[x] > FRank[y], Return[False]];
	
	If[FDots[x] < FDots[y], Return[True]];
	If[FDots[x] > FDots[y], Return[False]];
	
	Return[False];
]

PosIndNum[x_F]:= Length[Select[List@@x, # > 0&]];


(* ::Subsection::Closed:: *)
(*FSort*)


FSort[F__List]:= Sort[F, FLess];


(* ::Section:: *)
(*End*)


End[]


EndPackage[];
