(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["FFI`"];


Begin["`Private`"]


(* ::Section:: *)
(*SP*)


SetAttributes[SP, Orderless];

(*Rules for SP*)
SP/: SP[x_]:= SP[x, x];
SP/: SP[x_ + y_, z_]:= SP[x, z] + SP[y, z];
SP/: SP[k_?NumberQ * x_, y_]:= k * SP[x, y];
SP/: SP[Y * x_, y_]:= Y * SP[x, y];

(*Output format for SP*)
Format[SP[x_, x_]]:= x^2;
Format[SP[x_, y_]]:= x * y;

Protect[SP];
Protect[Y];


FFI`Private`SP = SP;
FFI`Private`Y = Y;

Protect[FFI`Private`SP];
Protect[FFI`Private`Y];


(*NumeratorCounting[exp_, l_Symbol]:= Exponent[exp/.l->Y*l, Y];
NumeratorCounting[exp_, l_List]:= Exponent[exp/.Thread[l->Y*l], Y];*)


(* ::Section:: *)
(*End*)


End[]


EndPackage[];
