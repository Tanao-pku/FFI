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


$PrintLevel = 0;


Begin["`Private`"]


(* ::Section:: *)
(*End*)


End[];


EndPackage[];
