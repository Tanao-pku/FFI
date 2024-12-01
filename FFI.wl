(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


Print["FFI.wl loaded."];


(*Load files*)
Get[FileNameJoin[{DirectoryName[$InputFileName], "Common", "ZF.wl"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "Common", "SP.wl"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "Common", "Family.wl"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "Common", "CommonFunc.wl"}]];

Get[FileNameJoin[{DirectoryName[$InputFileName], "Interface", "Install.wl"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "Interface", "AsyAPI.wl"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "Interface", "SingularAPI.wl"}]];

Get[FileNameJoin[{DirectoryName[$InputFileName], "Basic", "Ideal.wl"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "Basic", "Finite.wl"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "Basic", "UV.wl"}]];
Get[FileNameJoin[{DirectoryName[$InputFileName], "Basic", "ExpIBP.wl"}]];


BeginPackage["FFI`"];


$PrintLevel = 0;


Begin["`Private`"]


(* ::Section:: *)
(*End*)


End[];


EndPackage[];
