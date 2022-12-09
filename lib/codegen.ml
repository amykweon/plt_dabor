(* Code generation for dabor *)

module L = Llvm
module A = Ast
module S = Sast
module E = Semant
open Ast
open Llvm
open Sast

module StringMap = Map.Make(String)

