open Printf
open Util
open Shared
open Ast

type ty = Int | Bool
type environment = ty Symtab.symtab

let typecheck (_prog : program) : unit = ()
