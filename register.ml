(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2012                                               			*)
(*    Sylvain Frédéric Nahas (sylvain.nahas@googlemail.com)								*)
(*																																				*)
(*  Copyright (C) 2007-2011                                               *)
(*    CEA (Commissariat � l'�nergie atomique et aux �nergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

open Property_status

(* -------------------------------------------------------------------------- *)
(* --- Plug-in Implementation                                             --- *)
(* -------------------------------------------------------------------------- *)

let iter () = Scan.iter (Inspector.create())
    
let iter =
  Dynamic.register
    ~plugin:"werror"
    ~journalize:true
    "iter"
    (Datatype.func Datatype.unit Datatype.unit)
    iter

let main () = if Werror_options.Enabled.get () then iter ()

let () = Db.Main.extend main

(*
Local Variables:
compile-command: "make -C ../.."
End:
*)
