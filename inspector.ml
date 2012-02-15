(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) 2012                                               			*)
(*    Sylvain Frédéric Nahas (sylvain.nahas@googlemail.com)								*)
(*																																				*)
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


(* an "inspector" object is responsible for computing consolidated properties properties statistics and abort on errors *)

open Property_status
open Consolidation

(* value to be given back by Frama-C to OS *)
let return_proof_error = 7

(* output error message on STDERR, flushing output, and abort *)
let xerror nb category desc  = (Printf.eprintf "[werror] Analysis reports %d %s (%s). Aborting.%!\n" nb category desc; 
                                Cmdline.run_after_exiting_stage (exit return_proof_error); )

class inspector =
object(self)

  val mutable st_unknown  = 0 ; (* no status *)
  val mutable st_partial  = 0 ; (* locally valid but missing hyp *)
  val mutable st_extern   = 0 ; (* considered valid *)
  val mutable st_complete = 0 ; (* valid and complete *)
  val mutable st_bug      = 0 ; (* invalid and complete *)
  val mutable st_alarm    = 0 ; (* invalid but missing hyp *)
  val mutable st_dead     = 0 ; (* under invalid hyp *)
  val mutable st_inconsistent = 0 ; (* unsound *)
      
  method categorize : Consolidation.t -> unit = function
  | Never_tried | Unknown _ -> st_unknown <- succ st_unknown ;
  | Considered_valid -> st_extern <- succ st_extern ;
  | Valid _ -> st_complete <- succ st_complete ;
  | Invalid _ -> st_bug <- succ st_bug ;
  | Valid_under_hyp _ -> st_partial <- succ st_partial ;
  | Invalid_under_hyp _ -> st_alarm <- succ st_alarm ;
  | Valid_but_dead _ | Invalid_but_dead _ | Unknown_but_dead _ -> 
    st_dead <- succ st_dead ;
  | Inconsistent _ -> st_inconsistent <- succ st_inconsistent ;

  method statistics (ip:Property.t) st =
    begin
      ignore(ip);
      self#categorize st;
    end

  method abort_on_error =
    if st_bug > 0 then xerror st_bug "bug(s)" "invalid and complete proofs" ;
    if st_alarm > 0 then xerror st_alarm "alarm(s)" "invalid proof, but missing hypothesis";
    if st_dead > 0 then xerror st_dead "dead(s)" "proofs whose hypothesis are invalid";
    if st_inconsistent > 0 then xerror st_inconsistent "unsound proof(s)" "whose hypothesis are invalid";
    if st_unknown > 0 && ( Werror_options.NoUnknown.get () == false ) then xerror st_unknown "unknown(s)" "proof without status. Use -werror-no-unknown to change this behavior";
    if ( st_extern > 0 ) && ( Werror_options.NoExtern.get () == false ) then xerror st_extern "external(s) proof(s)" "considered as valid. Use -werror-no-external to change this behavior";
    if ( st_partial > 0 ) && ( Werror_options.NoPartial.get () == false ) then xerror st_partial "partial(s)" "locally valid but with missing hypothesis. Use -werror-no-partial to change this behavior";

end (* inspector *)

let create () = (new inspector :> Scan.inspector)
