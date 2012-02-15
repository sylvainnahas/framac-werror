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

include Plugin.Register
  (struct
     let name = "werror"
     let shortname = "werror"
     let help = "Abort on warnings generated by proof analysis"
   end)

module Enabled =
  Bool
    (struct
       let option_name = "-werror"
       let arg_name = ""
       let help = "Abort when some proof could not be discharged"
       let kind = `Tuning
       let default = false
     end)

module NoPartial =
  Bool
    (struct
       let option_name = "-werror-no-partial"
       let arg_name = ""
       let help = "Do not abort on partial proofs (default: false)"
       let kind = `Tuning
       let default = false
     end)

module NoExtern =
  Bool
    (struct
       let option_name = "-werror-no-external"
       let arg_name = ""
       let help = "Do not abort on external proofs (default: false)"
       let kind = `Tuning
       let default = false
     end)

module NoUnknown =
  Bool
    (struct
       let option_name = "-werror-no-unknown"
       let arg_name = ""
       let help = "Do not abort on proofs whose state is unknown (default: false)"
       let kind = `Tuning
       let default = false
     end)
