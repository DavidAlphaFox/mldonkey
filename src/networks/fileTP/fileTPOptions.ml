(* Copyright 2001, 2002 b8_bavard, b8_fee_carabine, INRIA *)
(*
    This file is part of mldonkey.

    mldonkey is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    mldonkey is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with mldonkey; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Md4
open CommonOptions
open Options

let cmd_basedir = Autoconf.current_dir (* will not work on Windows *)

let fileTP_ini = create_options_file (
    Filename.concat file_basedir "fileTP.ini")

let fileTP_section = file_section fileTP_ini [] ""  
  
  (*
let enabled = define_option fileTP_section
    ["fileTP2_enabled"]
    "Do you want to support FileTP2 protocol (not yet supported)"
    bool_option true
*)  
  
let commit_in_subdir = define_option fileTP_section ["commit_in_subdir"]
  "The subdirectory of temp/ where files should be moved to"
    string_option "FileTP"

let user_agent = Printf.sprintf "MLDonkey %s" Autoconf.current_version

let mirrors = define_option fileTP_section ["mirrors"]
    "A list of lists, where each list contains equivalent prefixes for mirrors"
    (list_option (list_option string_option))
  
  (*
   [
      [
"http://www-ftp.lip6.fr/pub/linux/distributions/mandrake/";
"http://mirrors.kernel.org/mandrake/";
     ]
   ]
*)

  []
  
  (*
let verbose_clients = 
  define_option fileTP_section ["verbose_clients"] 
  "level of verbosity when communicating with clients" 
    int_option 0
    
let verbose_servers = 
  define_option fileTP_section ["verbose_servers"] 
    "level of verbosity when communicating with servers" int_option 0
    *)

let network_options_prefix = define_option fileTP_section
    ["options_prefix"] "The prefix which is appended to options names
    when they are used in the telnet/WEB interfaces"
    string_option "FTP-"
  
let shortname o =
  Printf.sprintf "%s%s" !!network_options_prefix (shortname o)
  
let gui_fileTP_options_panel = 
  (*
  define_option fileTP_section ["gui_fileTP_options_panel"]
    "Which options are configurable in the GUI option panel, and in the
  fileTP section. Last entry indicates the kind of widget used (B=Boolean,T=Text)"
(list_option (tuple3_option (string_option, string_option, string_option)))
  *)
  [
(*    "Max Connected Ultrapeers", shortname max_ultrapeers, "T"; 
    "Max Known Ultrapeers", shortname max_known_ultrapeers, "T";
    "Max Known Peers", shortname max_known_peers, "T";    *)
    "Commit Downloads In Incoming Subdir", shortname commit_in_subdir, "T";
  ]
  
let network_name = "KaZaA"
