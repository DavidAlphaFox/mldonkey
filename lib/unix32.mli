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

type t

val create : string -> Unix.open_flag list -> int -> t
val close : t -> unit
(* val force_fd : t -> Unix.file_descr *)
  
(* val seek64 : t -> int64 -> Unix.seek_command -> int64 *)
val getsize64 : string -> int64
val ftruncate64 : t -> int64 -> unit

val close_all : unit -> unit

val fds_size : int
val filename : t -> string
val set_filename : t -> string -> unit
val max_cache_size : int ref
val mtime64 : string -> float
  
val flush : unit -> unit
val flush_fd : t -> unit
val buffered_write : t -> int64 -> string -> int -> int -> unit
val write : t -> int64 -> string -> int -> int -> unit
val max_buffered : int64 ref
  
val fd_of_chunk : t -> int64 -> int64 -> (Unix.file_descr * int64)
  
val read : t -> int64 -> string -> int -> int -> unit
val allocate_chunk :  t -> int64 -> int64 -> unit
  
val copy_chunk : t -> t -> int64 -> int64 -> int64 -> unit
  