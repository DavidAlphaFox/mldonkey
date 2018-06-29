(* Copyright 2001, 2002 b52_simon :), b8_bavard, b8_fee_carabine, INRIA *)
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

open Printf2
(* 定义数据类型 *)
type value =
  | String of string
  | Int of int64
  | List of value list
  | Dictionary of (string * value) list

let decode s =
  let len = String.length s in (* 计算字符串的长度 *)
  let rec decode s pos len = (* 第归解析数据 *)
    if pos >= len then assert false; (* 触发报错 *)
    match s.[pos] with
     '0'..'9' -> (* 字符串数据 "abc" => 3:abc *)
        let rec iter_i s pos len = (* 寻找 `:` 的位置  *)
           if pos = len then pos else
           match s.[pos] with
             '0' .. '9' -> iter_i s (pos+1) len
            | ':' -> pos
            | _ -> assert false
        in
        let end_pos = iter_i s (pos+1) len in (* 获取字符串最后一个位置 *)
        let size = int_of_string (String.sub s pos (end_pos-pos)) in (* 编码中字符串的长度 *)
        String (String.sub s (end_pos+1) size), (end_pos+1+size) (* 截取对应的字符串 *)
   | 'i' -> (* 整形数据 123 => i123e i开头e结尾 *)
        let rec iter_i s pos len =
           if pos = len then assert false;
           match s.[pos] with
            | 'e' -> pos
            | _ -> iter_i s (pos+1) len
        in
        let end_pos = iter_i s (pos+1) len in
        let number = String.sub s (pos+1) (end_pos-pos-1) in
        (Int (try Int64.of_string number with _ -> Int64.of_float (float_of_string number))),
          (end_pos+1)
   | 'l' -> (* 列表数据 List<"abc", 123> => l3:abci123ee l开头e结尾 *)
        let rec iter s pos len list =
          if pos = len then assert false;
          match s.[pos] with
            | 'e' -> List (List.rev list), (pos+1)
            | _ ->
               let v, pos = decode s pos len in
               iter s pos len (v :: list)
        in
        iter s (pos+1) len []
   | 'd' -> (* 字典数据 Dictionary<{"name":"create chen"},{"age":23}> => d4:name11:create chen3:agei23ee d开头e结尾 *)
        let rec iter s pos len list =
          if pos = len then assert false;
          match s.[pos] with
            | 'e' -> Dictionary (List.rev list), (pos+1)
            | _ ->
              match decode s pos len with
              | (String key,pos) ->
                let v, pos = decode s pos len in
                iter s pos len ((key,v) :: list)
              | _ -> assert false 
        in
        iter s (pos+1) len []
   | _ -> assert false
  in
  assert (len > 0);
  let (v,pos) = decode s 0 len in
  v

let encode ?(strict=true) v =
  let buf = Buffer.create 100 in
  let encode_string s = Printf.bprintf buf "%d:%s" (String.length s) s in
  let rec encode v =
    match v with
    | String s -> encode_string s
    | Int i -> Printf.bprintf buf "i%Lde" i
    | List list ->
       Buffer.add_char buf 'l';
       List.iter encode list;
       Buffer.add_char buf 'e'
    | Dictionary list ->
       Buffer.add_char buf 'd';
       (* When calculating hash for the torrent file we leave the dictionary
       "as is" in order to get the same hash as the client that created
       this torrent even when the keys are not sorted (as required by BEP-3) *)
       let list = if strict then List.sort (fun (s1, _) (s2, _) -> compare s1 s2) list else list in
       List.iter (fun (key,v) -> encode_string key; encode v) list;
       Buffer.add_char buf 'e'
  in
  encode v;
  Buffer.contents buf

let print b =
  let buf = Buffer.create 100 in
  let print_string s =
    if String.length s > 200 then
      Printf.bprintf buf " \"%s...\"" (String.escaped (String.sub s 0 200))
    else
      Printf.bprintf buf "\"%s\"" (String.escaped s)
  in
  let rec print v =
    match v with
    | String s -> print_string s
    | Int i -> Printf.bprintf buf " %Ld" i
    | List list ->
       Printf.bprintf buf " [\n";
       List.iter (fun v -> print v; Printf.bprintf buf ";\n")  list;
       Printf.bprintf buf " ]\n";
    | Dictionary list ->
       Printf.bprintf buf " {\n";
       List.iter (fun (key,v) -> print_string key; Buffer.add_string buf " = "; print v; Printf.bprintf buf ";\n")  list;
       Printf.bprintf buf " }\n";
  in
  print b;
  Buffer.contents buf

