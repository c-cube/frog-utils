
(* This file is free software, part of frog-utils. See file "license" for more details. *)

module StrMap = CCMap.Make(String)
module StrSet = CCSet.Make(String)
module Int_map = CCMap.Make(CCInt)

module Str = struct
  let split ~by s =
    let i = String.index s by in
    String.sub s 0 i, String.sub s (i+1) (String.length s -i-1)
end

module Blob = struct

  type 'a decoder = string -> ('a, string) result

  type 'a t = 'a decoder * string

  let custom f hash : _ t = (f, hash)

  let string =
    custom (fun s -> CCResult.return s)

  let yojson f hash =
    let f' s = f @@ Yojson.Safe.from_string s in
    custom f' hash

end

