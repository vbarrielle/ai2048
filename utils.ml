
open Core.Std

let unwrap = function
  | None -> assert false
  | Some x -> x
