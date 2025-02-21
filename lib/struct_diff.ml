type ('a, 'diff) diff_fun = 'a -> 'a -> 'diff option

module Int = struct
  type t = int * int

  let diff i i' = if Int.equal i i' then None else Some (i, i')
end

module Result = struct
  type ('a, 'adiff, 'err, 'errdiff) t =
    | No_share of ('a, 'err) result * ('a, 'err) result
    | Ok_diff of 'adiff
    | Error_diff of 'errdiff

  let diff diff_a diff_err res res' =
    match (res, res') with
    | Ok a, Ok a' -> Option.map (fun diff -> Ok_diff diff) (diff_a a a')
    | Error e, Error e' ->
        Option.map (fun diff -> Error_diff diff) (diff_err e e')
    | _ -> Some (No_share (res, res'))
end

module Collection = struct
  type 'a elm = Added of 'a | Removed of 'a
  type 'a t = 'a elm list
end

module Ordered_collection = struct
  type ('a, 'diff) elm = Added of 'a | Removed of 'a | Modified of 'diff
  type ('a, 'diff) t = ('a, 'diff) elm option list
end

module List = struct
  let diff diff_a l l' =
    let open Ordered_collection in
    let rec rec_diff acc l l' =
      match (l, l') with
      | [], [] -> List.rev acc
      | hd :: tl, hd' :: tl' ->
          let elm = Option.map (fun x -> Modified x) (diff_a hd hd') in
          rec_diff (elm :: acc) tl tl'
      | [], hd :: tl -> rec_diff (Some (Added hd) :: acc) [] tl
      | hd :: tl, [] -> rec_diff (Some (Removed hd) :: acc) tl []
    in
    rec_diff [] l l'
end

module Option = struct
  type ('a, 'diff) t = No_share of 'a option * 'a option | Some_diff of 'diff

  let diff diff_a opt opt' =
    match (opt, opt') with
    | None, None -> None
    | Some a, Some a' -> Option.map (fun diff -> Some_diff diff) (diff_a a a')
    | _ -> Some (No_share (opt, opt'))
end
