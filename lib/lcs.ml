type 'a table = {
  equal : 'a -> 'a -> bool;
  xseq : 'a array;
  xlen : int;
  yseq : 'a array;
  ylen : int;
  matrix : int array array;
}

let compute_table ~equal l l' =
  let xseq = Array.of_list l in
  let yseq = Array.of_list l' in
  let xlen = Array.length xseq in
  let ylen = Array.length yseq in
  let matrix = Array.make_matrix xlen ylen 0 in
  let get x y = if x < 0 || y < 0 then 0 else matrix.(x).(y) in
  for x = 0 to xlen - 1 do
    for y = 0 to ylen - 1 do
      if equal xseq.(x) yseq.(y) then matrix.(x).(y) <- get (x - 1) (y - 1) + 1
      else
        let lcs1 = get (x - 1) y in
        let lcs2 = get x (y - 1) in
        matrix.(x).(y) <- Int.max lcs1 lcs2
    done
  done;
  { equal; xseq; xlen; yseq; ylen; matrix }

let read_length { matrix; xlen; ylen; _ } = matrix.(xlen - 1).(ylen - 1)
[@@ocaml.warning "-32"]

let read_one ~visited

let read_one { matrix; xlen; xseq; ylen; yseq; equal } =
  let get_left x y = if x < 1 then 0 else matrix.(x - 1).(y) in
  let get_up x y = if y < 1 then 0 else matrix.(x).(y - 1) in
  let rec rec_read_one acc x y =
    if x < 0 || y < 0 then acc
    else
      let lcs_len = matrix.(x).(y) in
      if Int.equal lcs_len 0 then acc
      else if equal xseq.(x) yseq.(y) then
        rec_read_one (xseq.(x) :: acc) (x - 1) (y - 1)
      else
        let lcs_left = get_left x y in
        let lcs_up = get_up x y in
        match Int.compare lcs_left lcs_up with
        | cmp when cmp >= 0 -> rec_read_one acc (x - 1) y
        | _ -> rec_read_one acc x (y - 1)
  in
  rec_read_one [] (xlen - 1) (ylen - 1)

let read_all {matrix; xlen; ylen; yseq; equal} =
  let get_left x y = if x < 1 then 0 else matrix.(x - 1).(y) in
  let get_up x y = if y < 1 then 0 else matrix.(x).(y - 1) in
  let rec rec_read_one acc x y =


let remove_common_prefix ~equal l l' =
  let rec aux acc l l' =
    match (l, l') with
    | [], [] -> (acc, [], [])
    | [], _ -> (acc, [], l')
    | _, [] -> (acc, l, [])
    | hd :: tl, hd' :: tl' when equal hd hd' -> aux (hd :: acc) tl tl'
    | _, _ -> (acc, l, l')
  in
  aux [] l l'

let remove_common_suffix ~equal l l' =
  let suffix, rev_remainder, rev_remainder' =
    remove_common_prefix ~equal (List.rev l) (List.rev l')
  in
  (suffix, List.rev rev_remainder, List.rev rev_remainder')

let one ~equal l l' =
  let rev_prefix, lend, lend' = remove_common_prefix ~equal l l' in
  let suffix, lmid, lmid' = remove_common_suffix ~equal lend lend' in
  match (lmid, lmid') with
  | [], [] -> l
  | _, [] -> l'
  | [], _ -> l
  | _, _ ->
      let tbl = compute_table ~equal lmid lmid' in
      let lcs_mid = read_one tbl in
      List.rev_append rev_prefix (lcs_mid @ suffix)

let all ~equal:_ _ _ = [ [] ]
