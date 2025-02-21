let pp_int_list = Fmt.(Dump.list int)
let equal = Int.equal

let test_one l l' =
  let lcs = Struct_diff__Lcs.one ~equal l l' in
  Format.printf "%a\n" pp_int_list lcs

let test_all l l' =
  let all_lcs = Struct_diff__Lcs.all ~equal l l' in
  Format.printf "%a\n" (Fmt.Dump.list pp_int_list) all_lcs

let%expect_test "empty" =
  let l, l' = ([], []) in
  test_one l l';
  [%expect {|[]|}];
  test_all l l';
  [%expect {|[[]]|}]

let%expect_test "id" =
  let l, l' = ([ 1; 2; 3 ], [ 1; 2; 3 ]) in
  test_one l l';
  [%expect {|[1; 2; 3]|}];
  test_all l l';
  [%expect {|[[]]|}]

let%expect_test "no sharing" =
  let l, l' = ([ 1; 2; 3 ], [ 4; 5; 6 ]) in
  test_one l l';
  [%expect {|[]|}];
  test_all l l';
  [%expect {|[[]]|}]

let%expect_test "simple" =
  let l, l' = ([ 5; 2; 6; 3; 1; 8 ], [ 2; 7; 6; 1; 9 ]) in
  test_one l l';
  [%expect {|[2; 6; 1]|}];
  test_all l l';
  [%expect {|[[]]|}]

let%expect_test "multiple" =
  let l, l' = ([ 1; 3; 2; 1; 4 ], [ 3; 1; 2 ]) in
  test_one l l';
  [%expect {|[1; 2]|}];
  test_all l l';
  [%expect {|[[]]|}]

let%expect_test "common prefix and suffix" =
  let l, l' = ([ 1; 2; 1; 3; 2; 1; 4; 2; 1 ], [ 1; 2; 3; 1; 2; 2; 1 ]) in
  test_one l l';
  [%expect {|[1; 2; 1; 2; 2; 1]|}];
  test_all l l';
  [%expect {|[[]]|}]
