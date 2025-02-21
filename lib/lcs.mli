val one : equal:('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list
(** [one ~equal l l'] returns the Longest Common Subsequence shared by
    [l] and [l']. *)

val all : equal:('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list list
(** [all ~equal l l'] returns all Longest Common Subsequences shared by
    [l] and [l']. Returns a non empty list. Returns [[[]]] if the two sequences
    share no value. *)
