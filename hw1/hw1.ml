open List;;
open Pervasives;;

let rec subset a b = 
    match a with
    | [] -> true
    | head::tail -> (
        match (mem head b) with
        | true -> (subset tail b)
        | false -> false 
        (*if (mem head b) then (subset tail b) else false*)
    )
;;

let equal_sets a b = 
    (subset a b) && (subset b a)
;;

let rec set_union a b = 
    match a with 
    | [] -> b
    | head::tail-> (
        match (mem head b) with
        | true -> set_union tail b
        | false -> set_union tail (head::b))
        (*
        if (mem head b)  then set_union tail b
        else (set_union tail (head::b))
        *)
;;

let rec set_intersection a b = 
    match a with 
    | [] -> []
    | head::tail -> (
        match (mem head b) with
        | true -> head::(set_intersection tail b)
        | false -> set_intersection tail b )
        (*if (mem head b) then head::(set_intersection tail b)
        else set_intersection tail b *)
;;

let rec set_diff a b =
    match a with 
    | []->[]
    | head::tail ->(
        match (mem head b) with
        | true -> set_diff tail b
        | false -> head::(set_diff tail b))
        (*
        if mem head (set_intersection a b) then set_diff tail b
        else head::(set_diff tail b)
        *)
;;


let rec computed_fixed_point eq f x =
    match (eq (f x) x) with
    | true -> x 
    | false -> computed_fixed_point eq f (f x)
    (*
    if (eq (f x) x) then x
    else computed_fixed_point eq f (f x)
    *)
;; 


(*define anonymous function that recursevly calls itself with p*)
(*have computed period point call itself with x = (f x)*)
let rec computed_periodic_point eq f p x = 
    let rec is_periodic eq f p x orig =
        match p with 
        | 0 -> true
        | 1 -> (eq (f x) orig)
            (*
            if (eq (f x) orig) then true
            else false *)
        | _ -> is_periodic eq f (p-1) (f x) orig
    in
        match (is_periodic eq f p x x) with
        | true -> x
        | false -> (computed_periodic_point eq f p (f x))
        (*
        if (is_periodic eq f p x x) then x
        else (computed_periodic_point eq f p (f x))*)
;;

let rec while_away s p x =
    match (p x) with
    | true -> x::(while_away s p (s x))
    | false -> []
    (*
    if not (p x) then []
    else x::(while_away s p (s x))
    *)
;;

(* another implementation of computed_fixed_point*)
(*let computed_fixed_point eq f x = computed_periodic_point eq f 1 x;; *)

let rec rle_decode lp = 
    let rec rep_symbol n s =
        match n with 
        | 0 -> []
        | _ -> s::(rep_symbol (n-1) s)
    in
    match lp with
    | [] -> []
    | (num,sym)::tail -> append (rep_symbol num sym) (rle_decode tail)
;;


type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal


let filter_blind_alleys g = 
    (*create a list of all terminals present*)
    let rec find_terminals grammars = 
        (*add to list any terminal found rules*)
        let rec find_sub_terminals rhs = 
        match rhs with
        | [] -> []
        | (T some_term)::tail -> (T some_term)::(find_sub_terminals tail)
        | (N some_non_term)::tail2 -> (find_sub_terminals tail2)
        in
        match grammars with
        | [] -> []
        | (lhs, rhs)::tail1 -> (
            match rhs with 
            | [] -> find_terminals tail1
            | (N non_term)::tail2 -> set_union (find_sub_terminals tail2) (find_terminals tail1)
            | (T term)::tail3 -> set_union (find_sub_terminals tail3) (set_union [T term] (find_terminals tail1)) 
            )
    in
    (*find good nonterminals by taking a patch from the terminals*)
    let rec find_good_nonterms depth terms grammars =
        (*scan list for rules in the set of good non terminals and terminals*)
        let rec scan_list terms grammars =
        match grammars with
        | [] -> terms
        | (lhs, rhs)::tail -> (
                match (subset rhs terms) with
                | true -> set_union [N lhs] (if mem (N lhs) terms then (scan_list terms tail) else scan_list (set_union [N lhs] terms) grammars) 
                | false -> scan_list terms tail )
        (*
               if (subset rhs terms) then 
                   set_union [N lhs] (if mem (N lhs) terms then (scan_list terms tail) else scan_list (set_union [N lhs] terms) grammars) 
                else scan_list terms tail) *)
        in
        match depth with
        | 0 -> terms
        | _ -> find_good_nonterms (depth -1) (scan_list terms grammars) grammars
    in
    (*check if each grammar is a subset of the terminals and good nonterminals*)
    let rec remove_blind_alley good_stuff grammars = 
        match grammars with 
            | [] -> []
            | (lhs, rhs)::tail -> (
                match (subset rhs good_stuff) with
                | true -> (lhs, rhs)::(remove_blind_alley good_stuff tail)
                | false -> remove_blind_alley good_stuff tail )
                (*
                if (subset rhs good_stuff) then (lhs, rhs)::(remove_blind_alley good_stuff tail)
                else remove_blind_alley good_stuff tail *)
    in
    match g with
        | (start, rules) ->
        (start, remove_blind_alley (find_good_nonterms (length rules) (find_terminals rules) rules) rules)
        (*create a tuple of start and good grammars, 
        good grammars defined by finding all the terminals,
        finding all the non terminals connected to the terminals,
        then removing all rules not in the set composed of terminals and good non_terminals*)
;;

