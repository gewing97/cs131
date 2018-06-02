open List;;
open Pervasives;;

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal

let convert_grammar gram1 = 
    let rec find_non_terminal_rules r n = 
        match r with 
        | [] -> []
        | (lhs, rhs)::tail -> 
            if (lhs = n) then rhs::(find_non_terminal_rules tail n)
            else find_non_terminal_rules tail n
    in
    match gram1 with 
    | (n, rhs) -> (n, find_non_terminal_rules rhs)
;;

let parse_prefix gram = 
    let get_rule_list n = function | (_,rhs) -> rhs n
    in
    let rec match_first_mem gram rules accept deriv frag = 
        if length frag < length rules then None else (
        match rules with 
        | [] -> (accept deriv frag)
        | (T term_rule)::other_rules->
            (match frag with 
            | [] -> None
            | f1::f -> if (f1 = term_rule) then match_first_mem gram other_rules accept deriv f else None )
        | (N rule)::other_rules -> move_horizontal gram rule other_rules (get_rule_list rule gram) accept deriv frag
        )
    and move_horizontal gram curr_nonterm rules alt accept deriv frag =
        match alt with
        | [] -> None
        | h::t ->
                match match_first_mem gram (h @ rules) accept (deriv @ [(curr_nonterm, h)]) frag with
                    | None -> move_horizontal gram curr_nonterm rules t accept deriv frag
                    | good -> good
    in
    let find_root g = 
        match g with 
        | (lhs, _) -> lhs 
    in
    let make_matcher gram accept frag =
        move_horizontal gram (find_root gram) [] (get_rule_list (find_root gram) gram) accept [] frag
    in
    make_matcher gram
