let accept_comment deriv suff =
    let rec find_last_entry = function
    | [] -> []
    | h::[] -> [h]
    | h::t -> find_last_entry t
    in
    match suff with 
    | [] -> None
    | h::t -> if h = "/*" then 
        (match find_last_entry t with
            | [] -> None 
            | l::_ -> if l = "*/" then Some(deriv, suff)
        else None)
    else None

let accept_all derivation string = Some (derivation, string)


type some_nonterminals = 
    | Function | Def | Paramlist | Param | Name | Char | Special


let mygrammar_1 = 
    (Function, function 
        | Function -> [[N Name; N Paramlist; T ";" ; N Def]; 
                        [N Name; N Paramlist; T ";"]]
        | Def -> [[N Function]] 
        | Paramlist -> [[T "("; N Param; T ")"]]
        | Param -> [[N Name; T ","; N Param]; 
                    [N Name]]
        | Name -> [[N Char; N Name]; 
                    [N Char]]
        | Char -> [[T "a"];[T "b"];[T "c"];[T "d"];[T "e"];[T "f"];
                    [T "g"];[T "h"];[T "i"];[T "j"];[T "k"];[T "l"];
                    [T "m"];[T "n"];[T "o"];[T "p"];[T "q"];[T "r"];
                    [T "s"];[T "t"];[T "u"];[T "v"];[T "w"];[T "x"];
                    [T "y"]; [T "z"]]
    )


(* intent of this is to check that it backtracks correctly*)
(* it follows the first definition of function first before 
    reaching the end and finding there's nothing that conforms 
    to definition of def, thus it must backtrack to original definition
    of function and use the second definition, undoing all the parsing 
    it did for the parameter list*)
let test_1 = parse_prefix mygrammar_1 accept_comment 
    ["t";"e";"s";"t";"(";"a";"r";"g";"o";",";"a";"r";
    "g";"a";"r";"g";"t";")";";";"/*";
    "b";"a";"c";"k";"t";"r";"a";"c";"k";
    "m";"o";"r";"e";"*/"]
    = Some
    ([(Function, [N Name; N Paramlist; T ";"]); 
    (Name, [N Char; N Name]); (Char, [T "t"]); 
    (Name, [N Char; N Name]); (Char, [T "e"]);
    (Name, [N Char; N Name]); (Char, [T "s"]); 
    (Name, [N Char]); (Char, [T "t"]); 
    (Paramlist, [T "("; N Param; T ")"]);
    (Param, [N Name; T ","; N Param]); 
    (Name, [N Char; N Name]); (Char, [T "a"]); 
    (Name, [N Char; N Name]); (Char, [T "r"]);
    (Name, [N Char; N Name]); (Char, [T "g"]); 
    (Name, [N Char]); (Char, [T "o"]); 
    (Param, [N Name]); 
    (Name, [N Char; N Name]); (Char, [T "a"]); 
    (Name, [N Char; N Name]); (Char, [T "r"]);
    (Name, [N Char; N Name]); (Char, [T "g"]); 
    (Name, [N Char; N Name]); (Char, [T "a"]); 
    (Name, [N Char; N Name]); (Char, [T "r"]);
    (Name, [N Char; N Name]); (Char, [T "g"]); 
    (Name, [N Char]); (Char, [T "t"])],
    ["/*"; "b"; "a"; "c"; "k"; 
    "t"; "r"; "a"; "c"; "k"; 
    "m"; "o"; "r"; "e";
   "*/"]);;


(*can it handle loops in definitions*)
(*def is defined to be a function *)
let test_2 = parse_prefix mygrammar_1 accept_all
    ["t";"(";"e";")"; ";";
    "s";"(";"t";",";"t";")";";";
    "w";"(";"o";")";";";
    "/*"; "loops";"*/"]
    = Some
   ([(Function, [N Name; N Paramlist; T ";"; N Def]);
    (Name, [N Char]); (Char, [T "t"]); 
    (Paramlist, [T "("; N Param; T ")"]);
    (Param, [N Name]); (Name, [N Char]); (Char, [T "e"]);
    (Def, [N Function]); 
    (Function, [N Name; N Paramlist; T ";"; N Def]);
    (Name, [N Char]); (Char, [T "s"]); 
    (Paramlist, [T "("; N Param; T ")"]);
    (Param, [N Name; T ","; N Param]); 
    (Name, [N Char]); (Char, [T "t"]);
    (Param, [N Name]); (Name, [N Char]); (Char, [T "t"]);
    (Def, [N Function]); 
    (Function, [N Name; N Paramlist; T ";"]);
    (Name, [N Char]); (Char, [T "w"]); 
    (Paramlist, [T "("; N Param; T ")"]);
    (Param, [N Name]); (Name, [N Char]); (Char, [T "o"])],
    ["/*"; "loops"; "*/"]);;

