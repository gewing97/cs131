let my_subset_test0 = subset [1;2;3] [1;2;3]
let my_subset_test1 = subset [[1;2;3]] [[1;2;3];[1;2]] 
let my_subset_test2 = not (subset ["one";"two";"three"] ["four";"five";"six"])
let my_subset_test3 = subset ["one";"two";"three"] ["one";"two";"three";"four";"five";"six"]

let my_equal_sets_test0 = equal_sets [] []
let my_equal_sets_test1 = not (equal_sets [] [1;2;3])
let my_equal_sets_test2 = equal_sets ["one";"two";"three"] ["one";"two";"three"]

let my_set_union_test0 = equal_sets (set_union ["words"] ["words";"werds";"wurdz"] ) ["words";"werds";"wurdz"]
let my_set_union_test1 = equal_sets (set_union [1;2] []) [1;2]
let my_set_union_test2 = equal_sets (set_union [["subset"];["sub";"subset"]] [["hello subsets"]]) [["subset"];["sub";"subset"];["hello subsets"]]

let my_set_intersection_test0 = equal_sets (set_intersection ["set1"] ["set2"]) []
let my_set_intersection_test1 = equal_sets (set_intersection [2;5;0;6;2;4] [1;8;0;0;2;7;3;8;2;5;5]) [2;5;0]
let my_set_intersection_test2 = equal_sets (set_intersection ["25"; "or"; "6"; "to"; "4"] ["to";"be";"or";"not"]) ["or";"to"]

let my_set_diff_test0 = equal_sets (set_diff ["bold";"as";"love"] ["love";"me";"do"]) ["bold"; "as"]
let my_set_diff_test1 = equal_sets (set_diff [2;5;0;6;2;4] [1;8;0;0;2;7;3;8;2;5;5]) [6;4]
let my_set_diff_test2 = equal_sets (set_diff [1;2;3] [3;2;1]) []

let my_computed_fixed_point_test0 = ((computed_fixed_point (=) (fun fn -> fn + 0) 1) = 1)
let my_computed_fixed_point_test1 = ((computed_fixed_point (=) (fun fn -> fn / fn) 700034) = 1)
let my_computed_fixed_point_test2 = ((computed_fixed_point (=) (fun fn -> (fn *. fn)) 8.) = infinity)

let my_computed_periodic_point_test0 = ((computed_periodic_point (=) (fun fn -> fn) 0 1) = 1)
let my_computed_periodic_point_test1 = ((computed_periodic_point (=) (fun fn -> (fn *. fn)) 1 0.2) = 0.)

let my_while_away_test0 = (equal_sets (while_away (fun x -> x-2) (fun fn -> ((fn mod 2) = 0) && (fn > 0)) 10) [2;4;6;8;10])
let my_while_away_test0 = (equal_sets (while_away (fun x -> x-2) (fun fn -> ((fn mod 4) = 0) && (fn > 0)) 10) [])


let my_rle_decode_test0 = (equal_sets (rle_decode [2,2;4,4]) [2;2;4;4;4;4])
let my_rle_decode_test1 = (equal_sets (rle_decode [1,"and";2,"on";2,"and on";1,"we go"]) ["and" ;"on" ;"on" ;"and on" ;"and on" ;"we go"])


type just_college_thoughts =
    | Freshmen | Sophomore | Happiness | Sad | Depression | Expletive | Dropout


let college_thoughts_grammar = 
    Freshmen,
    [Freshmen, [N Happiness];
    Freshmen, [N Sad];
    Freshmen, [N Happiness; N Sad; T "confusion"];
    Freshmen, [N Sophomore];
    Sophomore, [N Sophomore];
    Sophomore, [N Sad; N Depression];
    Sophomore, [N Depression];
    Depression, [N Depression];
    Sad, [N Expletive];
    Expletive, [N Dropout; T "welp"];
    Expletive, [T "small bad word"];
    Expletive, [T "big bad word"];
    Expletive, [T "many bad word"];
    Dropout, [T "such is life"]]

let my_blind_alley_test0 = (filter_blind_alleys college_thoughts_grammar
    =
    (Freshmen,
    [Freshmen, [N Sad];
    Sad, [N Expletive];
    Expletive, [N Dropout; T "welp"];
    Expletive, [T "small bad word"];
    Expletive, [T "big bad word"];
    Expletive, [T "many bad word"];
    Dropout, [T "such is life"]]))



