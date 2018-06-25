let rec fact accum = function 
    | 0 -> accum
    | x -> fact (x * accum) (x - 1);;

let rec bad_fact = function 
    | 0 -> 1
    | x -> x * (bad_fact (x - 1)) ;;

    