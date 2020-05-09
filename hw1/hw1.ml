let rec subset a b = match a with
  [] -> true
| _ -> if List.mem (List.hd a) b then subset (List.tl a) b else false
;;

let equal_sets a b = 
if subset a b && subset b a then true else false
;;

let set_union a b = 
a @ b;;


let rec set_intersection a b = match a with
| [] -> []
| _ -> if List.mem (List.hd a) b then (List.hd a)::(set_intersection (List.tl a) b)
else set_intersection (List.tl a) b
;;

let rec set_diff a b = match a with
| [] -> []
| _ -> if List.mem (List.hd a) b then (set_diff (List.tl a) b)
else (List.hd a)::(set_diff (List.tl a) b)
;;

let rec  computed_fixed_point eq f x = 
if eq (f x) x then x else computed_fixed_point eq f (f x) ;;

let tuple_matcher (x,_) = x;;
let tuple_matcher2 (_, x) = x;;

type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal
;;

let equal_second_elem_sets a b = 
equal_sets (tuple_matcher2 a) (tuple_matcher2 b)
;;

let which_type w = match w with
| N n -> n
| T t -> t
;;

let rec filtering rhs = match rhs with
[] -> []
| N  head::tail -> head :: (filtering (List.tl rhs))
| T  head::tail -> filtering (List.tl rhs) 
;;

let rec get_reachable_symbols params = 
let rules = tuple_matcher params in
let reachable_symbols = tuple_matcher2 params in

match rules with
| [] -> reachable_symbols
| _ -> let tmp_rule = List.hd rules in
       let rest_rules = tuple_matcher2 tmp_rule in 
       let tmp_symbol = tuple_matcher tmp_rule in      
       let t = List.tl rules in
       if List.mem tmp_symbol reachable_symbols then let right_hand_side = filtering rest_rules in let nextVal = set_union reachable_symbols right_hand_side in 
                                                     get_reachable_symbols (t, nextVal)
       else
         get_reachable_symbols (t, reachable_symbols)
;;

let rec get_reachable params = 
let n_symbol = get_reachable_symbols (tuple_matcher params, tuple_matcher2 params) in
if equal_sets (tuple_matcher2 params) n_symbol then (tuple_matcher2 params) else get_reachable (tuple_matcher params, n_symbol)
;; 

let filter_reachable g = 
let start_symbol = tuple_matcher g in 
let rules = tuple_matcher2 g in 

let reachable_symbols = get_reachable (rules, [start_symbol]) in
let filtered_rules = List.filter (fun r -> List.mem (tuple_matcher r) reachable_symbols) rules in
(start_symbol, filtered_rules)
;;


