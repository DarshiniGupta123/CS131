(*let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x
   *)
let accept_all string = Some string

type awksub_nonterminals =
B | D | Y

let awkish_grammar =

(B, 
 function
  | B ->
      [[N D; N D; N Y]]; 
  | D ->
      [[T"01"]; [T"02"]; [T"03"]]
  | Y ->
      [[T"1999"]; [T"1998"]; [T"2000"]])	

let matcher_test = (make_matcher awkish_grammar accept_all ["01"; "02"; "1999"]) = Some []
;;

let parser_test = match make_parser awkish_grammar ["01"; "02"; "1999"] with 
| Some tree -> if parse_tree_leaves tree = ["01"; "02"; "1999"] then true else false
| None -> false 
;;
