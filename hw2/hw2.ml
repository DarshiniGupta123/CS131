type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal


type ('nonterminal, 'terminal) parse_tree =
   Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal


let tuple_matcher1 (x,_) = x;;
let tuple_matcher2 (_, x) = x;;

let rec grammar_helper rules temp = match rules with
| [] -> []
| head::tail ->
   let h2 = tuple_matcher1 head in
   let h3 = tuple_matcher2 head in
   match h2 with
   | h2 when h2 = temp ->  (h3) :: (grammar_helper tail temp)
   | _ -> (grammar_helper tail temp)
;;

let convert_grammar gram1 =
((tuple_matcher1 gram1), (grammar_helper (tuple_matcher2 gram1)))
;;

let rec parse_tree_leaves2 tree = 
match tree with
 
| [] -> []
| head::tail -> parse_tree_helper head tail
and parse_tree_helper h t = match h with 
| Leaf l -> l :: parse_tree_leaves2 t
| Node (n, sub) -> (parse_tree_leaves2 sub) @ (parse_tree_leaves2 t)
;;

let parse_tree_leaves tree = parse_tree_leaves2 [tree];;

let rec matcher grammar symb rules accept frag  = match rules with
| [] -> None
| head :: tail -> 
   let m = matcher2 grammar symb (head) accept frag in  
   match m with
     | None -> matcher grammar symb tail accept frag
     | _ -> m

and matcher2 grammar symb rules accept frag = match rules with
| [] -> accept frag
| headR :: tailR -> match headR with
  | N n ->
     matcher grammar n ((snd grammar) n) (matcher2 grammar symb (tailR) accept) frag
  | T t -> match frag with 
     | [] -> None
     | head2 :: tail2 ->
       if head2 = t then matcher2 grammar symb tailR accept tail2
       else None
;;      


let make_matcher gram accept frag = 
  matcher gram (fst gram) ((snd gram) (fst gram)) accept frag
;;

let make_parser grammar frag =
  None
;;



