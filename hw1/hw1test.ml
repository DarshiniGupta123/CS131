let subset_test3 = not (subset [1;2;3] [])
let equal_sets_test2 = equal_sets [1;4;4;2] [4;2;2;1]
let set_union_test3 = equal_sets (set_union [1;4;3] [2;5;3;2]) [1;2;3;4;5]
let set_intersection_test3 = equal_sets (set_intersection [1;2] []) []
let set_diff_test4 = equal_sets (set_diff [4;3;2;3;1] [4;3]) [2;1]
let computed_fixed_point_test4 = computed_fixed_point (=) (fun x -> x *. x) 100000. = infinity
					  
type days =
  | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday

let week =
  Monday,
  [Monday, [N Tuesday];
   Tuesday, [T"work"; N Thursday ];
   Wednesday, [T"khrgh"];
   Thursday, [N Wednesday];
   Friday, [N Saturday];
   Friday, [T"party"];
   Saturday, [N Monday];
   Saturday, [T"fun"; N Friday];
   Monday, [N Saturday]]

let giant_test3 = filter_reachable week = week
   

   
   
