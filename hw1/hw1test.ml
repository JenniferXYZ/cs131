let subset_test0 =  subset [] [1;2];;
let subset_test1 =  not (subset [1;2] []);;
let subset_test2 = subset [1;2] [1;2;3];;
let subset_test3 = not (subset [1;3] [1;2;4]);;

(*let equal_sets_test0 = equal_sets [] [];;*)
let equal_sets_test1 = equal_sets [1;2;3] [2;3;1];;
let equal_sets_test2 = not(equal_sets [1;3;1] [2;1;3]);;
let equal_sets_test0 = not(equal_sets [1;3] [1;2;3]);;

let set_union_test0 = equal_sets (set_union [] [1;2;3]) [1;2;3];;
let set_union_test1 = equal_sets (set_union [3;1;3] [1;2;3]) [1;2;3];;
let set_union_test2 = equal_sets (set_union [] []) [];;


let set_intersection_test0 =
  equal_sets (set_intersection [] [1;2;3]) [];;
let set_intersection_test1 =
  equal_sets (set_intersection [3;1;3] [1;2;3]) [1;3];;
let set_intersection_test2 =
  equal_sets (set_intersection [1;2;3;4] [3;1;2;4]) [4;3;2;1];;
 let set_intersection_test3 =
  equal_sets (set_intersection [1;2;3] []) [];;
  let set_intersection_test4 =
  equal_sets (set_intersection [] []) [];;


let set_diff_test0 = equal_sets (set_diff [1;3] [1;4;3;1]) [];;
let set_diff_test1 = equal_sets (set_diff [4;3;1;1;3] [1;3]) [4];;
let set_diff_test2 = equal_sets (set_diff [4;3;1] []) [1;3;4];;
let set_diff_test3 = equal_sets (set_diff [] [4;3;1]) [];;
let set_diff_test4 = equal_sets (set_diff [] []) [];;

let computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x / 2) 1000000000 = 0;;
let computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x *. 2.) 1. = infinity;;
let computed_fixed_point_test2 =
  computed_fixed_point (=) sqrt 10. = 1.;;
let computed_fixed_point_test3 =
  ((computed_fixed_point (fun x y -> abs_float (x -. y) < 1.)
			 (fun x -> x /. 2.)
			 10.)
   = 1.25);;

type giant_nonterminals = |Conversation|Sentence|Grunt|Snore|Shout|Quiet|Scream;;

let is_reachable_symbol_test0 = is_reachable_symbol Sentence [N Sentence; T","; N Conversation];;
let is_reachable_symbol_test1 = not (is_reachable_symbol Sentence [T","]);;
let is_reachable_symbol_test2 = not (is_reachable_symbol Sentence [N Quiet]);;

let is_reachable_from_pair_test0 = is_reachable_from_pair Sentence [Grunt, [T"khrgh"];
   Shout, [T"aooogah!"];
   Sentence, [N Quiet];
   Conversation, [N Sentence; T","; N Conversation]];;

let is_reachable_from_pair_test0 = is_reachable_from_pair Quiet [Grunt, [T"khrgh"];
   Shout, [T"aooogah!"];
   Sentence, [N Quiet];];;

let reachable_list_test0 = equal_sets (reachable_list Conversation 
	[Sentence, [N Shout]; Conversation, [N Snore]; Conversation, [N Sentence; T","; N Conversation]] []) 
	[Conversation, [N Snore]; Conversation, [N Sentence; T","; N Conversation]];;

let giant_grammar =
  Conversation,
  [Snore, [T"ZZZ"];
   Quiet, [];
   Grunt, [T"khrgh"];
   Shout, [T"aooogah!"];
   Sentence, [N Quiet];
   Sentence, [N Grunt];
   Sentence, [N Shout];
   Conversation, [N Snore];
   Conversation, [N Sentence; T","; N Conversation]];;

let giant_test0 =filter_reachable giant_grammar = giant_grammar;;

type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num;;

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]];;

let awksub_grammar = Expr, awksub_rules;;

let awksub_test0 =
  filter_reachable awksub_grammar = awksub_grammar;;

let awksub_test1 =
  filter_reachable (Expr, List.tl awksub_rules) = (Expr, List.tl awksub_rules);;

let awksub_test2 =
  filter_reachable (Lvalue, awksub_rules) = (Lvalue, awksub_rules);;

let awksub_test3 =
  filter_reachable (Expr, List.tl (List.tl awksub_rules)) =
    (Expr,
     [Expr, [N Expr; N Binop; N Expr];
      Expr, [N Lvalue];
      Expr, [N Incrop; N Lvalue];
      Expr, [N Lvalue; N Incrop];
      Lvalue, [T "$"; N Expr];
      Incrop, [T "++"];
      Incrop, [T "--"];
      Binop, [T "+"];
      Binop, [T "-"]]);;

let awksub_test4 =
  filter_reachable (Expr, List.tl (List.tl (List.tl awksub_rules))) =
    (Expr,
     [Expr, [N Lvalue];
      Expr, [N Incrop; N Lvalue];
      Expr, [N Lvalue; N Incrop];
      Lvalue, [T "$"; N Expr];
      Incrop, [T "++"];
      Incrop, [T "--"]]);;

type pokemon_nonterminals = |Bulbasaur|Charmander|Squirtle|Caterpie|Matepod|Pikachu|Raichu;;
let pokemon_rules = 
[Bulbasaur, [N Charmander];
 Bulbasaur, [N Caterpie; N Raichu];
 Bulbasaur, [T "UGH"; N Bulbasaur; T "UGH"];
 Bulbasaur, [N Squirtle; N Raichu];
 Charmander, [T "AHH"];
 Charmander, [N Pikachu; N Raichu];
 Charmander, [N Raichu; N Pikachu];
 Charmander, [N Bulbasaur];
 Squirtle, [T "("; N Charmander; T ")"];
 Caterpie, [T "BUGS ARE DISTUSTING"];
 Matepod, [T "HELLO WORLD"];
 Pikachu, [N Bulbasaur; N Squirtle];
 Pikachu, [T "PIKA"; N Raichu; T "PIKAPIKA"];
 Pikachu, [N Charmander; N Matepod];
 Raichu, [T "HEHE"];
 Raichu, [N Caterpie]];;

 let pokemon_grammar = Bulbasaur, pokemon_rules;;

let pokemon_test_0 = filter_reachable pokemon_grammar = pokemon_grammar;;
let pokemon_test_1 = filter_reachable (Charmander, List.tl pokemon_rules) = (Charmander, List.tl pokemon_rules);;
let pokemon_test_2 = filter_reachable (Raichu, List.tl (List.tl pokemon_rules)) = (Raichu, [
  Caterpie, [T "BUGS ARE DISTUSTING"];
  Raichu, [T "HEHE"];
  Raichu, [N Caterpie]]);;
let pokemon_test_3 = filter_reachable (Pikachu, []) = (Pikachu, []);;
let pokemon_test_4 = filter_reachable (Pikachu, List.tl (List.tl (List.tl pokemon_rules))) = (Pikachu,List.tl (List.tl (List.tl pokemon_rules)));;
let pokemon_test_5 = filter_reachable (Caterpie, List.tl (List.tl pokemon_rules)) = (Caterpie, [Caterpie, [T "BUGS ARE DISTUSTING"]]);;
