type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num

let accept_all string = Some string;;
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x;;

 let awkish_grammar =
  (Expr,
   function
     | Expr ->
         [[N Term; N Binop; N Expr];
          [N Term]]
     | Term ->
	 [[N Num];
	  [N Lvalue];
	  [N Incrop; N Lvalue];
	  [N Lvalue; N Incrop];
	  [T"("; N Expr; T")"]]
     | Lvalue ->
	 [[T"$"; N Expr]]
     | Incrop ->
	 [[T"++"];
	  [T"--"]]
     | Binop ->
	 [[T"+"];
	  [T"-"]]
     | Num ->
	 [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
	  [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]]);;

(* This one might take a bit longer.... *)
(* let test4 =
 ((make_matcher awkish_grammar accept_all
     ["("; "$"; "8"; ")"; "-"; "$"; "++"; "$"; "--"; "$"; "9"; "+";
      "("; "$"; "++"; "$"; "2"; "+"; "("; "8"; ")"; "-"; "9"; ")";
      "-"; "("; "$"; "$"; "$"; "$"; "$"; "++"; "$"; "$"; "5"; "++";
      "++"; "--"; ")"; "-"; "++"; "$"; "$"; "("; "$"; "8"; "++"; ")";
      "++"; "+"; "0"])
  = Some []);;
 *)

type giant_nonterminals =
  | Conversation | Sentence | Grunt | Snore | Shout;;

let giant_grammar =
( Conversation,
        function
          | Snore -> [ [T "ZZZ"] ]
          | Conversation -> [ [N Sentence; T ","; N Conversation];
                              [N Snore]; ]
          | Sentence -> [ [N Grunt];
                          [N Shout] ]
          | Grunt ->  [ [T"khrgh"] ]
          | Shout -> [ [T"aooogah!"] ] ) ;;

let make_matcher_test0 =
  ((make_matcher giant_grammar accept_all ["ha"]) = None) ;;

let make_matcher_test1 =
  ((make_matcher giant_grammar accept_all["ZZZ"; "aooogah!"])
   = Some ["aooogah!"] ) ;;

let make_matcher_test2 =
     ((make_matcher giant_grammar accept_empty_suffix ["khrgh"; "aooogah!"; ","; "ZZZ"; "ZZZ"]) = None) ;;

let make_parser_test_0 = ((make_parser giant_grammar ["khrgh"; ","; "ZZZ"]) =
                Some
                (Node (Conversation,
                              [Node (Sentence,
                                      [Node (Grunt, [Leaf "khrgh"])]);
                              Leaf ",";
                              Node (Conversation,
                                        [Node (Snore, [Leaf "ZZZ"])])]))) ;;

let make_parser_test_1 = match make_parser giant_grammar ["khrgh"; ","; "ZZZ"] with
    | Some tree -> parse_tree_leaves tree = ["khrgh"; ","; "ZZZ"]
    | _ -> false ;;

let make_parser_test_2 = ((make_parser giant_grammar []) = None);; (*Empty frag is passed, None returned*)

let make_parser_test_3 = ((make_parser giant_grammar ["ZZZ"; "ZZZ"]) = None);; (*Nothing matched, no tree returned*)