(*hw2*)
(*Problem 1*)
type ('nonterminal, 'terminal) symbol =
 | N of 'nonterminal
 | T of 'terminal

(*helper function: convert grammar for each symbol*)
 let rec convert_grammar_each_symbol sym gram ret_list = match gram with
 	| [] -> ret_list
 	| hd::tl -> if (fst hd) = sym then convert_grammar_each_symbol sym tl ((snd hd)::ret_list)
 				else convert_grammar_each_symbol sym tl ret_list;;

 let convert_grammar gram1 = (fst gram1, function x -> (convert_grammar_each_symbol x (snd gram1) []));;

(*Problem 2*)
type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(* ('a, 'b) parse_tree list -> 'b list -> 'b list *)
let rec parse_tree_helper tree ret_list = match tree with 
	| [] -> ret_list
	| hd::tl -> match hd with
		| Node(a,b) -> (parse_tree_helper b ret_list)@(parse_tree_helper tl ret_list)
		| Leaf l -> l::(parse_tree_helper tl ret_list) ;;

(* type of argument ('a,'b) parse_tree -> 'b list *)
(* we put [] to tree because we want it to be of parse_tree type instead of parse_tree list type*)
let parse_tree_leaves tree = parse_tree_helper [tree] [];;

(*Problem 3*)
let rec match_non_term start_symb gram_rules = 
	let rec make_appended_matchers gram_rules rule_list = match rule_list with
				| [] -> (fun acceptor frag -> None) (*Nothing is matched from the rule_list, then None is returned*)
				| rl_hd::rl_tl -> (fun acceptor frag -> let match_value = match_split_rl gram_rules rl_hd 
					(*Finds the next matching prefix from rule_list*)
						in match match_value acceptor frag with 
							(*Nothing is accepted in rl_hd, so matches rl_tl*)
							| None -> make_appended_matchers gram_rules rl_tl acceptor frag 
							(*matches hd with some value*)
							| Some x -> match_value acceptor frag
				)
		and 
		match_split_rl gram_rules rl_hd = 
			match rl_hd with
			| [] -> (fun acceptor -> acceptor) (*no rules to try, return what acceptor returns, i.e. identity*)
			| hd::tl -> (match hd with (*matches a single term*)
					(*terminal, try matching it with the next unmatched element in the fragment*)
					| T t -> (fun acceptor frag ->
									match frag with
										| [] -> None
															(*t matches with the next unmatched frag, so try next single term with the rest of frag*)
										| frag_hd::frag_tl -> if frag_hd = t then match_split_rl gram_rules tl acceptor frag_tl
															  else None)
					| N nt -> (fun acceptor frag ->
									match_non_term nt gram_rules (match_split_rl gram_rules tl acceptor) frag
							  )
			)
		in make_appended_matchers gram_rules (gram_rules start_symb);; (*gram_rules takes in start_symb and gives the rule_list to start with*)

let make_matcher gram = match_non_term (fst gram) (snd gram);;

(*Problem 4*)
let rec parse_non_term start_symb gram_rules acceptor derivation = 
	let rec make_appended_parsers gram_rules rule_list acceptor derivation = match rule_list with
			| [] -> (fun frag -> None)
			| rl_hd::rl_tl -> (fun frag -> let parse_value = parse_split_rl gram_rules rl_hd 
					in match parse_value acceptor derivation frag with
							| None -> make_appended_parsers gram_rules rl_tl acceptor derivation frag
							| Some x -> parse_value acceptor (derivation@[(start_symb,rl_hd)]) frag

			)
		and 
		parse_split_rl gram_rules rl_hd acceptor derivation= 
			match rl_hd with
				| [] -> (fun frag -> acceptor derivation frag)
				| hd::tl -> (match hd with
						| T t -> (fun frag ->
									match frag with
										| [] -> None
										| frag_hd::frag_tl -> if frag_hd = t then parse_split_rl gram_rules tl acceptor derivation frag_tl
															  else None)
						| N nt -> (fun frag ->
										parse_non_term nt gram_rules (parse_split_rl gram_rules tl acceptor) derivation frag
								  ) 
				)
		in make_appended_parsers gram_rules (gram_rules start_symb) acceptor derivation;;


let rec generate_parse_tree deviation = 
	(*This function takes in a visited rule_list and an unvisited (symb,rule list), 
		and returns a tuple (remaining_derivation, parse_tree) *)
	let rec parse_tree_helper visited unvisited = 
		match visited with
		| [] -> unvisited, []
		| N hd::tl -> (let get_hd = generate_parse_tree unvisited in 
							let get_tl = parse_tree_helper tl (fst get_hd)  in 
							(fst get_tl, (snd get_hd)::(snd get_tl))
						)
		| T hd::tl -> (let get_tl = parse_tree_helper tl unvisited in 
						(fst get_tl, (Leaf hd)::(snd get_tl))
						)

	in match deviation with 
		| hd::tl -> let ret = parse_tree_helper (snd hd) tl in (fst ret, Node (fst hd, snd ret));;


(*This function takes in gramar and returns a parse tree if the derivation exists, or None*)
let make_parser gram = 
  (*Check if a derivation exists*)
  let acceptor_der derivation = function
  	| [] -> Some (derivation)
  	| _ -> None in 

  (fun frag -> let list_parse = parse_non_term (fst gram) (snd gram) acceptor_der []
  		in match list_parse frag with
  			| Some x -> let tree = generate_parse_tree x in Some (snd tree)
  			| None -> None
  );;