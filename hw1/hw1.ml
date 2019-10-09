open List;;

type ('nonterminal, 'terminal) symbol = 
| N of 'nonterminal
| T of 'terminal;;

let rec is_element elt lst=
match lst with
| [] -> false
| hd::tl -> if hd = elt then true else is_element elt tl;;

let rec subset a b=
match a with
| [] -> true
| hd::tl -> (is_element hd b) && (subset tl b);;

let equal_sets a b = (subset a b) && (subset b a);;

let set_union a b = a @ b;;

let rec set_intersection a b = 
	match a with
	| [] -> []
	| hd::tl -> if is_element hd b then hd::(set_intersection tl b) else set_intersection tl b;;

let rec set_diff a b = 
	match a with 
	| [] -> []
	| hd::tl -> if not(is_element hd b) then hd::(set_diff tl b) else set_diff tl b;;

let rec computed_fixed_point eq f x =
	if eq x (f x) then x else computed_fixed_point eq f (f x);;


(*Given a symbol, we can check whether it is reachable from a single rule*)
let rec is_reachable_symbol symb rules = 
	match rules with
	| [] -> false
	| hd::tl -> match hd with |T _ -> is_reachable_symbol symb tl
							  |N a -> if a = symb then true else is_reachable_symbol symb tl;;

(*Given a symbol, we can check whether it is reachable from a list of (symbol,rules) pair*)
let rec is_reachable_from_pair symb lst = 
	match lst with
	| [] -> false
	| hd::tl -> if is_reachable_symbol symb (snd hd) then true else is_reachable_from_pair symb tl;;

(*Go thru a list of (symbol,rules) pair from a start_symbol, a pair is reachable when pair[0] is start_symbol, 
or pair[0] is reachable thru the reachable list we return, 
then append the pair to new list*)
let rec reachable_list start_symb lst reachable_lst = 
	match lst with
	| [] -> reachable_lst
	| hd::tl -> if ((fst hd) = start_symb) || is_reachable_from_pair (fst hd) reachable_lst then reachable_list start_symb tl (hd::reachable_lst)
				else reachable_list start_symb tl reachable_lst;;

(*Append more reachable rules, finally get a list of all reachable rules*)
let rec append_rules grammar = 
	let first_few_rules = reachable_list (fst grammar) (snd grammar) [] in 
	computed_fixed_point (equal_sets) (fun x -> reachable_list (fst grammar) (snd grammar) x) first_few_rules;;


(*Filter out all unreachable pairs*)
let rec filter_reachable_pairs grammar = 
	let reachable_rule_generator = append_rules grammar in 
	List.filter (fun x -> List.mem x reachable_rule_generator) (snd grammar);;

let rec filter_reachable g = (fst g, filter_reachable_pairs g);;










