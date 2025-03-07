type ('nonterminal, 'terminal) symbol =
    | N of 'nonterminal
    | T of 'terminal
    
type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal
(* returns a Homework 2-style grammar,
 which is converted from the Homework 1-style grammar gram1*)

 (*1-------------------------------------------------------------------------------*)
let convert_grammar gram1 =
  let rec convert rules nt = 
    match rules with
    | [] -> []
    | (lhs,rhs)::rest -> 
        if (lhs) = nt 
          then (rhs)::(convert rest nt) 
        else convert rest nt
  in
  (fst gram1, fun nt -> convert (snd gram1) nt)

  (*2-------------------------------------------------------------------------------*)
  let parse_tree_leaves tree =
    let rec extract_leaves = function
      | [] -> []
      | Leaf leaf :: tail -> leaf :: extract_leaves tail
      | Node (_, subtree) :: tail -> extract_leaves subtree @ extract_leaves tail
    in
    extract_leaves [tree]

(*3 make_matcher below-------------------------------------------------------------------------------*)
let make_matcher (start, rules) =
  let rec find_match rule_frag acceptor frag =
    match rule_frag with
    | [] -> None
    | head::tail ->
      match match_expr head acceptor frag with
      | Some _ as result -> result  
      | None -> find_match tail acceptor frag  
  and match_expr expr acceptor frag =
    match expr, frag with
    | [], _ -> acceptor frag
    | _, [] -> None
    | (T token)::tl, first::rest ->
      if token = first then match_expr tl acceptor rest
      else None
    | (N nonterm)::tl, _ ->
      find_match (rules nonterm) (match_expr tl acceptor) frag

  in

  fun acceptor fragment -> match_expr [N start] acceptor fragment


(*4-------------------------------------------------------------------------------*)
let handle_parse_result frag tree =
  if frag = [] then Some tree else None

let rec attempt_parse prodFunc startSymbol ruleAlternatives handle_frag frag treeChildren =
  match ruleAlternatives with
  | [] -> None 
  | (rule::otherRules) ->
    match attempt_single_rule prodFunc startSymbol rule handle_frag frag treeChildren with
    | None -> attempt_parse prodFunc startSymbol otherRules handle_frag frag treeChildren  
    | Some parsedTree -> Some parsedTree

and attempt_single_rule prodFunc startSymbol rule handle_frag frag treeChildren =
  match rule with
  | [] -> handle_frag frag (Node(startSymbol, treeChildren))  
  | (head::tail) ->
    match head with
    | (N nonterminal) ->
       let recursive_accept frag2 tree2 =
        attempt_single_rule prodFunc startSymbol tail handle_frag frag2 (treeChildren @ [tree2]) in
      attempt_parse prodFunc nonterminal (prodFunc nonterminal) recursive_accept frag []
    | (T terminal) ->
      match frag with
      | [] -> None  
      | (current::remaining) ->
        if terminal = current then
          attempt_single_rule prodFunc startSymbol tail handle_frag remaining (treeChildren @ [Leaf terminal])  
        else
          None  

let make_parser grammar =
  let startSymbol = fst grammar
  and prodFunc = snd grammar in
  let alternatives = prodFunc startSymbol in
  (fun frag -> attempt_parse prodFunc startSymbol alternatives handle_parse_result frag [])