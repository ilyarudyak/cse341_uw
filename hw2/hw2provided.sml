(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2 

(* put your solutions for problem 1 here *)
(* part a) *)
fun same_string (s1 : string, s2 : string) =
    s1 = s2 

(* fun all_except_option (s, []) = NONE
  | all_except_option (s, x::xs) = 
    let fun  f (s, []) = []
	   | f (s, x::xs) = if same_string(s, x) then xs
			  else x::f(s, xs)
    in
	if same_string(s, x) then SOME xs
	else SOME (x::f(s, xs)) 
    end *)
fun all_except_option e =
    case e of
	(s, []) => NONE
      | (s, x::xs) => if same_string(s, x) then SOME xs
		      else case all_except_option(s, xs) of
			       NONE => NONE
			     | SOME xs' => SOME (x::xs')

(* part b) *)
(* fun get_substitutions1 ([], s) = []
  | get_substitutions1 (xs::xss, s) =
    case all_except_option (s, xs) of 
	NONE => get_substitutions1 (xss, s) 
      | SOME xs' => if xs' <> xs 
		    then xs' @ get_substitutions1 (xss, s)
		    else get_substitutions1 (xss, s) *)
fun get_substitutions1 e =
    case e of
	([], s) => []
      | (xs::xss, s) =>
	let val tmp = get_substitutions1 (xss, s)
	in
	case all_except_option (s, xs) of 
	     NONE => tmp
	   | SOME xs' =>  xs' @ tmp
	end

fun get_substitutions2 e =
    get_substitutions1 e

(* part d) *)
(* string list * record -> record list with case*)
fun dhelper (xs : string list, 
	fullname : {first:string,middle:string,last:string}) =
    case xs of
	[] => []
      | x::xs => case fullname of 
		{first=s1,middle=s2,last=s3} =>
		{first=x,middle=s2,last=s3}::dhelper(xs, fullname)

fun similar_names (xss : string list list, 
	fullname : {first:string, middle:string, last:string}) =
   
    case fullname of
	{first=s1,middle=s2,last=s3} =>
	let val xs = get_substitutions1(xss,s1)
	in fullname::dhelper(xs,fullname)
	end
   


(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

(* -------------a)----------------------------------------- *)
(* card -> color *)
(* takes a card and returns its color *)
fun card_color (c) = 
    case c of
	(Spades, _) => Black
      | (Clubs, _) => Black
      |  _ => Red
      
(* -------------b)----------------------------------------- *)
(* card -> int*)
(* takes a card and returns its value *)
fun card_value (c) =
    case c of
	(_,Ace) => 11
      | (_,Num x) => x
      | _ => 10

(* -------------c)----------------------------------------- *)
(* card list * card * exn -> card list*)
(* removes card from cs if any, else raise exception e *)
fun remove_card (cs, c, e) =
    case cs of
	[] => raise e
     | (c'::cs') => if c'=c then cs' else c'::remove_card(cs',c,e)

(* -------------d)----------------------------------------- *)
fun all_same [] = true
  | all_same [c] = true
  | all_same (c::c'::cs) = if c=c' then all_same(c'::cs)
			   else false

(* takes a list of cards and returns true 
if all the cards in the list are the same color *)
(* card list -> bool*)
fun all_same_color (cs) =
    let fun to_color (cs) =
	    map card_color(cs)
    in all_same(to_color (cs))
    end

(* -------------e)----------------------------------------- *)
(* card list -> int *)
(* takes a list of cards and returns the sum of their values *)
(* fun sum_cards (cs) = 
    let fun sum_list [] = 0
	  | sum_list (x::xs) = x + sum_list(xs)
    in
	sum_list (map card_value(cs))
    end *)

fun sum_cards cs = 
    let fun f (cs,acc) =
	    case cs of
		[] => acc
	      | c::cs' => f(cs', c+acc)
    in f(map card_value(cs), 0)
    end 

    
(* -------------f)----------------------------------------- *)
(* card list * int -> int *)
(* computes the score as described above *)
fun score (hs, goal) = 
    let fun prelim_score (sum, goal) =
	    if sum > goal 
	    then 3 * (sum - goal)
	    else goal - sum
    in let val p_score = prelim_score(sum_cards(hs), goal)
       in if all_same_color (hs) then p_score div 2 else p_score
       end 
    end

(* -------------g)----------------------------------------- *)	
(* runs a game *)
(* card list * move list * int -> int *)
fun officiate (cs, ms, goal) = 

   let fun move (cs, ms, hs) = 

	    case ms of

		 (* The game ends if there are no more moves. *)
		   [] => score (hs, goal)
		| m'::ms' => 

		  case m' of
		      Discard c => move(cs, ms', remove_card(hs, c, IllegalMove))
		   |  Draw =>

		  (* If the player draws and the card-list 
                     is empty (ie he tries to draw from empty 
                     card-list), the game is over.*)
		  case cs of
		      [] => score (hs, goal) 

		 (* if drawing causes the sum of the held-cards 
                    to exceed the goal, the game is over 
                    (after drawing) *)
		    | c'::cs' =>  if sum_cards (c'::hs) > goal 
			          then score (c'::hs, goal)
				  
                                  (* play continues with a larger held-cards 
                                  and a smaller card-list. *)
				  else move (cs', ms', c'::hs) 

		    (* If the player discards some card c, 
                       play continues  with the held-cards 
                       not having c and the card-list unchanged.*)
		 

    (* The game starts with the held-cards being the empty list. *)
    in move (cs, ms, [])
    end







