(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu 



(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals (xs) =
    List.filter (fn s => Char.isUpper(String.sub(s,0))) xs

(* closest to the beginning  of the list *)
fun longest_string1 (xs) = 
    List.foldl (fn (s0, s1) => if String.size(s0) <= String.size(s1) then s1 else s0) "" xs


(* closest to the end of the list *)
fun longest_string2 (xs) = 
    List.foldl (fn (s0, s1) => if String.size(s0) < String.size(s1) then s1 else s0) "" xs


val longest_capitalized  = longest_string1 o only_capitals

val rev_string = String.implode o List.rev o String.explode

(* (int * int -> bool) -> string list -> string *)
fun longest_string_helper f = 
    List.foldl ( fn (s0,s1) => if f(String.size(s0), String.size(s1)) 
			       then s0 
			       else s1 ) "" 

(* = longest_string1, beginning *)
val longest_string3 = 
    longest_string_helper (fn (x,y) => x > y)


val longest_string4 = 
    longest_string_helper (fn (x,y) => x >= y)



(* (’a -> ’b option) -> ’a list -> ’b *)
fun first_answer f = fn xs =>
	case xs of
	    [] => raise NoAnswer
	 | x::xs' => (case (f x) of
			  SOME x => x
		       | NONE => first_answer f xs')

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(* pattern -> int *)
val count_wildcards = 
    g (fn () => 1) (fn s=> 0) 


val count_wild_and_variable_lengths =
    g (fn () => 1) (fn s => String.size(s)) 

fun count_some_var (s0, p) =
    g (fn () => 0) (fn s => if s=s0 then 1 else 0) p 














