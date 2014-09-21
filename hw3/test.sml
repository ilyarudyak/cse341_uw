(* fun map (f,xs) =
    case xs of 
	[] => []
	   | x::xs' => (f x)::(map(f,xs')) 





val x2 = map (hd, [[1,2],[3,4],[5,6,7]])
val x3 = List.map hd [[1,2],[3,4],[5,6,7]]


fun map1 f = fn xs =>
	      case xs of
		  [] => []
	       | x::xs' => (f x)::(map1 f xs')

 
val  map2 = fn f => fn xs =>
	      case xs of
		  [] => []
	       | x::xs' => (f x)::(map1 f xs')



val x4 = map2 hd [[1,2],[3,4],[5,6,7]]

fun filter (f,xs) =
    case xs of
	[] => []
     | x::xs' => if (f x)
		 then x::filter(f,xs')
		 else filter(f,xs')

val test0 = filter( (fn x => x>=1),[0,1,2,0] )
val test1 = List.filter (fn x => x>=1) [0,1,2,0] *)

(*
exception NoAnswer

(* (’a -> ’b option) -> ’a list -> ’b *)
fun first_answer f = fn xs =>
	case xs of
	    [] => raise NoAnswer
	 | x::xs' => (case (f x) of
			  SOME x => x
		       | NONE => first_answer f xs')
	 

fun all_answer f = fn xs =>
	case xs of
	    [] => SOME []
	 | x::xs' => (case (f x) of
			  SOME x => x
		       | NONE => first_answer f xs')




val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5]

 val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3]


val test8 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
*)

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

fun tuple_to_slist p =
    case p of
	TupleP ps => (case ps of
			  [] => []
		       | (Variable s)::ps' => s::tuple_to_slist (TupleP ps')
		       | _::ps' => tuple_to_slist (TupleP ps') )
     | _ => []

fun list_of_strings (p : pattern) =
	    case p of 
		Variable(s) => [s]
	      | TupleP(ps) => tuple_to_slist TupleP(ps)
	      | _ => []




val p0 = Variable "a"
val p1 = Variable "b"
val test0 = tuple_to_slist (TupleP [p0,p1,Wildcard])











