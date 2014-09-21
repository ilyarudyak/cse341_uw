fun same_string (s1 : string, s2 : string) =
    s1 = s2 

fun all_except_option e =
    case e of
	(s, []) => NONE
      | (s, x::xs) => if same_string(s, x) then SOME xs
		      else case all_except_option(s, xs) of
			       NONE => NONE
			     | SOME xs' => SOME (x::xs')


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


fun rev1 xs =
    case xs of
	[] => []
      | x::xs' => rev1(xs') @ [x]


fun rev2 xs =
    let fun f (xs, acc)=
	case xs of
	    [] => acc
	  | x::xs'  => f(xs', x::acc) 

    in f(xs, [])
    end


 
val test = rev2([1,2,3])

