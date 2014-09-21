(*
fun remove_dupl (xs : int list) =
    if null xs then []
    else let fun isIn (xs : int list, x : int) =
                 if null xs then false
                 else if x = hd xs then true
                 else isIn(tl xs, x)
	 in  let val tmp = remove_dupl(tl xs) 
             in  if isIn(tl xs, hd xs) then tmp
		 else (hd xs) :: tmp
             end
	 end

*)

fun isLeapYear (x : int) =
    if x <= 0 then false
    else if x mod 400 = 0 then true
    else if x mod 4 = 0 andalso x mod 100 <> 0 then true
    else false

fun getElement (xs : int list, index : int) =
    if index = 1 then hd xs
    else getElement(tl xs, index-1)

fun reasonable_date (date : int*int*int) =
    let 
	val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	val year = #1 date
	val month = #2 date
	val day = #3 date
	
    in
	if year <= 0 then false
	else if month < 1 orelse month > 12 then false
	else if day < 1 then false
	else if month <> 2 andalso day <= getElement(days, month) then true
	else if month = 2 andalso isLeapYear(year) andalso day <= 29 then true
	else if month = 2 andalso day <= 28 then true
	else false
    end


val test1 = reasonable_date((2013, 2, 1)) = true
val test2 = reasonable_date((2013, 2, 28)) = true
val test3 = reasonable_date((2013, 2, 29)) = false
val test4 = reasonable_date((2013, 2, 30)) = false
val test5 = reasonable_date((2004, 2, 29)) = true











 

