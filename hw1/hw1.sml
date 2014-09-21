fun is_older (d1 : int*int*int, d2 : int*int*int) =
    if (#1 d1) < (#1 d2) then true
    else if (#1 d1) = (#1 d2) andalso (#2 d1) < (#2 d2) then true
    else if (#1 d1) = (#1 d2) andalso (#2 d1) = (#2 d2) andalso (#3 d1) < (#3 d2) then true
    else false



(* 2)--------------------------------------------------------------- *)

fun number_in_month (xs : (int*int*int) list, m : int) =
    if null xs then 0
    else if (#2 (hd xs) = m) then 1 + number_in_month(tl xs, m)
    else number_in_month(tl xs, m)

(* 3)--------------------------------------------------------------- *)

fun number_in_months (xs : (int*int*int) list, m : int list) = 
    if null m then 0
    else number_in_month(xs, hd m) + number_in_months(xs, tl m)

(* 4)--------------------------------------------------------------- *)

fun dates_in_month (xs : (int*int*int) list, m : int) =
    if null xs then []
    else if #2 (hd xs) = m then (hd xs) :: dates_in_month(tl xs, m)
    else dates_in_month(tl xs, m)

(* 5)--------------------------------------------------------------- *)

fun dates_in_months (xs : (int*int*int) list, m : int list) =
    if null m then []
    else dates_in_month(xs, hd m) @ dates_in_months(xs, tl m)

(* 6)--------------------------------------------------------------- *)


fun get_nth (xs : string list, x : int) =
    if x = 1 then hd xs
    else get_nth(tl xs, x-1)

(* 7)--------------------------------------------------------------- *)

fun date_to_string (x : int*int*int) =
    let
        val xs = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth (xs, #2 x) ^ " " ^ Int.toString(#3 x) ^ ", " ^ Int.toString(#1 x)
    end

(* 8)--------------------------------------------------------------- *)

fun number_before_reaching_sum (sum : int, xs : int list) =
    if null xs then 0
    else if hd xs >= sum then 0
    else 1 + number_before_reaching_sum(sum - (hd xs), tl xs)


(* 9)--------------------------------------------------------------- *)

fun what_month (day : int) =
    let
       val xs = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
       number_before_reaching_sum(day, xs) + 1
    end

(* 10)--------------------------------------------------------------- *)

fun month_range (day1 : int, day2 : int) =
    if day1 > day2 then []
    else if day1 = day2 then [what_month(day1)]
    else what_month(day1) :: month_range(day1+1, day2)

(* 11)--------------------------------------------------------------- *)

fun oldest (xs : (int*int*int) list) =
    if null xs then NONE
    else let val old = oldest(tl xs)
         in if isSome old andalso is_older(valOf(old), hd xs) then old
            else SOME (hd xs)
         end



(* 12)--------------------------------------------------------------- *)


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

(* 13)--------------------------------------------------------------- *)

fun number_in_months_challenge (xs : (int*int*int) list, m : int list) =
    let 
	val tmp = remove_dupl(m) 
    in 	
	if null tmp then 0
	else number_in_month(xs, hd tmp) + number_in_months_challenge(xs, tl tmp)
    end

(* 4)--------------------------------------------------------------- *)

fun dates_in_months_challenge (xs : (int*int*int) list, m : int list) =
    let 
	val tmp = remove_dupl(m) 
    in  
	if null tmp then []
	else dates_in_month(xs, hd tmp) @ dates_in_months_challenge(xs, tl tmp)
    end

(* 4)--------------------------------------------------------------- *)

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


(* 4)--------------------------------------------------------------- *)
    





















    

