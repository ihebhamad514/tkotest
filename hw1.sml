(* Homework 1 SML *)

(* Checking if the first date is older than the second *)
fun isDateOlder(d1 : int*int*int, d2 : int*int*int) =
  if (#1 d1) <> (#1 d2) 
  then (#1 d1) < (#1 d2)
  else if (#2 d1) <> (#2 d2)
  then (#2 d1) < (#2 d2)
  else (#3 d1) < (#3 d2)

(* Counting how many dates are in the given month *)
fun countInMonth(datesList : (int * int * int) list, m : int) =
  if null datesList
  then 0
  else if (#2 (hd datesList)) = m
  then 1 + countInMonth(tl datesList, m)
  else countInMonth(tl datesList, m)

(* Counting dates that fall within a list of months *)
fun countInMonths(datesList : (int * int * int) list, monthsList : int list) =
  if null monthsList
  then 0
  else countInMonth(datesList, hd monthsList) + countInMonths(datesList, tl monthsList)

(* Filtering dates that fall in a specific month *)
fun datesInSpecificMonth(datesList : (int * int * int) list, m : int) =
  if null datesList
  then []
  else if (#2 (hd datesList)) = m
  then (hd datesList) :: datesInSpecificMonth(tl datesList, m)
  else datesInSpecificMonth(tl datesList, m)

(* Filtering dates that match any month from a list *)
fun datesInGivenMonths(datesList : (int * int * int) list, monthsList : int list) =
  if null monthsList
  then []
  else datesInSpecificMonth(datesList, hd monthsList) @ datesInGivenMonths(datesList, tl monthsList)

(* Grab the nth string from a list *)
fun grabNthString(stringsList : string list, n : int) =
  if (n = 1)
  then hd stringsList
  else grabNthString(tl stringsList, n - 1)

(* Turn a date into a nice string format *)
fun prettyDate(date : int * int * int) =
  let val monthsNames = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
      val monthStr = grabNthString(monthsNames, #2 date)
  in monthStr ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

(* Figure out how many elements are needed to hit a target sum *)
fun beforeSumReached(sumTarget : int, numList: int list) =
  let fun helper(nums : int list, target : int, total : int, count : int) =
	if total + (hd nums) < target
	then helper(tl nums, target, total + (hd nums), count + 1)
	else count
  in helper(numList, sumTarget, 0, 0)
  end

(* Find the month of a given day of the year *)
fun findMonth(dayNum : int) =
  let val monthDays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in beforeSumReached(dayNum, monthDays) + 1
  end

(* Get a list of months for a range of days *)
fun getMonthRange(startDay : int, endDay : int) =
  if startDay > endDay
  then []
  else findMonth(startDay) :: getMonthRange(startDay + 1, endDay)

(* Find the oldest date from a list *)
fun oldestDate(datesList : (int * int * int) list) =
  if null datesList
  then NONE
  else let
      fun oldestInList(datesList : (int * int * int) list) =
	if null (tl datesList)
	then hd datesList
	else let val remainingOldest = oldestInList(tl datesList)
	     in
		 if isDateOlder(hd datesList, remainingOldest)
		 then hd datesList
		 else remainingOldest
	     end
  in
      SOME (oldestInList datesList)
  end