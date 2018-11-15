fun is_older(x : (int*int*int), y : (int*int*int)) =
    if #1 x = #1 y
    then if #2 x = #2 y
        then if #3 x = #3 y
            then false
            else if #3 x < #3 y
                then true
                else false
        else if #2 x < #2 y
            then true
            else false
    else if #1 x < #1 y
        then true
        else false

fun number_in_month(xs : (int * int * int) list, y : int) =
    if null xs
    then 0
    else if #2 (hd xs) = y
        then 1 + number_in_month(tl xs, y)
        else number_in_month(tl xs, y)
   
fun number_in_months(xs : (int * int * int) list, ys : int list) =
    if null ys
    then 0
    else number_in_month(xs, (hd ys)) + number_in_months(xs, (tl ys))

fun dates_in_month(xs : (int * int * int) list, y : int) =
    if null xs
    then []
    else if #2 (hd xs) = y
        then (hd xs) :: dates_in_month(tl xs, y)
        else dates_in_month(tl xs,y)

fun dates_in_months(xs : (int * int * int) list, ys : int list) =
    if null ys
    then []
    else dates_in_month(xs, hd ys) @ dates_in_months(xs, tl ys)

fun get_nth(ls : string list, n : int) =
    if n = 1
    then hd ls
    else get_nth(tl ls, n - 1)

fun date_to_string(xs : (int * int * int)) =
    let
        val mounts = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
    in
        get_nth(mounts, #2 xs) ^ " " ^ Int.toString(#3 xs) ^ ", " ^ Int.toString(#1 xs)
    end

fun number_before_reaching_sum(sum : int, xs : int list) =
    let
        val count = 0
    in
        if null xs
        then 0
        else if sum <= (hd xs)
            then 0
            else 1 + number_before_reaching_sum(sum - (hd xs), tl xs)
    end

fun what_month(x : int) =
    let 
        val mounts = [31,28,31,30,31,30,31,31,30,31,30,31]
    in 
        number_before_reaching_sum(x, mounts) + 1
    end

fun month_range(day1 : int, day2 : int) =
    if day1 <= day2
    then what_month(day1)::month_range(day1+1,day2)
    else []

fun oldest(xs : (int * int * int) list) =
    if null xs
    then NONE
    else
        let val tl_ans = oldest(tl xs)
        in if isSome tl_ans andalso is_older(valOf tl_ans, hd xs)
            then tl_ans
            else SOME(hd xs)
        end