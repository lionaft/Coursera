(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(s : string, sl : string list) =
    case sl of
        [] => NONE
      | x::sl' =>  if same_string(s,x)
                    then case all_except_option(s, sl') of 
                            NONE => SOME [] 
                          | SOME y => SOME y
                    else case all_except_option(s, sl') of
                            NONE => NONE
                          | SOME y => SOME (x::y)


fun get_substitutions1(sll : string list list, s : string) =
    case sll of
        [] => []
        | sl::sll' => let val case_all_except_option = case all_except_option(s, sl) of NONE => [] | SOME y => y
                    in
                        if case_all_except_option = sl
                            then get_substitutions1(sll',s)
                            else case_all_except_option@get_substitutions1(sll',s)
                    end

fun get_substitutions2(sll : string list list, s : string) =
    let fun aux(slla, acc) =
            case slla of
                [] => acc
              | sl::slla' => let val case_all_except_option = case all_except_option(s, sl) of NONE => [] | SOME y => y
                                in
                                    if case_all_except_option = sl
                                        then aux(slla',acc)
                                        else aux(slla',acc@case_all_except_option)
                                end
    in
        aux(sll,[])
    end

fun similar_names(sll : string list list, fullname : {first:string, last:string, middle:string}) =
    let fun get_fn(sl: string list) =
        case sl of
            [] => []
          | s::sl' => case fullname of 
                        {first=a, last=b, middle=c} => {first=s, last=b, middle=c}::get_fn(sl')
    in
        case fullname of {first=a, last=b, middle=c} => fullname::get_fn(get_substitutions1(sll,a))
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

fun card_color(c : card) =
    case c of 
        (Clubs,_) => Black
      | (Spades,_) => Black
      | _ => Red

fun card_value(c : card) =
    case c of
        (_, Num x) => x
      | (_, Ace) => 11
      | _ => 10

fun remove_card(cs : card list, c : card, e) =
    case cs of
        [] => raise e
      | x::cs' => if x = c
                    then cs'
                    else x::remove_card(cs', c, e)

fun all_same_color(cs : card list) =
    case cs of
        [] => true
      | x::xs' => case xs' of
                    [] => true
                  | y::ys' => card_color(x) = card_color(y) andalso all_same_color(xs')

fun sum_cards(cs : card list) =
    let fun aux(css : card list, acc : int) =
            case css of
                [] => acc
              | x::css' => aux(css', acc + card_value(x))
    in
        aux(cs,0)
    end

fun score(cs : card list, goal : int) =
    let 
        val sum_c = sum_cards(cs)
        val pscore = if sum_c >= goal
                        then 3*(sum_c-goal)
                        else goal-sum_c
    in
        if all_same_color(cs)
            then pscore div 2
            else pscore
    end

fun officiate(cs : card list, ml : move list, goal : int) =
    let fun aux(c_cs : card list, c_ch : card list, c_ml : move list) =
            case c_ml of
                [] => score(c_ch, goal)
            | (Discard c)::c_ml' => aux(c_cs, remove_card(c_ch, c, IllegalMove), c_ml')
            | Draw::c_ml' => case c_cs of
                                    [] => score(c_ch,goal)
                                | c::c_cs' => if sum_cards(c_ch) > goal
                                                    then score(c_ch,goal)
                                                    else aux(c_cs', c::c_ch, c_ml')
    in
        aux(cs, [], ml)
    end

fun sums(cs : card list) =
    let 
        fun sum_list(sl : int list, s : int) =
                case sl of
                    [] => []
                  | x::sl' => (x+s)::sum_list(sl',s)
        fun aux(css : card list, acc : int list) =
                case css of
                    [] => acc
                  | c::css' => case c of
                                    (_, Num x) => aux(css', sum_list(acc, x))
                                  | (_, Ace) => aux(css', sum_list(acc, 1)@sum_list(acc,11))
                                  | _ => aux(css', sum_list(acc, 10)) 
    in
        aux(cs,[0])
    end

fun score_challenge(cs : card list, goal : int) =
    let val same_color = all_same_color(cs)
        fun calc_score(csum : int) =
            let 
                val pscore = if csum >= goal
                                then 3*(csum-goal)
                                else goal-csum
            in
                if same_color
                  then pscore div 2
                  else pscore
            end
        fun minsums(scores : int list, acc : int) =
            case scores of
                [] => acc
                | x::xs => let 
                            val fscore = calc_score(x)
                           in
                            if fscore < acc then minsums(xs,fscore) else minsums(xs,acc)
                           end

        val csums = sums(cs)
    in
        case csums of
            [0] => calc_score(0)
          | x::xs => minsums(xs, calc_score(x))  
    end

fun officiate_challenge(cs : card list, ml : move list, goal : int) =
    let 
        fun exceed_sum(cs : card list) =
            let 
                fun exceed_aux(c_sums : int list) =
                    case c_sums of
                        [] => true
                      | x::xs => x > goal andalso exceed_aux(xs)  
            in
                exceed_aux(sums(cs))
            end
        fun aux(c_cs : card list, c_ch : card list, c_ml : move list) =
            case c_ml of
                [] => score_challenge(c_ch, goal)
            | (Discard c)::c_ml' => aux(c_cs, remove_card(c_ch, c, IllegalMove), c_ml')
            | Draw::c_ml' => case c_cs of
                                    [] => score_challenge(c_ch,goal)
                                | c::c_cs' => if exceed_sum(c_ch)
                                                    then score_challenge(c_ch,goal)
                                                    else aux(c_cs', c::c_ch, c_ml')
    in
        aux(cs, [], ml)
    end

fun careful_player(cs : card list, goal : int) =
    let
        fun aux(css : card list, c_ch : card list, acc : move list) =
            if score(c_ch,goal) = 0
                then acc
                else case css of
                        [] => acc@[Draw]
                      | c1::c2::css' => if sum_cards(c1::c_ch) <= goal andalso sum_cards(c2::c_ch) <= goal andalso score(c2::c_ch, goal) = 0
                                        then acc@[Draw]@[(Discard c1)]@[Draw]
                                        else if sum_cards(c1::c_ch) > goal
                                                then acc
                                                else aux((c2::css'), c1::c_ch, acc@[Draw])
                      | c::css' => if sum_cards(c::c_ch) > goal
                                    then acc
                                    else aux(css', c::c_ch, acc@[Draw])
    in
        aux(cs,[],[])
    end


