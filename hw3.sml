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

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals(sl : string list) = List.filter (fn s => Char.isUpper (String.sub(s,0))) sl

fun longest_string1(sl : string list) = foldl (fn (s,x) => if String.size s > String.size x then s else x) "" sl

fun longest_string2(sl : string list) = foldl (fn (s,x) => if String.size s >= String.size x then s else x) "" sl

fun longest_string_helper f sl = f sl

val longest_string3 = longest_string_helper longest_string1

val longest_string4 = longest_string_helper longest_string2

val longest_capitalized = longest_string1 o only_capitals

fun rev_string (s : string) = String.implode (List.rev (String.explode s))

fun first_answer f xs =
	case xs of
		[] => raise NoAnswer
	  | x::xs' => case f(x) of
	  				NONE => first_answer f xs'
				  | SOME v => v	  
		

fun all_answers f xs =
	let
		fun helper (f, ys, acc) =
			case ys of
				[] => SOME acc
			  | y::ys' => case f(y) of
			  				NONE => NONE
						  | SOME a => helper(f, ys', a@acc)
	in
		helper(f, xs, [])
	end

fun count_wildcards (p : pattern) = g (fn x => 1) (fn x => 0) p

fun count_wild_and_variable_lengths (p : pattern) = g (fn x => 1) (fn x => String.size x) p

fun count_some_var (s : string, p : pattern) = g (fn x => 0) (fn x => if x = s then 1 else 0) p

fun check_pat (p : pattern) = 
	let 
		fun get_strings (px : pattern) =
			case px of
		 	  Variable x => [x]
		 	  | TupleP ps => List.foldl (fn (x,l) => l@get_strings(x)) [] ps
		 	  | ConstructorP (_,py) => get_strings(py)
			  | _ => []
		fun check_repeat (ls : string list) =
			case ls of
				[] => true
			  | l::ls' => if List.exists (fn x => x = l) ls' then false else check_repeat(ls')
	in
		check_repeat(get_strings(p))
	end

fun match ( vp : valu * pattern) =
	case vp of
		(_, Wildcard) => SOME []
	  | (Unit,UnitP) => SOME []
	  | (v, Variable(s)) => SOME [(s,v)]
	  | (Const x, ConstP y) => if x = y then SOME [] else NONE
	  | (Tuple vs, TupleP ps) => if List.length(vs) = List.length(ps) then all_answers match (ListPair.zip(vs,ps)) else NONE
	  | (Constructor(s2,v),ConstructorP(s1,p)) => if s1 = s2 andalso isSome(match(v,p)) then match(v,p) else NONE
	  | _ => NONE

fun first_match v pl =
	SOME (first_answer (fn x => match(v,x)) pl) handle NoAnswer => NONE

datatype typ = Anything (* any type of value is okay *)
				| UnitT (* type for Unit *)
				| IntT (* type for integers *)
				| TupleT of typ list (* tuple types *)
				| Datatype of string (* some named datatype *)

fun typecheck_patterns ((ls:(string*string*typ) list), pl) =
	let
		exception TypeMismatchError

		fun get_pattern(p) =
			let 
				fun get_constructor (s,p,cl) =
				case ls of
					[] => raise TypeMismatchError
				  | (s1,s2,ret)::cl' => if s1 = s andalso get_pattern(p) = ret 
											then Datatype(s2)
											else get_constructor(s,p,cl')
			in
				case p of
					Wildcard => Anything
				  | Variable _ => Anything
				  | UnitP => UnitT
				  | ConstP _ => IntT
				  | TupleP ps => TupleT (List.map get_pattern ps)
				  | ConstructorP (s,pc) => get_constructor(s,pc,ls)
			end
		
			  				
		fun compare_types (t1,t2) =
			if t1=t2
			then t1
			else case(t1,t2) of
				(Anything,_) => t2
			  | (_,Anything) => t1
			  | (TupleT tl1, TupleT tl2) =>
			  		if List.length(tl1) = List.length(tl2)
					  then TupleT(List.map compare_types (ListPair.zip(tl1,tl2)))
					  else raise TypeMismatchError

	in
		SOME (List.foldl compare_types Anything (List.map get_pattern pl)) handle TypeMismatchError => NONE
	end