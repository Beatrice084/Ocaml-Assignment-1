(*** CSI 3120 Assignment 1 ***)
(*** Beatrice Johnston ***) 
(*** 8198979 ***)
(*** 4.05.0 ***)
(* If you use the version available from the lab machines via VCL, the
   version is 4.05.0 ***)

(* You do not have to understand the definition of the "undefined"
   function below.  Part of your job is to replace all calls to this
   function in the code below with the answers that you are asked
   for. *)

let undefined : unit -> 'a = fun () -> failwith "undefined"

(* 1. Please define these variables with the appropriate values.  Be
   sure that these statements all type-check after editing them.  You
   can do this by copying and pasting to the shell window running the
   OCaml by compiling with "ocaml" and loading into the shell window
   running...  in the terminal emulator, or by using an evaluation
   plugin installed in your editor, for example, Ctrl+c and then
   Ctrl+e in Emacs with Tuareg mode *)
                                     
(* 1a. Create a string with your first name *)
let name : string = "Beatrice"

(* 1b. Use a string operator on the string from 1.a. to create a
   string that contains both your first and last names. *)
let fullname : string = name ^ "Johnston"

(* 1c. Create a string containing your email address *)
let email : string = "bjohn084@uottawa.ca"

(* 1d. Replace (Other "...") in class_year with the appropriate item
   below *)
(* ie: replace (Other "...") with SecondYear or ThirdYear for example *)
type year = FirstYear | SecondYear | ThirdYear | FourthYear | Other of string

let class_year : year = ThirdYear

(* 1e. Replace the .... with one thing you hope to learn about in
   this course *)
let learning : string = "I hope to learn more about OCaml!"

let print = Printf.printf

let print_survey () = 
  let string_year = 
    (match class_year with
       | FirstYear -> "2022"
       | SecondYear -> "2021"
       | ThirdYear -> "2020"
       | FourthYear -> "2019"
       | Other s -> "Other: " ^ s
    ) in
    (print "----------------------------------------\n";
     print "Name: %s\n" name;
     print "Email: %s\n" email;
     print "Year: %s\n" string_year; 
     print "%s\n" learning;
     print "----------------------------------------\n\n")

(* Problem 2 - Fill in types: *)
(* Replace each ??? with the appropriate type of the corresponding
   expression.  Be sure to remove the comments from each subproblem
   and to type check it before submission.  *)
(* Note that the expressions might not do anything useful -- and in
   fact might even display interesting problems! -- but all you should
   do is fill in the ???s to make them type check. *)

(* Problem 2a. *)
let prob2a : string  = let greet y = "Hello " ^ y in greet "World!" 

(* Problem 2b. *)
let prob2b : float  = float_of_int( int_of_float(2.2 +. 7.7)) 

(* Problem 2c. *)
let rec prob2c (x : char) : char  =
  prob2c ( if true then prob2c x else 'h')

(* Problem 2d. *)
let rec prob2d (y: bool) (z: bool) : bool  =
   prob2d (prob2d z y) (not y)

(* Problem 3 - Explain why each of 3a, 3b, 3c will not compile (use
   the strings exp3a, exp3b, and exp3c for your answers) and change
   the code in some small way so that it does, and leave prob3a,
   prob3b, and prob3c uncommented. Do not change the top-level type
   associated with the expression. *)

(* Problem 3a. *)
let exp3a : string = "Need to compare the  same types to use less then operator"

let prob3a (x : float) (y : float): bool = 
  let compare x y = x < y in 
  compare 3.9 4.0

(* Problem 3b. *)
let exp3b : string = "Need to put arugments for aux in brackets when the the argements expressions"

let prob3b : int = 
  let fib n  =
   let rec aux n y x =
    if n <= 0 then x 
    else aux (n-1) (x+y) y 
   in
   aux n 1 0
  in
  fib 10

(* Problem 3c. *)
let exp3c : string = "Need to add rec when doing recursion"

let prob3c : int =
  let rec sumTo (n:int) : int =
  if n <= 0 then 0
  else n + sumTo (n-1)
  in
  sumTo 10 

(* Problem 4 *)
(* Replace each ?? with the type of the corresponding expression, and
   write a function f that has the correct type signature. Explain in
   exp4 a problem that remains with the function prob4 *)

let f (a: int) (b: int) : float  =
  (float_of_int a) +. (float_of_int b)

let rec prob4 (x: float) (y: int) : int =
  prob4 (f y 4) (int_of_float x)

let exp4 : string = "prob4 doesn't have a stop condition so it goes on recursively forever. Also, not possible to logically determine result type of prob4 (put int in so it would compile)"

(* Problem 5 *)

(* You do not have to understand the following exception definition
   but you will use it below. *)

(* Write the function count_divisors, which takes one parameters n and
   returns the number of divisors that n has (including 1 and n): *)

(* let _ = count_divisors 17 (* 2 -- 17 divides only 1 and 17 *)
let _ = count_divisors 4  (* 3 -- 4 divides 1, 4, and 2 *)
let _ = count_divisors 18 (* 6 -- 18 divides 1, 18, 2, 3, 6, and 9 *) *)
                             
(* The type signature for count_divisors is: *)
(*   count_divisors : int -> int *)

(* count_divisors should call the function (bad_divisors n) defined
   above if n <= 0  *)

(* You can write as many "helper" functions as you need using
   "let..in" or just "let". *)

(* After writing count_divisors above, uncomment the following lines
   to test your code.  (Note: your code is not necessarily completely
   correct just because it passes these 3 tests. *)

(* let _ = assert (count_divisors 17 = 2)
let _ = assert (not (count_divisors 4 = 2))
let _ = assert (count_divisors 18 = 6) *)

exception BadDivisors of int
let bad_divisors n = raise (BadDivisors n)

let rec count_divisors_helper (n : int) (m: int) (count: int) : int =
  if m == 0 then count
  else
    let rec check (x: int) (y: int): bool = (x mod y) == 0 in
      if check (n) (m) then count_divisors_helper (n) (m-1) (count + 1)
      else count_divisors_helper (n) (m-1) (count)

let rec count_divisors (n: int) : int = 
  if n <= 0 then bad_divisors n
  else count_divisors_helper (n) (n) 0;;

(* print_int (count_divisors (18));; *)
