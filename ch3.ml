(* using if statements instead of match *)
let rec factorial a =
  if a = 1 then 1 else a * factorial (a - 1)

(* using match statments *)
let rec factorial a =
  match a with
  | 1 -> 1
  | _ -> a * factorial (a - 1)

let isvowel c =
  match c with
  | 'a' | 'e' | 'i' | 'o' | 'u' -> true
  | _ -> false

let rec gcd a b =
  match b with
  | 0 -> a
  | _ -> gcd b (a mod b)

(* returns the sum of all the integers
  from 1 to n *)
let rec sumallint n =
  match n with
  | 1 -> 1
  | _ -> n + sumallint (n - 1)

(* given two numbers x and n, compute x^n *)
let rec tothepower x n =
  match n with
  | 0 -> 1
  | 1 -> x
  | _ -> x * tothepower x (n - 1)

(* check if chars are lowercase *)
let islower c =
  match c with
  'a'..'z' -> true
  | _ -> false
