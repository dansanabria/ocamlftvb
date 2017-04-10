(* function to check if list is empty *)
let isnil l =
  match l with
  | [] -> true
  | _ -> false

(* function to check the length of a list *)
let rec length l =
  match l with
  | [] -> 0
  | hd :: tl -> 1 + length tl

(* get rid of the header since we are not using it *)
let rec length l =
  match l with
  | [] -> 0
  | _ :: tl -> 1 + length tl

(* make it more efficient by using an accumulator - aka tail recursive function*)
let rec length_with_acc l n =
  match l with
  | [] -> n
  | hd :: tl -> length_with_acc tl (n + 1)
let length l = length_with_acc l 0

(* function returning odd elements in a list *)
let rec odd_items l =
  match l with
  | [] -> []
  | [hd] -> [hd]
  | hd :: _ :: tl -> hd :: odd_items tl

(* own imlpementation of list append *)
let rec append a b =
  match a with
  | [] -> b
  | hd :: tl -> hd :: append tl b

(* function to reverse a list *)
let rec rev (l : int list) =
  match l with
  | [] -> []
  | hd :: tl -> rev tl @  [hd]

(* take n items function *)
(* temporarly commenting out to avoid merlin warnings *)
let rec take n l =
  if n = 0 then
    []
  else
    match l with
    | hd :: tl -> hd :: take (n-1) tl

(* function to drop element e from a list *)
let rec drop e l =
  if e = 0 then
    l
  else
    match l with
    | hd :: tl -> drop (e - 1) tl

(* function to return even number on a list *)
let rec even_items l =
  match l with
  | [] -> []
  | [hd] -> []
  | hd :: e :: tl -> e :: even_items tl

(* function count_true counts the number of true elements in a list *)
let rec count_true l =
  match l with
  | [] -> 0
  | true :: tl -> 1 + count_true tl
  | false :: tl -> count_true tl

(* the above is the inefficient way, we could do the same we did with length
 * and use an accumulator *)
let rec count_true_with_acc n l =
  match l with
  | [] -> n
  | true :: tl -> count_true_with_acc (n+1) tl
  | false :: tl -> count_true_with_acc n tl
let count_true l = count_true_with_acc 0 l

(* palindrome function - we make use of rev function above *)
(* also making the function monomorphic to get used to trace *)
let palindrome (l : int list) =
  l @ rev l

(* check if it is a palindrome *)
let is_palindrome l =
  l = rev l

(* funtion to drop the last element on a list *)
let rec drop_last (l : int list) =
  match l with
  | [] -> []
  | [_] -> []
  | hd :: tl -> hd :: drop_last tl

(* function member which returns true if an element is in a list *)
let rec member m l =
  match l with
  | [] -> false
  | hd :: tl -> hd = m || member m tl

let rec rev_with_acc n l =
  match l with
  | [] -> n
  | hd :: tl -> rev_with_acc (hd :: n) tl
let rev l = rev_with_acc [] l
