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
let rec rev l =
  match l with
  | [] -> []
  | hd :: tl -> rev tl @  hd

(* take n items function *)
let rec take n l =
  if n = 0 then
    []
  else
    match l with
    | hd :: tl -> hd :: take (n-1) tl 
