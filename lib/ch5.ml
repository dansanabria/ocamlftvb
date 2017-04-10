open Ch4
(* examples and exercises from chapter 5  *)

(* we start with an implementation of the
 * insertionsort algorithmn *)

let rec insert (e : int) (l : int list) =
  match l with
  | [] -> [e]
  | hd :: tl ->
      if e <= hd then
        e :: hd :: tl
      else
        hd :: insert e tl
let rec sort l =
  match l with
  | [] -> []
  | hd :: tl -> insert hd (sort tl)

(* running trace on insert and sort in the toplevel
 * give us a good illustration of how these functions
 * work
utop # sort;;
- : int list -> int list = <fun>

utop # sort [3;6;1;19;15];;
sort <-- [3; 6; 1; 19; 15]
sort <-- [6; 1; 19; 15]
sort <-- [1; 19; 15]
sort <-- [19; 15]
sort <-- [15]
sort <-- []
sort --> []
insert <-- 15
insert --> <fun>
insert* <-- []
insert* --> [15]
sort --> [15]
insert <-- 19
insert --> <fun>
insert* <-- [15]
insert <-- 19
insert --> <fun>
insert* <-- []
insert* --> [19]
insert* --> [15; 19]
sort --> [15; 19]
insert <-- 1
insert --> <fun>
insert* <-- [15; 19]
insert* --> [1; 15; 19]
sort --> [1; 15; 19]
insert <-- 6
insert --> <fun>
insert* <-- [1; 15; 19]
insert <-- 6
insert --> <fun>
insert* <-- [15; 19]
insert* --> [6; 15; 19]
insert* --> [1; 6; 15; 19]
sort --> [1; 6; 15; 19]
insert <-- 3
insert --> <fun>
insert* <-- [1; 6; 15; 19]
insert <-- 3
insert --> <fun>
insert* <-- [6; 15; 19]
insert* --> [3; 6; 15; 19]
insert* --> [1; 3; 6; 15; 19]
sort --> [1; 3; 6; 15; 19]
- : int list = [1; 3; 6; 15; 19]
 *)

(* so the time insertionsort above takes to complete
* can be improved with the mergesort algo below *)

(* let's start with the merge part *)

let rec merge ( x : int list ) ( y : int list ) =
  match x, y with
  | [], l -> l
  | l , [] -> l
  | hx :: tx, hy :: ty ->
      if hx < hy then
        hx :: merge tx (hy :: ty)
      else
        hy :: merge (hx :: tx) ty

(* now the msort part *)
let rec msort l =
  match l with
  | [] -> []
  | [x] -> [x]
  | _ ->
      let left = take (length l / 2) l in
      let rigth = drop (length l / 2) l in
      merge (msort left) (msort rigth)

(* the signature and trace output give us a good illustration
 * of how this algo works *)

(*utop # msort;;
- : int list -> int list = <fun>
utop # msort [4;2;6;1];;
msort <-- [4; 2; 6; 1]
msort <-- [6; 1]
msort <-- [1]
msort --> [1]
msort <-- [6]
msort --> [6]
merge <-- [6]
merge --> <fun>
merge* <-- [1]
merge <-- [6]
merge --> <fun>
merge* <-- []
merge* --> [6]
merge* --> [1; 6]
msort --> [1; 6]
msort <-- [4; 2]
msort <-- [2]
msort --> [2]
msort <-- [4]
msort --> [4]
merge <-- [4]
merge --> <fun>
merge* <-- [2]
merge <-- [4]
merge --> <fun>
merge* <-- []
merge* --> [4]
merge* --> [2; 4]
msort --> [2; 4]
merge <-- [2; 4]
merge --> <fun>
merge* <-- [1; 6]
merge <-- [2; 4]
merge --> <fun>
merge* <-- [6]
merge <-- [4]
merge --> <fun>
merge* <-- [6]
merge <-- []
merge --> <fun>
merge* <-- [6]
merge* --> [6]
merge* --> [4; 6]
merge* --> [2; 4; 6]
merge* --> [1; 2; 4; 6]
msort --> [1; 2; 4; 6]
- : int list = [1; 2; 4; 6]
*)

(* making msort more efficient by calculating length l / 2 once *)
let rec msort l =
  match l with
  | [] -> []
  | [x] -> [x]
  | _ ->
      let half = length l / 2 in
      let left = take half l in
      let right = drop half l in
      merge (msort left) (msort right)

(* version of insertion sort that sorts in reverse order *)
let rec insert e l =
  match l with
  | [] -> [e]
  | hd :: tl ->
      if e >= hd then
        e :: hd :: tl
      else
        hd :: insert e tl
let rec sort l =
  match l with
  | [] -> []
  | hd :: tl -> insert hd ( sort tl )

(* function to detect if list is sorted
 * assuming original version of sort is in scope*)
let is_sorted l =
  l = sort l
