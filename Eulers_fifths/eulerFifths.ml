(* function to raise an int x to the fifth power *) 
let raise : int -> int = fun (x : int) -> x * x * x * x * x in 

(* optimization so that it can call precomputed raise values *)
let power = 
  (raise 0) :: (raise 1) :: (raise 2) :: (raise 3) :: (raise 4) ::
  (raise 5) :: (raise 6) :: (raise 7) :: (raise 8) :: (raise 9) :: []
in

let rec baseGetPower : int list -> int -> int = 
  fun (xs : int list) -> fun (idx : int) ->
  match xs with
  | [] -> -1
  | hd::tl -> begin
      if idx = 0
      then hd
      else baseGetPower tl (idx - 1)
    end
in

let getPower : int -> int =
  fun (idx : int) -> baseGetPower power idx 
in 

(* function that checks if a number is a sum of its  *)
let rec check (checkNum : int) (currNum : int) (currSum : int) =
  if currNum = 0 
  then checkNum = currSum
  else check (checkNum) (currNum / 10) ((getPower (currNum mod 10)) + currSum)
in


(* driver  *)
let rec driver : int -> int -> int =
 fun (currNum : int) -> fun (currSum : int) ->
  if currNum = 2
  then currSum
  else begin
    let fifth_equiv = check currNum currNum 0 in 
    if fifth_equiv
    then driver (currNum - 1) (currSum + currNum)
    else driver (currNum - 1) (currSum)
  end
in

(* 9^5 is 59049, using this information, we get that 
   we have an upper bound of roughly ~400,000 as 
   5 * 9^5 is 295,245 ==> 6 * 9^5 is 354,294
*)

let () = print_int (driver 400000 0) in
print_newline ()