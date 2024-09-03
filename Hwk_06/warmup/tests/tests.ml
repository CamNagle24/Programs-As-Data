
(* If a test fails with `dune runtests --profile release` try loading the
   tests into utop to experiment with the intermediate values.

   Run  `dune utop --profile release` 
   then `open Tests ;;`

   then `test1 () ;;` or `test2 ();;` etc.

   then, for example, `eval e04 [] ;;` 

   If this was the asssert that failed then evaluating it here in
   utop directly will observe the error. You can explore other values
   and their evaluations in debugging your code.
*)

open Expr

(* some sample expressions *)

(* arithmetic *)
let e01 = Add (Val (Int 3), Val (Int 5))
let e02 = Mul (Add (Val (Int 3), Val (Int 2)), 
               Sub (Val (Int 4), Val (Int 2)))
let e03 = Sub (Val (Int 8), Val (Int 2))
let e04 = Add (Val (Int 8), Neg (Val (Int 2)))


(* relational and logical operations *)
let e05 = Lt (Add (Val (Int 3), Val (Int 5)), Val (Int 10))

let e06 = And (Lt (Val (Int 10), Val (Int 15)),
              Not (Eq (Val (Int 10), Val (Int 8))))

let e07 = Or (Gt (Val (Int 10), Val (Int 15)),
              Not (Eq (Val (Int 10), Val (Int 8))))



(* Let expressions *)

let e08 = Let ("x", Add (Val (Int 3), Val (Int 4)),
              Add (Id "x", Val (Int 5))
           )

   
let e09 = Let ("x", Add (Val (Int 3), Val (Int 4)),
              Lt (Id "x", Val (Int 5))
           )
       
(* ``let x = 3 < 5 in x && let x = 1 + 2 in x = 3 *)
let e10 = Let ("x",
              Lt (Val (Int 3), Val (Int 5)),
              And (Id "x",
                   Let ("x",
                        Add (Val (Int 1), Val (Int 2)),
                        Eq (Id "x", Val (Int 3))
                       )
                  )
             )


(* Errors *)
(* Unbound Variables *)
let e20 = And (Id "y", Val (Bool true))


(* ``let x = 3 < 5 in y && let x = 1 + 2 in x = 3 *)
let e21 = Let ("x",
              Lt (Val (Int 3), Val (Int 5)),
              And (Id "y",
                   Let ("x",
                        Add (Val (Int 1), Val (Int 2)),
                        Eq (Id "x", Val (Int 3))
                       )
                  )
             )

(* let x = x + 4 in y && true *)
let e22 = Let ("x", Add (Id "x", Val (Int 4)),
                 And (Id "y", Val (Bool true))
              )


(* Division by Zero *)

let e23 = Let ("x", Val (Int 0), Div (Val (Int 4), Id "x"))


(* Type Errors *)
let e24 = Add (Val (Int 3), Val (Bool true))

let e25 = Let ("x", Add (Val (Int 3), Val (Int 4)),
                 And (Id "x", Val (Bool true))
              )





let test1 () =
  assert (eval e01 [] = Int 8);
  assert (eval e02 [] = Int 10);
  assert (eval e03 [] = Int 6);
  assert (eval e04 [] = Int 6);

  assert (eval e05 [] = Bool true);
  assert (eval e06 [] = Bool true);
  assert (eval e07 [] = Bool true);

  assert (eval e08 [] = Int 12);
  assert (eval e09 [] = Bool false);
  assert (eval e10 [] = Bool true)


let test2 () = 

  assert (try (fun _ -> false) (eval e20 []) with
          | UnboundVariable "y" -> true
          | _ -> false);
  assert (try (fun _ -> false) (eval e21 []) with
          | UnboundVariable "y" -> true
          | _ -> false);
  assert (try (fun _ -> false) (eval e22 []) with
          | UnboundVariable v -> (v = "x") || (v = "y")
          | _ -> false)

let test3 () =
  assert (try (fun _ -> false) (eval e23 []) with
          | DivisionByZero (Int 4) -> true
          | _ -> false)

let test4 () = 
  assert (try (fun _ -> false) (eval e24 []) with
          | IncorrectType "Add" -> true
          | _ -> false);

  assert (try (fun _ -> false) (eval e25 []) with
          | IncorrectType "And" -> true
          | _ -> false)

