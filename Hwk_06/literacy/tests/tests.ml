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

(* Expressions *)
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


(* Functions *)
let e11 = Let ("inc", Lam ("n", Add (Id "n", Val (Int 1))),
               App (Id "inc", Val (Int 2)))

let e12 = Lam ("n", Add (Id "n", Val (Int 1)))

let e13 = Let ("add", 
               Lam ("x", Lam ("y", Add (Id "x", Id "y"))),
               App (App (Id "add", Val (Int 5)), Val (Int 3))
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


(* Unbound Variables *)
let e26 = Lam ("y", Add (Id "x", Id "y"))

(* Type Errors *)
let e27 = Let ("inc", Val (Int 1),
               App (Id "inc", Val (Int 2)))



let expr_1 =
  MultiLet ([], Add (Val (Int 1), Val (Int 2)))

let expr_2 =
  MultiLet ([("x", Val (Int 1));
             ("y", Val (Int 2))],
            Add (Id "x", Id "y"))

let expr_3 =
  MultiLet ([("m", Val (Int 1));
             ("n", Add (Id "m", Val (Int 2)))
             ],
            Add (Id "m", Id "n"))

let test1 () = 
  assert (eval expr_1 [] = Int 3);
  assert (eval expr_2 [] = Int 3);
  assert (eval expr_3 [] = Int 4)

let test2 () =
  assert (List.sort compare (freevars e01) = []);
  assert (List.sort compare (freevars e08) = []);
  assert (List.sort compare (freevars e20) = ["y"]);
  assert (List.sort compare (freevars e21) = ["y"]);
  assert (List.sort compare (freevars e22) = ["x"; "y"]);


  assert (List.sort compare (freevars e11) = []);
  assert (List.sort compare (freevars e12) = []);
  assert (List.sort compare (freevars e13) = []);
  assert (List.sort compare (freevars e26) = ["x"])



let test3 () =
  assert (eval e01 [] = Int 8);
  assert (eval e02 [] = Int 10);
  assert (eval e03 [] = Int 6);
  assert (eval e04 [] = Int 6);

  assert (eval e05 [] = Bool true);
  assert (eval e06 [] = Bool true);
  assert (eval e07 [] = Bool true);

  assert (eval e08 [] = Int 12);
  assert (eval e09 [] = Bool false);
  assert (eval e10 [] = Bool true);


  assert (eval e11 [] = Int 3);
  assert (eval e12 [] = Closure ("n", Add (Id "n", Val (Int 1)), []));
  assert (eval e13 [] = Int 8);

  assert (eval e26 [("x", Int 3)] = 
            Closure ("y", Add (Id "x", Id "y"), [("x", Int 3)]))


let test4 () = 

  assert (try (fun _ -> false) (eval e20 []) with
          | UnboundVariable "y" -> true
          | _ -> false);
  assert (try (fun _ -> false) (eval e21 []) with
          | UnboundVariable "y" -> true
          | _ -> false);
  assert (try (fun _ -> false) (eval e22 []) with
          | UnboundVariable v -> (v = "x") || (v = "y")
          | _ -> false);


  assert (try (fun _ -> false) (eval e26 []) with
          | UnboundVariable "x" -> true
          | _ -> false)
       (* This happens since we need to create a closure envirnoment
          that include "x" and its value - but "x" is not declared. *)


let test5 () =
  assert (try (fun _ -> false) (eval e23 []) with
          | DivisionByZero (Int 4) -> true
          | _ -> false)

let test6 () = 
  assert (try (fun _ -> false) (eval e24 []) with
          | IncorrectType "Add" -> true
          | _ -> false);

  assert (try (fun _ -> false) (eval e25 []) with
          | IncorrectType "And" -> true
          | _ -> false);


  assert (try (fun _ -> false) (eval e27 []) with
          | IncorrectType "App" -> true
          | _ -> false)

