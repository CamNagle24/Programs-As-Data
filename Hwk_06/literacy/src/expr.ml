(* Hwk 06 Literacy. 

   Extend the functions below to match all of the constructs
   in the type `expr`.

   Some of the parts to be filled in can be found in the 
   functions we developed in class in the `Expressions`
   directory.  If you use work from class please add a note
   to identify the work that is not originally yours.
 *)


type value 
  = Int of int
  | Bool of bool
  | Closure of string * expr * environment

and environment = (string * value) list
                               
and expr 
  = Val of value

  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Neg of expr 

  | Lt  of expr * expr
  | Gt  of expr * expr
  | Eq  of expr * expr
  | And of expr * expr
  | Or  of expr * expr
  | Not of expr

  | Let of string * expr * expr
  | Id  of string

  | Lam of string * expr
  | App of expr * expr

(* Part 1: free variables *)

let rec freevars (e: expr) : string list =
  match e with
  | Val _ -> [ ] 
  | Id s -> [s]
  | Add (e1, e2) -> freevars e1 @ freevars e2
  | Sub (e1, e2) -> freevars e1 @ freevars e2
  | Mul (e1, e2) -> freevars e1 @ freevars e2
  | Div (e1, e2) -> freevars e1 @ freevars e2
  | Neg (e1) -> freevars e1
  | Lt (e1, e2) -> freevars e1 @ freevars e2
  | Gt (e1, e2) -> freevars e1 @ freevars e2
  | Eq (e1, e2) -> freevars e1 @ freevars e2
  | And (e1, e2) -> freevars e1 @ freevars e2
  | Or (e1, e2) -> freevars e1 @ freevars e2
  | Not e1 -> freevars e1
  | Let (s, e1, e2) -> 
     freevars e1 @
       (List.filter (fun s' -> s' <> s) (freevars e2))
  | Lam (s1, e1) ->
    List.filter (fun s -> s <> s1) (freevars e1)
  | App (e1, e2) ->
    freevars e1 @ freevars e2


(* Part 2: evaluation *)
exception DivisionByZero of value
exception UnboundVariable of string
exception IncorrectType of string


let rec eval (e:expr) (env: environment) : value =
  match e with
  | Val v -> v
  | Add (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 + v2)
       | _ -> raise (IncorrectType "Add")
     )
  | Sub (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 - v2)
       | _ -> raise (IncorrectType "Sub")
     )
  | Mul (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> Int (v1 * v2)
       | _ -> raise (IncorrectType "Mul")
     )
  | Div (e1, e2) ->
     ( match eval e1 env, eval e2 env with
       | Int v1, Int v2 -> 
        if v2 = 0 then raise (DivisionByZero (eval e1 env))
        else Int (v1 / v2)
       | _ -> raise (IncorrectType "Div")
     )
  | Neg (e1) ->
     ( match eval e1 env with
       | Int v1 -> Int (-v1)
       | _ -> raise (IncorrectType "Neg")
     )

  | Lt (e1, e2) ->
        (match eval e1 env, eval e2 env with
        | Int v1, Int v2 -> Bool (v1 < v2)
        | _ -> raise (IncorrectType "Lt")
        )
  | Gt (e1, e2) ->
        (match eval e1 env, eval e2 env with
        | (Int v1, Int v2) -> Bool (v1 > v2)
        | _ -> raise (IncorrectType "Gt")
        )
  | Eq (e1, e2) ->
        (match eval e1 env, eval e2 env with
        | (Int i1, Int i2) -> Bool (i1 = i2)
        | (Bool b1, Bool b2) -> Bool (b1 = b2)
        | _ -> raise (IncorrectType "Eq")
        )
  | And (e1, e2) ->
        (match eval e1 env, eval e2 env with
        | (Bool b1, Bool b2) -> Bool (b1 && b2)
        | _ -> raise (IncorrectType "And")
        )
  | Or (e1, e2) ->
        (match eval e1 env, eval e2 env with
        | (Bool b1, Bool b2) -> Bool (b1 || b2)
        | _ -> raise (IncorrectType "Or")
        )
  | Not (e1) ->
        (match (eval e1 env) with
        | (Bool b1) -> Bool (not b1)
        | _ -> raise (IncorrectType "Not")
        )
  | Let (s1, e1, e2) ->
        let v = eval e1 env in
        let new_env = (s1, v) :: env in
        eval e2 new_env
  | Id (s1) ->
    (match List.assoc_opt s1 env with
    | Some v -> v
    | None -> raise (UnboundVariable s1))
  | Lam (s1, e1) ->
    Closure (s1, e1, env)
  | App (e1, e2) ->
        (match eval e1 env with
        | Closure (s1, e', env1) ->
          let v2 = eval e2 env in
          let env1' = (s1, v2) :: env1 in
          eval e' env1'
        | _ -> raise (IncorrectType "App")
        )
