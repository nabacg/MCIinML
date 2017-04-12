type id = string

datatype binop = Plus | Minus | Times | Div


datatype stm =
         CompoundStm of stm * stm
         | AssignStm of id * exp
         | PrintStm  of exp list
     and exp = IdExp of id
             | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp
             | ExpList of explist
     and explist =  PairExpList of exp * explist
                  | LastExpList of exp


fun extractstm(EseqExp (s, e))  = [s]
  | extractstm _  = []

fun flatmap(f, init,  xs) = foldl op@ init (map f xs)

fun maxint(x: int, y: int) = if x > y then x else y

fun maxargs (PrintStm exps) =  maxint(List.length exps,
                                     foldl maxint 0
                                           (map maxargs
                                                (flatmap(extractstm,[], exps))))
  | maxargs (AssignStm (id, EseqExp(s, e))) = maxargs(s)
  | maxargs (AssignStm (id, _)) = 0
  | maxargs (CompoundStm (s1, s2)) = maxint(maxargs(s1), maxargs(s2))



type table = (id*int) list

fun lookup ((sym:id, v:int)::es, s:id) = if s = sym then v else lookup(es, s)
  | lookup (nil, s:id)  = raise Fail ("Symbol " ^ s ^ " not defined")

(*
 *)

fun interpStm (PrintStm exps, env:table) =  foldl
                                                (fn (e, acc) =>
                                                    let val (r, newEnv) = interpExpr(e, acc)
                                                        val _ = print ( (Int.toString r) ^ "\n")
                                                    in

                                                        newEnv
                                                    end)
                                                env
                                                exps
  | interpStm (AssignStm (id, e), env:table) = let val (v, newEnv) = interpExpr(e, env)
                                    in
                                        (id, v)::newEnv
                                    end

  | interpStm (CompoundStm(s1, s2), env:table) = interpStm(s2, interpStm(s1, env))
and interpExpr (IdExp id, env:table) = (lookup(env, id), env)
  | interpExpr (NumExp n, env:table) = (n, env)
  | interpExpr (EseqExp (s, exp), env:table) = let val env1 = interpStm(s, env)
                                               in
                                                   interpExpr(exp, env1)
                                               end


  | interpExpr (OpExp (arg1, binop,arg2), env:table) = let val (v1, env1) = interpExpr(arg1, env)
                                                           val (v2, env2) = interpExpr(arg2, env1)


                                                         val binopFn = case binop of
                                                               Plus => op+
                                                               |  Minus => op-
                                                               |  Times => op*
                                                               |  Div => fn(x,y) => x div y
                                                       in
                                                          (binopFn(v1, v2), env2)
                                                      end



val prog =
    CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
                CompoundStm(AssignStm("b", EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],OpExp(NumExp 10, Times, IdExp"a"))),PrintStm[IdExp "b"]))
