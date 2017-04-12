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

val prog =
    CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
                CompoundStm(AssignStm("b", EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],OpExp(NumExp 10, Times, IdExp"a"))),PrintStm[IdExp "b"]))
