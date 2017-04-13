open String

type key = string
datatype tree = LEAF | TREE of tree * key * tree

val empty = LEAF


fun insert (key, LEAF) = TREE(LEAF, key, LEAF)
  | insert (key, TREE(l, k, r)) =
    if key < k
    then
        TREE(insert(key, l), k, r)
    else if key > k
    then
        TREE(l, k, insert(key, r))
    else
        TREE(l, key, r)

fun member (key, LEAF) = false
  | member (key, TREE(l, k, r)) = if key = k
                                  then
                                      true
                                  else
                                      if key < k
                                      then
                                          member(key, l)
                                      else
                                          member(key, r)


val t0 = insert("E", empty)
val t1 = foldl insert t0 ["B", "F", "G", "A"]

val in1 = "t s p i p f b s t"
val in2 = "a b c d e f g h i"


fun splitStr(inStr) =
  List.filter(fn s => s <> " ")
             (foldl (fn (s, acc) => str(s)::acc)
                    []
                    (explode inStr))

val t2 = foldl insert empty (splitStr in1)
val t3 = foldl insert empty (splitStr in2)
