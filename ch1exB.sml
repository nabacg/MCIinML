open String

type key = string
datatype 'a tree = LEAF | TREE of 'a tree * (key * 'a) * 'a tree

val empty = LEAF


fun insert (LEAF, (key, value)) = TREE(LEAF, (key, value), LEAF)
  | insert (TREE(l, (k, v), r), (key, value)) =
    if key < k
    then
        TREE(insert(l, (key, value)), (k, v), r)
    else if key > k
    then
        TREE(l, (k, v), insert(r, (key, value)))
    else
        TREE(l, (key, value), r)

fun lookup (LEAF, key) = raise Fail ( "Key not found= " ^ key)
  | lookup (TREE(l, (k, v), r), key) = if key = k
                                  then
                                      v
                                  else
                                      if key < k
                                      then
                                          lookup(l, key)
                                      else
                                          lookup(r, key)


val treemap = foldl (fn (e, t) => insert(t, e))
                    empty [("B", 43), ("F", 555),
                           ("G", 41), ("A", 55), ("F", ~1)]


                    (*

 lookup(treemap, "F");

lookup(treemap, "B");
*)
