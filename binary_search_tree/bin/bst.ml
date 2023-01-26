open Binary_search_tree.Bst

let unbalanced_tree1 =
  insert
    ( insert
        ( insert
            ( insert
                ( insert
                    ( insert
                        ( insert
                            (insert (insert (empty, "t", 8), "s", 7), "p", 6),
                          "i",
                          5 ),
                      "p",
                      4 ),
                  "f",
                  3 ),
              "b",
              2 ),
          "s",
          1 ),
      "t",
      0 )

let unbalanced_tree2 =
  insert
    ( insert
        ( insert
            ( insert
                ( insert
                    ( insert
                        ( insert
                            (insert (insert (empty, "a", 8), "b", 7), "c", 6),
                          "d",
                          5 ),
                      "e",
                      4 ),
                  "f",
                  3 ),
              "g",
              2 ),
          "h",
          1 ),
      "i",
      0 )

let () =
  let v = lookup (unbalanced_tree1, "b") in
  print_int v;
  print_newline ();
  let u = lookup (unbalanced_tree2, "c") in
  print_int u;
  print_newline ();
  print_string (string_of_int_tree unbalanced_tree1);
  print_newline ();
  print_string (string_of_int_tree unbalanced_tree2)
