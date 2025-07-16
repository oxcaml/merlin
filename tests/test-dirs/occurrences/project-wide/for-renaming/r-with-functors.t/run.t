
  $ $OCAMLC -bin-annot-cms -bin-annot-occurrences -c func.mli func.ml
  $ $OCAMLC -bin-annot-cms -bin-annot-occurrences -o main.exe func.cmo main.ml
  $ ocaml-index aggregate func.cms func.cmsi main.cms -o index.merlin-index

  $ ./main.exe
  Hello world!

  $ ocaml-index dump index.merlin-index
  11 uids:
  {uid: [intf]Func.0; locs: "txt": File "func.mli", line 1, characters 24-27
   uid: Func.0; locs: "txt": File "func.ml", line 1, characters 24-27
   uid: Main.0; locs:
     "txt": File "main.ml", line 1, characters 22-25;
     "M.txt": File "main.ml", line 4, characters 14-19
   uid: [intf]Func.1; locs:
     "P": File "func.mli", line 1, characters 12-13;
     "P": File "func.mli", line 3, characters 17-18;
     "P": File "func.mli", line 4, characters 10-11
   uid: Func.1; locs: "P": File "func.ml", line 1, characters 12-13
   uid: Main.1; locs:
     "P": File "main.ml", line 1, characters 7-8;
     "P": File "main.ml", line 2, characters 21-22
   uid: [intf]Func.2; locs: "Make": File "func.mli", line 3, characters 7-11
   uid: Func.2; locs: "txt": File "func.ml", line 3, characters 30-33
   uid: Main.2; locs: "M": File "main.ml", line 2, characters 7-8
   uid: Func.3; locs: "Params": File "func.ml", line 4, characters 10-16
   uid: Func.4; locs:
     "Make": File "func.ml", line 3, characters 7-11;
     "Func.Make": File "main.ml", line 2, characters 11-20
   }, 0 approx shapes: {}, and shapes for CUS .
  and related uids:{([intf]Func.2 Func.4); ([intf]Func.1 Func.1); ([intf]Func.0
                   Func.0 Main.0 Func.2)}

We expect 2 occurrences in func.ml, 1 in func.mli and 2 in main.ml
  $ $MERLIN single occurrences -scope renaming -identifier-at 4:18 \
  > -filename main.ml <main.ml | jq '.value[] | .file,.start'
  "$TESTCASE_ROOT/main.ml"
  {
    "line": 1,
    "col": 22
  }
  "$TESTCASE_ROOT/main.ml"
  {
    "line": 4,
    "col": 16
  }
  "$TESTCASE_ROOT/func.ml"
  {
    "line": 1,
    "col": 24
  }
  "$TESTCASE_ROOT/func.ml"
  {
    "line": 3,
    "col": 30
  }
  "$TESTCASE_ROOT/func.mli"
  {
    "line": 1,
    "col": 24
  }

  $ $MERLIN single occurrences -scope renaming -identifier-at 4:18 \
  > -filename main.ml <main.ml -log-file -
  # 0.01 Mconfig - normalize
  {
    "ocaml": {
      "include_dirs": [],
      "hidden_dirs": [],
      "no_std_include": false,
      "unsafe": false,
      "classic": false,
      "principal": false,
      "real_paths": true,
      "recursive_types": false,
      "strict_sequence": false,
      "applicative_functors": true,
      "nopervasives": false,
      "strict_formats": false,
      "open_modules": [],
      "ppx": [],
      "pp": null,
      "warnings": {
        "actives": [
          1, 2, 3, 5, 6, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
          23, 24, 25, 26, 28, 31, 43, 46, 47, 49, 51, 52, 53, 54, 55, 56, 57,
          58, 59, 61, 62, 63, 64, 65, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80,
          81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97,
          98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
          112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125,
          126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139,
          140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153,
          154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167,
          168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181,
          182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195,
          196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209,
          210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223,
          224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237,
          238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250
        ],
        "warn_error": [],
        "alerts": {
          "alerts": [ "unstable", "unsynchronized_access" ],
          "complement": true
        },
        "alerts_error": { "alerts": [], "complement": false }
      },
      "cmi_file": null,
      "parameters": [],
      "as_parameter": false,
      "zero_alloc_check": "default",
      "zero_alloc_assert": "default"
    },
    "merlin": {
      "build_path": [],
      "source_path": [],
      "hidden_build_path": [],
      "hidden_source_path": [],
      "cmi_path": [],
      "cmt_path": [],
      "index_files": [
        "$TESTCASE_ROOT/index.merlin-index"
      ],
      "flags_applied": [],
      "extensions": [],
      "suffixes": [
        { "impl": ".ml", "intf": ".mli" }, { "impl": ".re", "intf": ".rei" }
      ],
      "stdlib": "/home/lstevenson/local/github/flambda-backend/hashes/e609909979262053d552213efd4996d983c399b7/_install/lib/ocaml",
      "source_root": null,
      "unit_name": null,
      "unit_name_for": {},
      "wrapping_prefix": null,
      "reader": [],
      "protocol": "json",
      "log_file": "-",
      "log_sections": [],
      "flags_to_apply": [],
      "failures": [],
      "assoc_suffixes": [
        { "extension": ".re", "reader": "reason" },
        { "extension": ".rei", "reader": "reason" }
      ],
      "cache_lifespan": "5"
    },
    "query": {
      "filename": "main.ml",
      "directory": "$TESTCASE_ROOT",
      "printer_width": 0,
      "verbosity": "lvl 0"
    }
  }
  # 0.01 Pipeline - pop_cache
  nothing cached for this configuration
  # 0.01 New_commands - run(query)
  {
    "command": "occurrences",
    "kind": "identifiers",
    "position": { "line": 4, "column": 18 },
    "scope": "renaming"
  }
  # 0.01 Mconfig - normalize
  {
    "ocaml": {
      "include_dirs": [],
      "hidden_dirs": [],
      "no_std_include": false,
      "unsafe": false,
      "classic": false,
      "principal": false,
      "real_paths": true,
      "recursive_types": false,
      "strict_sequence": false,
      "applicative_functors": true,
      "nopervasives": false,
      "strict_formats": false,
      "open_modules": [],
      "ppx": [],
      "pp": null,
      "warnings": {
        "actives": [
          1, 2, 3, 5, 6, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
          23, 24, 25, 26, 28, 31, 43, 46, 47, 49, 51, 52, 53, 54, 55, 56, 57,
          58, 59, 61, 62, 63, 64, 65, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80,
          81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97,
          98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111,
          112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125,
          126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139,
          140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153,
          154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167,
          168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181,
          182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195,
          196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209,
          210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223,
          224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237,
          238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250
        ],
        "warn_error": [],
        "alerts": {
          "alerts": [ "unstable", "unsynchronized_access" ],
          "complement": true
        },
        "alerts_error": { "alerts": [], "complement": false }
      },
      "cmi_file": null,
      "parameters": [],
      "as_parameter": false,
      "zero_alloc_check": "default",
      "zero_alloc_assert": "default"
    },
    "merlin": {
      "build_path": [],
      "source_path": [],
      "hidden_build_path": [],
      "hidden_source_path": [],
      "cmi_path": [],
      "cmt_path": [],
      "index_files": [
        "$TESTCASE_ROOT/index.merlin-index"
      ],
      "flags_applied": [],
      "extensions": [],
      "suffixes": [
        { "impl": ".ml", "intf": ".mli" }, { "impl": ".re", "intf": ".rei" }
      ],
      "stdlib": "/home/lstevenson/local/github/flambda-backend/hashes/e609909979262053d552213efd4996d983c399b7/_install/lib/ocaml",
      "source_root": null,
      "unit_name": null,
      "unit_name_for": {},
      "wrapping_prefix": null,
      "reader": [],
      "protocol": "json",
      "log_file": "-",
      "log_sections": [],
      "flags_to_apply": [],
      "failures": [],
      "assoc_suffixes": [
        { "extension": ".re", "reader": "reason" },
        { "extension": ".rei", "reader": "reason" }
      ],
      "cache_lifespan": "5"
    },
    "query": {
      "filename": "main.ml",
      "directory": "$TESTCASE_ROOT",
      "printer_width": 0,
      "verbosity": "lvl 0"
    }
  }
  # 0.01 Phase cache - Reader phase
  Cache is disabled: configuration
  # 0.01 Mreader - run
  extension("main.ml") = ".ml"
  # 0.01 Phase cache - PPX phase
  Cache is disabled: reader cache is disabled
  # 0.01 Mconfig - build_path
  2 items in path, 2 after deduplication
  # 0.01 Mconfig - build_path
  2 items in path, 2 after deduplication
  # 0.01 File_cache(Directory_content_cache) - read
  reading "$TESTCASE_ROOT" from disk
  # 0.01 File_cache(Directory_content_cache) - read
  reading "/home/lstevenson/local/github/flambda-backend/hashes/e609909979262053d552213efd4996d983c399b7/_install/lib/ocaml" from disk
  # 0.01 File_cache(Cmi_cache) - read
  reading "/home/lstevenson/local/github/flambda-backend/hashes/e609909979262053d552213efd4996d983c399b7/_install/lib/ocaml/stdlib.cmi" from disk
  # 0.01 File_cache(Cmi_cache) - read
  reading "$TESTCASE_ROOT/func.cmi" from disk
  # 0.01 index-occurrences - index_buffer
  Path: Func!.Make
  # 0.01 index-occurrences - index_buffer
  Shape of path: CU Func . "Make"[module]
  # 0.01 index-occurrences - read_unit_shape
  inspecting Func
  # 0.01 File_cache(Cms_cache) - read
  reading "$TESTCASE_ROOT/func.cms" from disk
  # 0.01 index-occurrences - read_unit_shape
  shapes loaded for Func
  # 0.01 index-occurrences - index_buffer
  Found Func.Make (File "$TESTCASE_ROOT/main.ml", line 2, characters 11-20) wiht uid Func.4
  # 0.01 index-occurrences - index_buffer
  Path: P/282[1]
  # 0.01 index-occurrences - index_buffer
  Shape of path: {<Main.1>
   "txt"[value] -> <Main.0>;
   }
  # 0.01 index-occurrences - index_buffer
  Found P (File "$TESTCASE_ROOT/main.ml", line 2, characters 21-22) wiht uid Main.1
  # 0.01 index-occurrences - index_buffer
  Path: Stdlib!.print_endline
  # 0.01 index-occurrences - index_buffer
  Shape of path: CU Stdlib . "print_endline"[value]
  # 0.01 index-occurrences - read_unit_shape
  inspecting Stdlib
  # 0.01 File_cache(Cmt_cache) - read
  reading "/home/lstevenson/local/github/flambda-backend/hashes/e609909979262053d552213efd4996d983c399b7/_install/lib/ocaml/stdlib.cmt" from disk
  # 0.01 index-occurrences - read_unit_shape
  shapes loaded for Stdlib
  # 0.01 index-occurrences - index_buffer
  Found print_endline (File "$TESTCASE_ROOT/main.ml", line 4, characters 0-13) wiht uid Stdlib.321
  # 0.01 index-occurrences - index_buffer
  Path: M/291[2].txt
  # 0.01 index-occurrences - index_buffer
  Shape of path: CU Func . "Make"[module]({<Main.1>
                            "txt"[value] -> <Main.0>;
                            })<Main.2>
  . "txt"[value]
  # 0.01 index-occurrences - read_unit_shape
  inspecting Func
  # 0.01 stat_cache - reuse cache
  $TESTCASE_ROOT/func.cms
  # 0.01 File_cache(Cms_cache) - read
  reusing "$TESTCASE_ROOT/func.cms"
  # 0.01 index-occurrences - read_unit_shape
  shapes loaded for Func
  # 0.01 index-occurrences - index_buffer
  Found M.txt (File "$TESTCASE_ROOT/main.ml", line 4, characters 14-19) wiht uid Main.0
  # 0.01 Mtyper - node_at
  Node: [ structure ]
  # 0.01 Mtyper - node_at
  Deepest before [ expression; expression; structure_item; structure ]
  # 0.01 type-enclosing - reconstruct-identifier
  paths: [M;txt]
  # 0.01 locate - reconstructed identifier
  M.txt
  # 0.01 occurrences - occurrences
  Looking for occurences of M.txt (pos: 4:18)
  # 0.01 context - inspect_context
  current node is: [[ structure ]]
  # 0.01 context - inspect_context
  current enclosing node is: expression
  # 0.01 context - inspect_context
  name is: [txt]
  # 0.01 locate - from_string
  inferred context: expression
  # 0.01 locate - from_string
  looking for the source of 'M.txt' (prioritizing .ml files)
  # 0.01 env-lookup - lookup
  lookup in value namespace
  # 0.01 env-lookup - env_lookup
  found: 'M/291[2].txt' in namespace value with decl_uid [intf]Func.0
  at loc File "func.mli", line 1, characters 20-36
  # 0.01 locate - shape_of_path
  initial: CU Func . "Make"[module]({<Main.1>
                            "txt"[value] -> <Main.0>;
                            })<Main.2>
  . "txt"[value]
  # 0.01 locate - read_unit_shape
  inspecting Func
  # 0.01 Mconfig - cmt_path
  2 items in path, 2 after deduplication
  # 0.01 locate - find_file_with_path
  Try find "Func"
  # 0.01 locate - find_file_with_path
  Trying "Func.cms"
  # 0.01 stat_cache - reuse cache
  $TESTCASE_ROOT
  # 0.01 File_cache(Exists_in_directory) - read
  reading "$TESTCASE_ROOT" from disk
  # 0.01 locate - load_cmt
  Found "Func" at path "$TESTCASE_ROOT/func.cms"
  # 0.01 stat_cache - reuse cache
  $TESTCASE_ROOT/func.cms
  # 0.01 File_cache(Cms_cache) - read
  reusing "$TESTCASE_ROOT/func.cms"
  # 0.01 locate - File_switching.move_to
  file: $TESTCASE_ROOT/func.cms
  digest: 1dd90d0e2c624204f6af265c2fdfa32d
  # 0.01 locate - File_switching.move_to
  file: func.ml
  digest: 1dd90d0e2c624204f6af265c2fdfa32d
  # 0.01 locate - read_unit_shape
  shapes loaded for Func
  # 0.01 locate - shape_of_path
  reduced: Resolved: Main.0
  # 0.01 locate - find_loc_of_uid
  We look for Main.0 in the current compilation unit.
  # 0.01 locate - find_loc_of_uid
  Looking for Main.0 in the uid_to_loc table
  # 0.01 locate - find_source
  attempt to find "main.ml"
  # 0.01 locate - find_source
  initial path: "func.ml"
  # 0.01 locate - find_all_in_path_uncap
  Looking for file "Main.ml" in path:
  $TESTCASE_ROOT
  /home/lstevenson/local/github/flambda-backend/hashes/e609909979262053d552213efd4996d983c399b7/_install/lib/ocaml
  # 0.01 stat_cache - reuse cache
  $TESTCASE_ROOT
  # 0.01 File_cache(Exists_in_directory) - read
  reusing "$TESTCASE_ROOT"
  # 0.01 stat_cache - reuse cache
  $TESTCASE_ROOT
  # 0.01 File_cache(Exists_in_directory) - read
  reusing "$TESTCASE_ROOT"
  # 0.01 stat_cache - reuse cache
  /home/lstevenson/local/github/flambda-backend/hashes/e609909979262053d552213efd4996d983c399b7/_install/lib/ocaml
  # 0.01 File_cache(Exists_in_directory) - read
  reading "/home/lstevenson/local/github/flambda-backend/hashes/e609909979262053d552213efd4996d983c399b7/_install/lib/ocaml" from disk
  # 0.01 stat_cache - reuse cache
  /home/lstevenson/local/github/flambda-backend/hashes/e609909979262053d552213efd4996d983c399b7/_install/lib/ocaml
  # 0.01 File_cache(Exists_in_directory) - read
  reusing "/home/lstevenson/local/github/flambda-backend/hashes/e609909979262053d552213efd4996d983c399b7/_install/lib/ocaml"
  # 0.01 locate - find_all_in_path_uncap
  Looking for file "Main.re" in path:
  $TESTCASE_ROOT
  /home/lstevenson/local/github/flambda-backend/hashes/e609909979262053d552213efd4996d983c399b7/_install/lib/ocaml
  # 0.01 stat_cache - reuse cache
  $TESTCASE_ROOT
  # 0.01 File_cache(Exists_in_directory) - read
  reusing "$TESTCASE_ROOT"
  # 0.01 stat_cache - reuse cache
  $TESTCASE_ROOT
  # 0.01 File_cache(Exists_in_directory) - read
  reusing "$TESTCASE_ROOT"
  # 0.01 stat_cache - reuse cache
  /home/lstevenson/local/github/flambda-backend/hashes/e609909979262053d552213efd4996d983c399b7/_install/lib/ocaml
  # 0.01 File_cache(Exists_in_directory) - read
  reusing "/home/lstevenson/local/github/flambda-backend/hashes/e609909979262053d552213efd4996d983c399b7/_install/lib/ocaml"
  # 0.01 stat_cache - reuse cache
  /home/lstevenson/local/github/flambda-backend/hashes/e609909979262053d552213efd4996d983c399b7/_install/lib/ocaml
  # 0.01 File_cache(Exists_in_directory) - read
  reusing "/home/lstevenson/local/github/flambda-backend/hashes/e609909979262053d552213efd4996d983c399b7/_install/lib/ocaml"
  # 0.01 locate - find_source
  Found file: $TESTCASE_ROOT/main.ml (File "main.ml", line 1, characters 22-25)
  # 0.01 occurrences - locs_of
  Found definition uid using locate: Main.0 
  # 0.01 occurrences - locs_of
  Definition has uid Main.0 (File "main.ml", line 1, characters 22-25)
  # 0.01 occurrences - locs_of
  Indexing current buffer
  # 0.01 File_cache(Index_cache) - read
  reading "$TESTCASE_ROOT/index.merlin-index" from disk
  # 0.01 occurrences - find_linked_uids
  Found related uids: [[intf]Func.0;Func.0;Main.0;Func.2;]
  # 0.01 Mconfig - cmt_path
  2 items in path, 2 after deduplication
  # 0.01 locate - find_file_with_path
  Try find "Func"
  # 0.01 locate - find_file_with_path
  Trying "Func.cmsi"
  # 0.01 stat_cache - reuse cache
  $TESTCASE_ROOT
  # 0.01 File_cache(Exists_in_directory) - read
  reusing "$TESTCASE_ROOT"
  # 0.01 locate - load_cmt
  Found "Func" at path "$TESTCASE_ROOT/func.cmsi"
  # 0.01 File_cache(Cms_cache) - read
  reading "$TESTCASE_ROOT/func.cmsi" from disk
  # 0.01 locate - File_switching.move_to
  file: $TESTCASE_ROOT/func.cmsi
  digest: 4c53b6c54f55bd0895cef9ee2d86e138
  # 0.01 locate - lookup_uid_decl
  Cmt successfully loaded, looking for [intf]Func.0
  # 0.01 Mconfig - cmt_path
  2 items in path, 2 after deduplication
  # 0.01 locate - find_file_with_path
  Try find "Func"
  # 0.01 locate - find_file_with_path
  Trying "Func.cms"
  # 0.01 stat_cache - reuse cache
  $TESTCASE_ROOT
  # 0.01 File_cache(Exists_in_directory) - read
  reusing "$TESTCASE_ROOT"
  # 0.01 locate - load_cmt
  Found "Func" at path "$TESTCASE_ROOT/func.cms"
  # 0.01 stat_cache - reuse cache
  $TESTCASE_ROOT/func.cms
  # 0.01 File_cache(Cms_cache) - read
  reusing "$TESTCASE_ROOT/func.cms"
  # 0.01 locate - File_switching.move_to
  file: $TESTCASE_ROOT/func.cms
  digest: 1dd90d0e2c624204f6af265c2fdfa32d
  # 0.01 locate - lookup_uid_decl
  Cmt successfully loaded, looking for Func.0
  # 0.01 Mconfig - cmt_path
  2 items in path, 2 after deduplication
  # 0.01 locate - find_file_with_path
  Try find "Main"
  # 0.01 locate - find_file_with_path
  Trying "Main.cms"
  # 0.01 stat_cache - reuse cache
  $TESTCASE_ROOT
  # 0.01 File_cache(Exists_in_directory) - read
  reusing "$TESTCASE_ROOT"
  # 0.01 locate - load_cmt
  Found "Main" at path "$TESTCASE_ROOT/main.cms"
  # 0.01 File_cache(Cms_cache) - read
  reading "$TESTCASE_ROOT/main.cms" from disk
  # 0.01 locate - File_switching.move_to
  file: $TESTCASE_ROOT/main.cms
  digest: 30fdf844ed770fa5d598d261e569de14
  # 0.01 locate - lookup_uid_decl
  Cmt successfully loaded, looking for Main.0
  # 0.01 Mconfig - cmt_path
  2 items in path, 2 after deduplication
  # 0.01 locate - find_file_with_path
  Try find "Func"
  # 0.01 locate - find_file_with_path
  Trying "Func.cms"
  # 0.01 stat_cache - reuse cache
  $TESTCASE_ROOT
  # 0.01 File_cache(Exists_in_directory) - read
  reusing "$TESTCASE_ROOT"
  # 0.01 locate - load_cmt
  Found "Func" at path "$TESTCASE_ROOT/func.cms"
  # 0.01 stat_cache - reuse cache
  $TESTCASE_ROOT/func.cms
  # 0.01 File_cache(Cms_cache) - read
  reusing "$TESTCASE_ROOT/func.cms"
  # 0.01 locate - File_switching.move_to
  file: $TESTCASE_ROOT/func.cms
  digest: 1dd90d0e2c624204f6af265c2fdfa32d
  # 0.01 locate - lookup_uid_decl
  Cmt successfully loaded, looking for Func.2
  # 0.01 occurrences - get_external_locs
  Lookin for occurrences of Main.0 in index $TESTCASE_ROOT/index.merlin-index
  # 0.01 stat_cache - reuse cache
  $TESTCASE_ROOT/index.merlin-index
  # 0.01 File_cache(Index_cache) - read
  reusing "$TESTCASE_ROOT/index.merlin-index"
  # 0.01 occurrences - get_external_locs
  Lookin for occurrences of [intf]Func.0 in index $TESTCASE_ROOT/index.merlin-index
  # 0.01 stat_cache - reuse cache
  $TESTCASE_ROOT/index.merlin-index
  # 0.01 File_cache(Index_cache) - read
  reusing "$TESTCASE_ROOT/index.merlin-index"
  # 0.01 occurrences - get_external_locs
  Lookin for occurrences of Func.0 in index $TESTCASE_ROOT/index.merlin-index
  # 0.01 stat_cache - reuse cache
  $TESTCASE_ROOT/index.merlin-index
  # 0.01 File_cache(Index_cache) - read
  reusing "$TESTCASE_ROOT/index.merlin-index"
  # 0.01 occurrences - get_external_locs
  Lookin for occurrences of Main.0 in index $TESTCASE_ROOT/index.merlin-index
  # 0.01 stat_cache - reuse cache
  $TESTCASE_ROOT/index.merlin-index
  # 0.01 File_cache(Index_cache) - read
  reusing "$TESTCASE_ROOT/index.merlin-index"
  # 0.01 occurrences - get_external_locs
  Lookin for occurrences of Func.2 in index $TESTCASE_ROOT/index.merlin-index
  # 0.01 stat_cache - reuse cache
  $TESTCASE_ROOT/index.merlin-index
  # 0.01 File_cache(Index_cache) - read
  reusing "$TESTCASE_ROOT/index.merlin-index"
  # 0.01 occurrences - occurrences
  Found 4 locs
  # 0.01 occurrences - occurrences
  Found occ: M File "$TESTCASE_ROOT/main.ml", line 4, characters 14-19
  # 0.01 occurrences - occurrences
  Found occ: txt File "$TESTCASE_ROOT/func.ml", line 1, characters 24-27
  # 0.01 occurrences - occurrences
  Found occ: txt File "$TESTCASE_ROOT/func.ml", line 3, characters 30-33
  # 0.01 occurrences - occurrences
  Found occ: txt File "$TESTCASE_ROOT/func.mli", line 1, characters 24-27
  # 0.01 New_merlin - run(result)
  {
    "class": "return",
    "value": [
      {
        "file": "$TESTCASE_ROOT/main.ml",
        "start": { "line": 1, "col": 22 },
        "end": { "line": 1, "col": 25 },
        "stale": false
      },
      {
        "file": "$TESTCASE_ROOT/main.ml",
        "start": { "line": 4, "col": 16 },
        "end": { "line": 4, "col": 19 },
        "stale": false
      },
      {
        "file": "$TESTCASE_ROOT/func.ml",
        "start": { "line": 1, "col": 24 },
        "end": { "line": 1, "col": 27 },
        "stale": false
      },
      {
        "file": "$TESTCASE_ROOT/func.ml",
        "start": { "line": 3, "col": 30 },
        "end": { "line": 3, "col": 33 },
        "stale": false
      },
      {
        "file": "$TESTCASE_ROOT/func.mli",
        "start": { "line": 1, "col": 24 },
        "end": { "line": 1, "col": 27 },
        "stale": false
      }
    ],
    "notifications": [],
    "timing": {
      "clock": 10,
      "cpu": 9,
      "query": 2,
      "pp": 0,
      "reader": 0,
      "ppx": 0,
      "typer": 6,
      "error": 0
    },
    "heap_mbytes": 4,
    "cache": {
      "reader_phase": "miss",
      "ppx_phase": "miss",
      "typer": "miss",
      "cmt": { "hit": 0, "miss": 1 },
      "cms": { "hit": 4, "miss": 1 },
      "cmi": { "hit": 0, "miss": 2 }
    },
    "query_num": 0
  }
  {
    "class": "return",
    "value": [
      {
        "file": "$TESTCASE_ROOT/main.ml",
        "start": {
          "line": 1,
          "col": 22
        },
        "end": {
          "line": 1,
          "col": 25
        },
        "stale": false
      },
      {
        "file": "$TESTCASE_ROOT/main.ml",
        "start": {
          "line": 4,
          "col": 16
        },
        "end": {
          "line": 4,
          "col": 19
        },
        "stale": false
      },
      {
        "file": "$TESTCASE_ROOT/func.ml",
        "start": {
          "line": 1,
          "col": 24
        },
        "end": {
          "line": 1,
          "col": 27
        },
        "stale": false
      },
      {
        "file": "$TESTCASE_ROOT/func.ml",
        "start": {
          "line": 3,
          "col": 30
        },
        "end": {
          "line": 3,
          "col": 33
        },
        "stale": false
      },
      {
        "file": "$TESTCASE_ROOT/func.mli",
        "start": {
          "line": 1,
          "col": 24
        },
        "end": {
          "line": 1,
          "col": 27
        },
        "stale": false
      }
    ],
    "notifications": []
  }
