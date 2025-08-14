  $ $MERLIN server stop-server

  $ mkdir test

  $ test_merlin_overrides () {
  > local position="$1"
  > local file="$2"
  > 
  > local locate_output=$(ocamlmerlin server locate -position "$position" -filename "$file" -log-file - -log-section "maxwang"< "$file" -ocamllib-path "$MERLIN_TEST_OCAMLLIB_PATH" \
  >    | jq '{value: .value, cache: .cache}' \
  >    | sed -e 's:"[^"]*lib/ocaml:"lib/ocaml:g' \
  >    | sed -e 's:\\n:\n:g')
  > 
  > local document_output=$(ocamlmerlin server document -position "$position" -filename "$file" -log-file - -log-section "maxwang" < "$file" -ocamllib-path "$MERLIN_TEST_OCAMLLIB_PATH" \
  >    | jq '{value: .value, cache: .cache}' \
  >    | sed -e 's:"[^"]*lib/ocaml:"lib/ocaml:g' \
  >    | sed -e 's:\\n:\n:g')
  > 
  > echo "[merlin locate] output: $locate_output" 
  > echo "[merlin document] output: $document_output" 
  > }

All following tests are performed in /test and merlin has access to /test/.merlin. With 
[USE_PPX_CACHE], tests with use [reader_phase] and [ppx_phase] caches, which are dependencies
of overrides. 

  $ cd test
  $ cat >.merlin <<EOF
  > SOURCE_ROOT ../
  > USE_PPX_CACHE
  > EOF

Test cache hits on second use

  $ cat >./simple.ml <<EOF
  > [@@@do_nothing]
  > [@@@merlin.document
  >   [{
  >      location =
  >        {
  >          loc_start =
  >            { pos_fname = "simple.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 4 };
  >          loc_end =
  >            { pos_fname = "simple.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 14
  >            };
  >          loc_ghost = false
  >        };
  >      payload = "@@@do_nothing expands into nothing"
  >    }]]
  > [@@@merlin.locate
  >   [{
  >      location =
  >        {
  >          loc_start =
  >            { pos_fname = "simple.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 4 };
  >          loc_end =
  >            { pos_fname = "simple.ml"; pos_lnum = 1; pos_bol = 0; pos_cnum = 14
  >            };
  >          loc_ghost = false
  >        };
  >      payload =
  >        {
  >          pos_fname =
  >            "test/ppx.ml";
  >          pos_lnum = 101;
  >          pos_bol = 2833;
  >          pos_cnum = 2854
  >        }
  >    }]]
  > EOF

  $ test_merlin_overrides "1:4" "./simple.ml"
  [merlin locate] output: {
    "value": {
      "file": "$TESTCASE_ROOT/test/ppx.ml",
      "pos": {
        "line": 101,
        "col": 21
      }
    },
    "cache": {
      "reader_phase": "miss",
      "ppx_phase": "miss",
      "typer": "miss",
      "cmt": {
        "hit": 0,
        "miss": 0
      },
      "cms": {
        "hit": 0,
        "miss": 0
      },
      "cmi": {
        "hit": 0,
        "miss": 0
      },
      "document_overrides_phase": "miss",
      "locate_overrides_phase": "miss"
    }
  }
  [merlin document] output: {
    "value": "@@@do_nothing expands into nothing",
    "cache": {
      "reader_phase": "hit",
      "ppx_phase": "hit",
      "typer": "miss",
      "cmt": {
        "hit": 0,
        "miss": 0
      },
      "cms": {
        "hit": 0,
        "miss": 0
      },
      "cmi": {
        "hit": 0,
        "miss": 0
      },
      "document_overrides_phase": "miss",
      "locate_overrides_phase": "miss"
    }
  }

  $ test_merlin_overrides "1:4" "./simple.ml"
  [merlin locate] output: {
    "value": {
      "file": "$TESTCASE_ROOT/test/ppx.ml",
      "pos": {
        "line": 101,
        "col": 21
      }
    },
    "cache": {
      "reader_phase": "hit",
      "ppx_phase": "hit",
      "typer": "miss",
      "cmt": {
        "hit": 0,
        "miss": 0
      },
      "cms": {
        "hit": 0,
        "miss": 0
      },
      "cmi": {
        "hit": 0,
        "miss": 0
      },
      "document_overrides_phase": "miss",
      "locate_overrides_phase": "hit"
    }
  }
  [merlin document] output: {
    "value": "@@@do_nothing expands into nothing",
    "cache": {
      "reader_phase": "hit",
      "ppx_phase": "hit",
      "typer": "miss",
      "cmt": {
        "hit": 0,
        "miss": 0
      },
      "cms": {
        "hit": 0,
        "miss": 0
      },
      "cmi": {
        "hit": 0,
        "miss": 0
      },
      "document_overrides_phase": "hit",
      "locate_overrides_phase": "miss"
    }
  }

Test same file, different position

  $ test_merlin_overrides "1:9" "./simple.ml"
  [merlin locate] output: {
    "value": {
      "file": "$TESTCASE_ROOT/test/ppx.ml",
      "pos": {
        "line": 101,
        "col": 21
      }
    },
    "cache": {
      "reader_phase": "hit",
      "ppx_phase": "hit",
      "typer": "miss",
      "cmt": {
        "hit": 0,
        "miss": 0
      },
      "cms": {
        "hit": 0,
        "miss": 0
      },
      "cmi": {
        "hit": 0,
        "miss": 0
      },
      "document_overrides_phase": "miss",
      "locate_overrides_phase": "hit"
    }
  }
  [merlin document] output: {
    "value": "@@@do_nothing expands into nothing",
    "cache": {
      "reader_phase": "hit",
      "ppx_phase": "hit",
      "typer": "miss",
      "cmt": {
        "hit": 0,
        "miss": 0
      },
      "cms": {
        "hit": 0,
        "miss": 0
      },
      "cmi": {
        "hit": 0,
        "miss": 0
      },
      "document_overrides_phase": "hit",
      "locate_overrides_phase": "miss"
    }
  }

Test cache invalidation

  $ cat >./simple.ml <<EOF
  > [@@@do_nothing]
  > EOF

  $ test_merlin_overrides "1:4" "./simple.ml"
  [merlin locate] output: {
    "value": "Not in environment 'do_nothing'",
    "cache": {
      "reader_phase": "miss",
      "ppx_phase": "miss",
      "typer": "miss",
      "cmt": {
        "hit": 0,
        "miss": 0
      },
      "cms": {
        "hit": 0,
        "miss": 0
      },
      "cmi": {
        "hit": 0,
        "miss": 0
      },
      "document_overrides_phase": "miss",
      "locate_overrides_phase": "miss"
    }
  }
  [merlin document] output: {
    "value": "Not in environment 'do_nothing'",
    "cache": {
      "reader_phase": "hit",
      "ppx_phase": "hit",
      "typer": {
        "reused": 1,
        "typed": 0
      },
      "cmt": {
        "hit": 0,
        "miss": 0
      },
      "cms": {
        "hit": 0,
        "miss": 0
      },
      "cmi": {
        "hit": 0,
        "miss": 0
      },
      "document_overrides_phase": "miss",
      "locate_overrides_phase": "miss"
    }
  }

  $ ocamlmerlin server stop-server
  [255]
