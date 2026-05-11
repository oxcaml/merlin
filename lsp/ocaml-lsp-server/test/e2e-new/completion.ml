open Async
open Test.Import

let iter_completions
  ?prep
  ?path
  ?(triggerCharacter = "")
  ?(triggerKind = CompletionTriggerKind.Invoked)
  ~position
  =
  let makeRequest textDocument =
    let context = CompletionContext.create ~triggerCharacter ~triggerKind () in
    Lsp.Client_request.TextDocumentCompletion
      (CompletionParams.create ~textDocument ~position ~context ())
  in
  Lsp_helpers.iter_lsp_response ?prep ?path ~makeRequest
;;

let print_completions
  ?(prep = fun _ -> Fiber.return ())
  ?(path = "foo.ml")
  ?(limit = 10)
  ?(pre_print = fun x -> x)
  source
  position
  =
  iter_completions ~prep ~path ~source ~position (function
    | None -> print_endline "No completion Items"
    | Some completions ->
      let items =
        match completions with
        | `CompletionList comp -> comp.items
        | `List comp -> comp
      in
      items
      |> pre_print
      |> (function
       | [] -> print_endline "No completions"
       | items ->
         print_endline "Completions:";
         let originalLength = List.length items in
         items
         |> List.take (min limit originalLength)
         |> List.iter ~f:(fun item ->
           item
           |> CompletionItem.yojson_of_t
           |> Yojson.Safe.pretty_to_string ~std:false
           |> print_endline);
         if originalLength > limit then print_endline "............."))
;;

let%expect_test "can start completion at arbitrary position (before the dot)" =
  let source = {ocaml|Strin.func|ocaml} in
  let position = Position.create ~line:0 ~character:5 in
  let%map () = print_completions source position in
  [%expect
    {| No completions |}]
;;

let%expect_test "can start completion at arbitrary position" =
  let source = {ocaml|StringLabels|ocaml} in
  let position = Position.create ~line:0 ~character:6 in
  let%map () = print_completions source position in
  [%expect
    {| No completions |}]
;;

let%expect_test "can start completion at arbitrary position 2" =
  let source = {ocaml|StringLabels|ocaml} in
  let position = Position.create ~line:0 ~character:7 in
  let%map () = print_completions source position in
  [%expect
    {| No completions |}]
;;

let%expect_test "can start completion after operator without space" =
  let source = {ocaml|[1;2]|>List.ma|ocaml} in
  let position = Position.create ~line:0 ~character:14 in
  let%map () = print_completions source position in
  [%expect
    {| No completions |}]
;;

let%expect_test "can start completion after operator with space" =
  let source = {ocaml|[1;2] |> List.ma|ocaml} in
  let position = Position.create ~line:0 ~character:16 in
  let%map () = print_completions source position in
  [%expect
    {| No completions |}]
;;

let%expect_test "can start completion in dot chain with tab" =
  let source = {ocaml|[1;2] |> List.	ma|ocaml} in
  let position = Position.create ~line:0 ~character:17 in
  let%map () = print_completions source position in
  [%expect
    {| No completions |}]
;;

let%expect_test "can start completion in dot chain with newline" =
  let source =
    {ocaml|[1;2] |> List.
ma|ocaml}
  in
  let position = Position.create ~line:1 ~character:2 in
  let%map () = print_completions source position in
  [%expect
    {| No completions |}]
;;

let%expect_test "can start completion in dot chain with space" =
  let source = {ocaml|[1;2] |> List. ma|ocaml} in
  let position = Position.create ~line:0 ~character:17 in
  let%map () = print_completions source position in
  [%expect
    {| No completions |}]
;;

let%expect_test "can start completion after dereference" =
  let source =
    {ocaml|let apple=ref 10 in
!ap|ocaml}
  in
  let position = Position.create ~line:1 ~character:3 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "'a",
      "kind": 12,
      "label": "apple",
      "sortText": "0000",
      "textEdit": {
        "newText": "apple",
        "range": {
          "end": { "character": 3, "line": 1 },
          "start": { "character": 1, "line": 1 }
        }
      }
    }
    |}]
;;

let%expect_test "can complete symbol passed as a named argument" =
  let source =
    {ocaml|let g ~f = f 0 in
g ~f:ig|ocaml}
  in
  let position = Position.create ~line:1 ~character:7 in
  let%map () = print_completions source position in
  [%expect
    {| No completions |}]
;;

let%expect_test "can complete symbol passed as a named argument - 2" =
  let source =
    {ocaml|module M = struct let igfoo _x = () end
let g ~f = f 0 in
g ~f:M.ig|ocaml}
  in
  let position = Position.create ~line:2 ~character:9 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "'a -> unit",
      "kind": 12,
      "label": "igfoo",
      "sortText": "0000",
      "textEdit": {
        "newText": "igfoo",
        "range": {
          "end": { "character": 9, "line": 2 },
          "start": { "character": 7, "line": 2 }
        }
      }
    }
    |}]
;;

let%expect_test "can complete symbol passed as an optional argument" =
  let source =
    {ocaml|
let g ?f = f in
g ?f:ig
    |ocaml}
  in
  let position = Position.create ~line:2 ~character:7 in
  let%map () = print_completions source position in
  [%expect
    {| No completions |}]
;;

let%expect_test "can complete symbol passed as an optional argument - 2" =
  let source =
    {ocaml|module M = struct let igfoo _x = () end
let g ?f = f in
g ?f:M.ig|ocaml}
  in
  let position = Position.create ~line:2 ~character:9 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "'a -> unit",
      "kind": 12,
      "label": "igfoo",
      "sortText": "0000",
      "textEdit": {
        "newText": "igfoo",
        "range": {
          "end": { "character": 9, "line": 2 },
          "start": { "character": 7, "line": 2 }
        }
      }
    }
    |}]
;;

let%expect_test "completes identifier after completion-triggering character" =
  let source =
    {ocaml|
module Test = struct
  let somenum = 42
  let somestring = "hello"
end

let x = Test.
    |ocaml}
  in
  let position = Position.create ~line:6 ~character:13 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "int",
      "kind": 12,
      "label": "somenum",
      "sortText": "0000",
      "textEdit": {
        "newText": "somenum",
        "range": {
          "end": { "character": 13, "line": 6 },
          "start": { "character": 13, "line": 6 }
        }
      }
    }
    {
      "detail": "string",
      "kind": 12,
      "label": "somestring",
      "sortText": "0001",
      "textEdit": {
        "newText": "somestring",
        "range": {
          "end": { "character": 13, "line": 6 },
          "start": { "character": 13, "line": 6 }
        }
      }
    }
    |}]
;;

let%expect_test "completes infix operators" =
  let source =
    {ocaml|
let (>>|) = (+)
let y = 1 >
|ocaml}
  in
  let position = Position.create ~line:2 ~character:11 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "'a",
      "kind": 12,
      "label": ">>|",
      "sortText": "0000",
      "textEdit": {
        "newText": ">>|",
        "range": {
          "end": { "character": 11, "line": 2 },
          "start": { "character": 10, "line": 2 }
        }
      }
    }
    |}]
;;

let%expect_test "completes without prefix" =
  let source =
    {ocaml|
let somenum = 42
let somestring = "hello"

let plus_42 (x:int) (y:int) =
  somenum +
|ocaml}
  in
  let position = Position.create ~line:5 ~character:12 in
  let%map () = print_completions source position in
  [%expect
    {| No completions |}]
;;

let%expect_test "completes labels" =
  let source = {ocaml|let f = ListLabels.map ~|ocaml} in
  let position = Position.create ~line:0 ~character:24 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "'_weak1",
      "kind": 5,
      "label": "~",
      "sortText": "0000",
      "textEdit": {
        "newText": "~",
        "range": {
          "end": { "character": 24, "line": 0 },
          "start": { "character": 23, "line": 0 }
        }
      }
    }
    |}]
;;

let%expect_test "works for polymorphic variants - function application context - 1" =
  let source =
    {ocaml|
let f (_a: [`String | `Int of int]) = ()

let u = f `Str
  |ocaml}
  in
  let position = Position.create ~line:3 ~character:14 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "`String",
      "kind": 20,
      "label": "`String",
      "sortText": "0000",
      "textEdit": {
        "newText": "`String",
        "range": {
          "end": { "character": 14, "line": 3 },
          "start": { "character": 10, "line": 3 }
        }
      }
    }
    |}]
;;

let%expect_test "works for polymorphic variants - function application context - 2" =
  let source =
    {ocaml|
let f (_a: [`String | `Int of int]) = ()

let u = f `In
  |ocaml}
  in
  let position = Position.create ~line:3 ~character:13 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "`Int of int",
      "kind": 20,
      "label": "`Int",
      "sortText": "0000",
      "textEdit": {
        "newText": "`Int",
        "range": {
          "end": { "character": 13, "line": 3 },
          "start": { "character": 10, "line": 3 }
        }
      }
    }
    |}]
;;

let%expect_test "works for polymorphic variants" =
  let source =
    {ocaml|
type t = [ `Int | `String ]

let x : t = `I
  |ocaml}
  in
  let position = Position.create ~line:3 ~character:15 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "`Int",
      "kind": 20,
      "label": "`Int",
      "sortText": "0000",
      "textEdit": {
        "newText": "`Int",
        "range": {
          "end": { "character": 15, "line": 3 },
          "start": { "character": 13, "line": 3 }
        }
      }
    }
    |}]
;;

let%expect_test "completion for holes" =
  let source = {ocaml|let u : int = _|ocaml} in
  let position = Position.create ~line:0 ~character:15 in
  let filter =
    List.filter ~f:(fun (item : CompletionItem.t) ->
      not (String.starts_with ~prefix:"__" item.label))
  in
  let%map () = print_completions ~pre_print:filter source position in
  [%expect
    {|
    Completions:
    {
      "filterText": "_0",
      "kind": 1,
      "label": "0",
      "sortText": "0000",
      "textEdit": {
        "newText": "0",
        "range": {
          "end": { "character": 15, "line": 0 },
          "start": { "character": 14, "line": 0 }
        }
      }
    }
    |}]
;;

let%expect_test "completes identifier at top level" =
  let source =
    {ocaml|
let somenum = 42
let somestring = "hello"

let () =
  some
|ocaml}
  in
  let position = Position.create ~line:5 ~character:6 in
  let%map () = print_completions source position in
  [%expect
    {|
    Completions:
    {
      "detail": "int",
      "kind": 12,
      "label": "somenum",
      "sortText": "0000",
      "textEdit": {
        "newText": "somenum",
        "range": {
          "end": { "character": 6, "line": 5 },
          "start": { "character": 2, "line": 5 }
        }
      }
    }
    {
      "detail": "string",
      "kind": 12,
      "label": "somestring",
      "sortText": "0001",
      "textEdit": {
        "newText": "somestring",
        "range": {
          "end": { "character": 6, "line": 5 },
          "start": { "character": 2, "line": 5 }
        }
      }
    }
    |}]
;;

let%expect_test "completes from a module" =
  let source = {ocaml|let f = List.m|ocaml} in
  let position = Position.create ~line:0 ~character:14 in
  let%map () = print_completions source position in
  [%expect
    {| No completions |}]
;;

let%expect_test "completes a module name" =
  let source = {ocaml|let f = L|ocaml} in
  let position = Position.create ~line:0 ~character:9 in
  let%map () = print_completions ~pre_print:(List.take 5) source position in
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  (monitor.ml.Error (Failure "list shorter than n")
    ("Raised at Stdlib.failwith in file \"stdlib.ml\", line 29, characters 17-33"
      "Called from Ocaml_lsp_e2e__Test.Import.List.take in file \"lsp/ocaml-lsp-server/test/e2e-new/test.ml\", line 28, characters 17-30"
      "Called from Ocaml_lsp_e2e__Completion.print_completions.(fun) in file \"lsp/ocaml-lsp-server/test/e2e-new/completion.ml\", lines 35-36, characters 6-18"
      "Called from Fiber__Core.O.(>>|).(fun) in file \"fiber/src/core.ml\", line 250, characters 36-41"
      "Called from Fiber__Scheduler.exec in file \"fiber/src/scheduler.ml\", line 73, characters 8-11"
      "Re-raised at Stdune__Exn.raise_with_backtrace in file \"otherlibs/stdune/src/exn.ml\" (inlined), line 38, characters 27-56"
      "Called from Stdune__Exn_with_backtrace.reraise in file \"otherlibs/stdune/src/exn_with_backtrace.ml\", line 20, characters 33-71"
      "Called from Fiber__Scheduler.advance in file \"fiber/src/scheduler.ml\", line 195, characters 2-58"
      "Called from Fiber_async.deferred_of_fiber.loop.(fun) in file \"lsp/fiber-async/src/fiber_async.ml\", line 49, characters 19-61"
      "Caught by monitor block_on_async"))
  Raised at Base__Result.ok_exn in file "src/result.ml" (inlined), line 279, characters 17-26
  Called from Async_unix__Thread_safe.block_on_async_exn in file "src/thread_safe.ml", line 163, characters 29-63
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28
  |}]
;;

let%expect_test "completion doesn't autocomplete record fields" =
  let source =
    {ocaml|
    type r = {
      x: int;
      y: string
    }

    let _ =
  |ocaml}
  in
  let position = Position.create ~line:5 ~character:8 in
  let%map () =
    print_completions
      ~pre_print:
        (List.filter ~f:(fun (compl : CompletionItem.t) ->
           compl.label = "x" || compl.label = "y"))
      source
      position
  in
  (* We expect 0 completions*)
  [%expect {| No completions |}]
;;

let%expect_test "completion for `in` keyword - no prefix" =
  let source =
    {ocaml|
let foo param1 =
  let bar = param1 |ocaml}
  in
  let position = Position.create ~line:2 ~character:19 in
  let%map () = print_completions ~limit:3 source position in
  [%expect
    {|
    Completions:
    {
      "kind": 14,
      "label": "in",
      "textEdit": {
        "newText": "in",
        "range": {
          "end": { "character": 19, "line": 2 },
          "start": { "character": 19, "line": 2 }
        }
      }
    }
    {
      "detail": "'a -> 'b",
      "kind": 12,
      "label": "param1",
      "sortText": "0000",
      "textEdit": {
        "newText": "param1",
        "range": {
          "end": { "character": 19, "line": 2 },
          "start": { "character": 19, "line": 2 }
        }
      }
    }
    {
      "detail": "unit",
      "kind": 4,
      "label": "()",
      "sortText": "0001",
      "textEdit": {
        "newText": "()",
        "range": {
          "end": { "character": 19, "line": 2 },
          "start": { "character": 19, "line": 2 }
        }
      }
    }
    .............
    |}]
;;

let%expect_test "completion for `in` keyword - prefix i" =
  let source =
    {ocaml|
let foo param1 =
  let bar = param1 i
|ocaml}
  in
  let position = Position.create ~line:2 ~character:20 in
  let%map () = print_completions ~limit:3 source position in
  [%expect
    {|
    Completions:
    {
      "kind": 14,
      "label": "in",
      "textEdit": {
        "newText": "in",
        "range": {
          "end": { "character": 20, "line": 2 },
          "start": { "character": 19, "line": 2 }
        }
      }
    }
    {
      "detail": "type (+!'a : any separable) iarray : immutable_data with 'a",
      "kind": 25,
      "label": "iarray",
      "sortText": "0000",
      "textEdit": {
        "newText": "iarray",
        "range": {
          "end": { "character": 20, "line": 2 },
          "start": { "character": 19, "line": 2 }
        }
      }
    }
    {
      "detail": "type (!'a : value_or_null, +!'b : any) idx_imm : bits64 mod everything",
      "kind": 25,
      "label": "idx_imm",
      "sortText": "0001",
      "textEdit": {
        "newText": "idx_imm",
        "range": {
          "end": { "character": 20, "line": 2 },
          "start": { "character": 19, "line": 2 }
        }
      }
    }
    .............
    |}]
;;

let%expect_test "completion for `in` keyword - prefix in" =
  let source =
    {ocaml|
let foo param1 =
  let bar = param1 in
|ocaml}
  in
  let position = Position.create ~line:2 ~character:21 in
  let%map () = print_completions ~limit:3 source position in
  [%expect
    {|
    Completions:
    {
      "kind": 14,
      "label": "in",
      "textEdit": {
        "newText": "in",
        "range": {
          "end": { "character": 21, "line": 2 },
          "start": { "character": 19, "line": 2 }
        }
      }
    }
    {
      "detail": "type int : immediate",
      "kind": 25,
      "label": "int",
      "sortText": "0000",
      "textEdit": {
        "newText": "int",
        "range": {
          "end": { "character": 21, "line": 2 },
          "start": { "character": 19, "line": 2 }
        }
      }
    }
    {
      "detail": "type int16 : immediate",
      "kind": 25,
      "label": "int16",
      "sortText": "0001",
      "textEdit": {
        "newText": "int16",
        "range": {
          "end": { "character": 21, "line": 2 },
          "start": { "character": 19, "line": 2 }
        }
      }
    }
    .............
    |}]
;;
