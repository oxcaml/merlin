open Async
open! Test.Import

let path = Filename.concat (Sys.getcwd ()) "for_ppx.ml"
let uri = DocumentUri.of_path path

let print_hover hover =
  match hover with
  | None -> print_endline "no hover response"
  | Some hover ->
    hover |> Hover.yojson_of_t |> Yojson.Safe.pretty_to_string ~std:false |> print_endline
;;

let hover_req client position =
  Client.request
    client
    (TextDocumentHover
       { HoverParams.position
       ; textDocument = TextDocumentIdentifier.create ~uri
       ; workDoneToken = None
       })
;;

let%expect_test "with-ppx" =
  (* We will call 'hover' on the last line of this very file *)
  let position = Position.create ~line:2 ~character:5 in
  (* We need to wait for the first diagnostics *)
  let diagnostics = Fiber.Ivar.create () in
  let handler =
    let on_notification (_ : _ Client.t) (n : Client.in_notification) =
      match n with
      | PublishDiagnostics diag ->
        printfn "Received %i diagnostics" (List.length diag.diagnostics);
        List.iter diag.diagnostics ~f:(fun (d : Diagnostic.t) ->
          match d.message with
          | `String m -> print_endline m
          | `MarkupContent _ -> assert false);
        Fiber.Ivar.fill diagnostics ()
      | _ -> Fiber.return ()
    in
    Client.Handler.make ~on_notification ()
  in
  let%map output =
    Test.run ~handler
    @@ fun client ->
    let run_client () =
      let capabilities = ClientCapabilities.create () in
      Client.start client (InitializeParams.create ~capabilities ())
    in
    let run () =
      let* (_ : InitializeResult.t) = Client.initialized client in
      let textDocument =
        let text = Io.String_path.read_file path in
        TextDocumentItem.create ~uri ~languageId:"ocaml" ~version:0 ~text
      in
      let* () =
        Client.notification
          client
          (TextDocumentDidOpen (DidOpenTextDocumentParams.create ~textDocument))
      in
      let* () = Fiber.Ivar.read diagnostics in
      let* () =
        let+ resp = hover_req client position in
        print_hover resp
      in
      let output = [%expect.output] in
      let* () = Client.request client Shutdown in
      let+ () = Client.stop client in
      output
    in
    Fiber.fork_and_join_unit run_client run
  in
  let (_ : string) = [%expect.output] in
  ignore output
  (*= print_endline output;
  [%expect
    {xxx|
    [1mFile "/tmp/build_46f993_dune/camlppx1117cb", line 1[0m:
    [1;31mError[0m: The input is a binary ast for an unknown version of OCaml with magic number 'Caml1999M577'
    Received 1 diagnostics
    [1mFile "/tmp/build_46f993_dune/camlppx320abb", line 1[0m:
    [1;31mError[0m: The input is a binary ast for an unknown version of OCaml with magic number 'Caml1999M577'
    /usr/local/home/lstevenson/github/merlin-jst/lsp-and-merlin-minus-37/_build/default/lsp/ocaml-lsp-server/test/e2e-new/.ocaml_lsp_e2e.objs/byte/ocaml_lsp_e2e.cmi
    seems to be compiled with a version of OCaml (with magic number Caml1999I036) that is not supported by Merlin.
    This instance of Merlin handles OCaml 5.2.0minus-37 (with magic number Caml1999I577).
    {
      "contents": {
        "value": "(* ppx expect expansion *)\n[%expect {| |}]",
        "language": "ocaml"
      },
      "range": {
        "end": { "character": 17, "line": 2 },
        "start": { "character": 2, "line": 2 }
      }
    }
    |xxx}] *)
;;
