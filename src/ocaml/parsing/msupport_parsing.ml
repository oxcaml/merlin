(* Filled in from Msupport. *)
let msupport_raise_error : (exn -> unit) ref = ref (fun x -> raise x)

let raise_error exn = !msupport_raise_error exn
