(* -*- tuareg -*- *)
{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
}}

module Synted_app =
  Eliom_registration.App (
    struct
      let application_name = "synted"
    end)

let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()

let () =
  Synted_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"synted"
           ~css:[["css";"synted.css"]]
           Html5.F.(body [
             h2 [pcdata "Welcome from Eliom's distillery!"];
           ])))
