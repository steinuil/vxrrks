open Mirage

let packages = [
  package "duration";
  package "lwt_ppx";
]

let main =
  foreign
    "Unikernel.Main"
    ~packages
    (time @-> job)

let () =
  register "vxrrks" [main $ default_time]
