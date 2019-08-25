open Mirage

let packages = [
  package "duration";
  package "logs";
  package "lwt_ppx";
  package "cohttp";
  package "cohttp-mirage";
  package "mirage-conduit";
]

let main =
  foreign
    "Unikernel.Main"
    ~packages
    (http @-> job)

let stack =
  socket_stackv4 [Ipaddr.V4.any]

let http_srv = cohttp_server @@ conduit_direct stack

let () =
  register "vxrrks" [main $ http_srv]
