module T = Mirage_types_lwt
module type HTTP = Cohttp_lwt.S.Server

module Main (Http : HTTP) = struct
  module Server = Vxrrks_server.Server.Make(Http)

  let start http =
    let tcp = `TCP 8080 in
    http tcp @@ Server.make ()
end
