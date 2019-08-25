module type SERVER = Cohttp_lwt.S.Server

module Make (Cohttp_server : SERVER) = struct
  let make () =
    let callback _conn _req _body =
      let%lwt res, body =
        Cohttp_server.respond_string ~status:`OK ~body:"ayy" ()
      in
      Lwt.return @@ `Response (res, body)
    in
    let conn_closed _ = () in
    Cohttp_server.make_response_action ~callback ~conn_closed ()
end
