module type SERVER = Cohttp_lwt.S.Server

module Make (Cohttp_server : SERVER) = struct
  let router path =
    Logs.info (fun f -> f "%s" path);
    let path = path |> String.split_on_char '/' |> List.tl in
    match path with
    | [""] | ["index.html"] ->
      "tfw"
    | path ->
      String.concat ":" path

  let make () =
    let callback _conn req _body =
      let path = Cohttp.Request.uri req |> Uri.path in
      let%lwt res, body =
        Cohttp_server.respond_string ~status:`OK ~body:(router path) ()
      in
      Lwt.return @@ `Response (res, body)
    in
    let conn_closed _ = () in
    Cohttp_server.make_response_action ~callback ~conn_closed ()
end
