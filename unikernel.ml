module T = Mirage_types_lwt

module Main (Time : T.TIME) = struct
  let start _time =
    let rec loop () =
      Logs.info (fun f -> f "soap");
      Time.sleep_ns (Duration.of_sec 1);%lwt
      loop ()
    in
    loop ()
end
