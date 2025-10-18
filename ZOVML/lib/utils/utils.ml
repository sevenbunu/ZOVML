module BASEMONAD = struct
  let return x st = st, Result.Ok x
  let fail e st = st, Result.Error e

  let ( >>= ) x f st =
    let st1, x1 = x st in
    match x1 with
    | Result.Ok x -> f x st1
    | Result.Error x -> fail x st1
  ;;

  let ( >>| ) x f = x >>= fun a -> return @@ f a
  let ( let* ) = ( >>= )
  let read st = return st st
  let write st_new _ = st_new, Result.Ok ()
  let run f = f
end
