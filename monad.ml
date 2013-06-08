
module type MonadSig = sig

    type 'a t

    val bind : 'a t -> ('a -> 'b t) -> 'b t

    val return : 'a -> 'a t
end

module SuccessMonad  = struct

    type 'a t =
        | Success of 'a
        | Failure of string

    let bind m f =
        begin match m with
            | Success v -> f v
            | Failure msg -> Failure(msg)
        end

    let return v = Success(v)
end

module LazyMonad : MonadSig = struct
    type 'awesome t = unit -> 'awesome

    let return v =
        let ret = fun () -> v in
        ret

    let bind m f = fun () ->
      f (m ()) ()
end

open SuccessMonad
let ( >>= ) = bind
let ( >> ) m f = bind m begin fun _ -> f () end

let _ =
    let v =
        return "Hello"
        >>= fun m1 ->
        return "World"
        >>= fun m2 ->
        return (m1,m2)
    in
    begin match v with
        | Success(h,w) -> Printf.fprintf stderr "Success: %s %s!\n" h w
        | Failure(msg) -> Printf.fprintf stderr "FAILURE: %s\n" msg
    end

