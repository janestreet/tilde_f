open! Base

include Monad.Make_indexed (struct
  type ('a, 'i, 'j) t = f:('a -> 'j) -> 'i

  let return x ~f = f x
  let bind t ~f:bind_f ~f = t ~f:(fun x -> bind_f x ~f)
  let map t ~f:map_f ~f = t ~f:(fun x -> f (map_f x))
  let map = `Custom map
end)

let run t = t ~f:Fn.id
let of_curried t ~f = t ~f:(fun a b -> f (a, b))
let of_unlabeled t ~f = t f
let of_local t = (t : f:('a -> 'b) -> 'c :> f:('a -> 'b) -> 'c)
let of_local_k f = (f : 'a -> f:('b -> 'c) -> 'd :> 'a -> f:('b -> 'c) -> 'd)
