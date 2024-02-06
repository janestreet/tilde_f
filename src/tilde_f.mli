open! Base

(** The main purpose of [Tilde_f] is make code with a lot of nested function arguments
    more readable. The following expression:

    {[
      E1 ~f:(fun w ->
        E2 ~f:(fun x ->
          E3 ~f:(fun y ->
            E4 ~f:(fun z ->
              E5))))
    ]}

    can be written:

    {[
      Tilde_f.run
        (let open Tilde_f.Let_syntax in
         let%bind w = E1 in
         let%bind x = E2 in
         let%bind y = E3 in
         let%bind z = E4 in
         return E5)
    ]} *)

(** A higher order function whose argument is labelled [~f]. Here are a few examples:

    {[
      val Deferred.bind : 'a Deferred.t -> ('a, 'b Deferred.t, 'b Deferred.t) Tilde_f.t
      val List.init : int -> (int, 'a list, int) Tilde_f.t
      val List.iter : 'a list -> ('a, unit, unit) Tilde_f.t
      val Option.value_map : 'a option -> default:'b -> ('a, 'b, 'b) Tilde_f.t
    ]} *)
type ('f_in, 'ret, 'f_out) t := f:('f_in -> 'f_out) -> 'ret

(** The [Tilde_f] monad is harder to explain than to demonstrate. See the documentation at
    the top of this module for an explanation of how to use it. *)
include Monad.S_indexed with type ('f_in, 'ret, 'f_out) t := ('f_in, 'ret, 'f_out) t

(** [run t] is [t ~f:Fn.id]. *)
val run : (f:('a -> 'a) -> 'b) -> 'b

(** Use [curried] to adapt a function that passes two arguments to its [~f]. For example:

    {[
      let%bind x, y = Tilde_f.of_curried (List.map2 lst1 lst2) in
      f x y
    ]} *)
val of_curried : (f:('a -> 'b -> 'c) -> 'd) -> f:('a * 'b -> 'c) -> 'd

(** Use [of_unlabeled] to adapt a function that would have the right type for [Tilde_f] if
    only its function argument was labeled. For example:

    {[
      val Tilde_f.of_unlabeled Or_error.try_with : (unit, 'a Or_error.t, 'a) Tilde_f.t
      val Tilde_f.of_unlabeled Deferred.create : ('a Ivar.t, 'a Deferred.t, unit) Tilde_f.t
    ]} *)
val of_unlabeled : (('a -> 'b) -> 'c) -> f:('a -> 'b) -> 'c

(** Use [of_local] to adapt a function that would have the right type for [Tilde_f] except
    that its parameter is local. *)
val of_local : (f:('a -> 'b) -> 'c) -> f:('a -> 'b) -> 'c

(** Use [of_local_k] to adapt a function that would have the right type for [Tilde_f.bind]
    except that its parameter is local. ([k] is for Kleisli, which refers to a function
    whose type is of the form ['a -> 'b M.t].) *)
val of_local_k : ('a -> f:('b -> 'c) -> 'd) -> 'a -> f:('b -> 'c) -> 'd
