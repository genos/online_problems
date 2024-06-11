open Base

type 'a kind =
  | Input
  | Comp1 of ('a -> 'a) * 'a cell ref
  | Comp2 of ('a -> 'a -> 'a) * 'a cell ref * 'a cell ref

and 'a cell = {
  kind : 'a kind;
  value : 'a ref;
  eq : 'a -> 'a -> bool;
  callbacks : ('a -> unit) Hashtbl.M(Int).t;
  touches : 'a cell list ref;
}

type callback_id = int

let id_counter = ref 0

let create ~kind ~value ~eq =
  { kind; value = ref value; eq; callbacks = Hashtbl.create (module Int); touches = ref [] }

let create_input_cell ~value ~eq = create ~kind:Input ~value ~eq

let rec value_of { kind; value; _ } =
  match kind with
  | Input -> !value
  | Comp1 (f, c1) -> f (value_of !c1)
  | Comp2 (f, c1, c2) -> f (value_of !c1) (value_of !c2)

let rec fire c =
  let v = value_of c in
  if not (c.eq v c.value.contents) then (
    c.value := v;
    Hashtbl.iter c.callbacks ~f:(fun k -> k v);
    List.iter c.touches.contents ~f:fire)

let set_value { value; touches; eq; _ } v =
  if not (eq v !value) then (
    value := v;
    List.iter !touches ~f:fire)

let create_compute_cell_1 c1 ~f ~eq =
  let c2 = create ~kind:(Comp1 (f, ref c1)) ~value:(f (value_of c1)) ~eq in
  c1.touches := c2 :: c1.touches.contents;
  c2

let create_compute_cell_2 c1 c2 ~f ~eq =
  let c3 = create ~kind:(Comp2 (f, ref c1, ref c2)) ~value:(f (value_of c1) (value_of c2)) ~eq in
  c1.touches := c3 :: c1.touches.contents;
  c2.touches := c3 :: c2.touches.contents;
  c3

let add_callback { callbacks; _ } ~k =
  let key = !id_counter in
  Hashtbl.add_exn callbacks ~key ~data:k;
  id_counter := Int.succ !id_counter;
  key

let remove_callback c k = Hashtbl.remove c.callbacks k
