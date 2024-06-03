open Base

type allergen =
  | Eggs
  | Peanuts
  | Shellfish
  | Strawberries
  | Tomatoes
  | Chocolate
  | Pollen
  | Cats

let a2i = function
  | Eggs -> 1
  | Peanuts -> 2
  | Shellfish -> 4
  | Strawberries -> 8
  | Tomatoes -> 16
  | Chocolate -> 32
  | Pollen -> 64
  | Cats -> 128

let ashift = Fn.compose Int.floor_log2 a2i
let allergic_to n a = 1 = Int.shift_right (Int.bit_and n @@ a2i a) @@ ashift a

let allergies n =
  List.filter ~f:(allergic_to n)
    [
      Eggs; Peanuts; Shellfish; Strawberries; Tomatoes; Chocolate; Pollen; Cats;
    ]
