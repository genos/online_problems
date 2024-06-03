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

let allergic_to n a = 0 != Int.logand n @@ a2i a

let allergies n =
  List.filter (allergic_to n)
    [
      Eggs; Peanuts; Shellfish; Strawberries; Tomatoes; Chocolate; Pollen; Cats;
    ]
