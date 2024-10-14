namespace TensorLib

def hello := "world"

class TensorElement (a: Type) where
  add: a -> a -> a
  -- etc. enough to implement numpy ops

inductive TensorData (a: Type) [TensorElement a] where
| td_root (x: List a)
| td_node (tl: List (TensorData a))

-- Awkward... I'm sure this exists somewhere in the stdlib
def sequence {a: Type} (x: List (Option a)): Option (List a) :=
  match x with
  | .nil => some .nil
  | .cons none _ => none
  | .cons (some y) ys =>
    match sequence ys with
    | none => none
    | some ys' => some (.cons y ys')

-- somehow prove termination here
def Shape {a: Type} [TensorElement a] (x: TensorData a): Option (List Nat) :=
  match x with
  | TensorData.td_root x => some [List.length x]
  | TensorData.td_node xs =>
    match sequence (.map Shape xs) with
    | none => none
    | some .nil => some .nil
    | some (.cons x xs) =>
      if List.all xs (fun x' => x == x') then
        some (List.cons (1 + List.length xs) x)
      else
        none


structure BFloat16 where
  data: Float

instance : TensorElement BFloat16 where
  add b1 b2 := BFloat16.mk (b1.data + b2.data) -- todo: do the correct math

structure Float16 where
  data: Float

instance : TensorElement Float16 where
  add b1 b2 := Float16.mk (b1.data + b2.data) -- todo: do the correct math

structure Float32 where
  data: Float

instance : TensorElement Float32 where
  add b1 b2 := Float32.mk (b1.data + b2.data) -- todo: do the correct math

structure Float64 where
  data: Float

instance : TensorElement Float64 where
  add b1 b2 := Float64.mk (b1.data + b2.data) -- todo: do the correct math

structure Int8 where
  data: UInt8

instance : TensorElement Int8 where
  add b1 b2 := Int8.mk (b1.data + b2.data) -- todo: do the correct math

structure Int16 where
  data: UInt16

instance : TensorElement Int16 where
  add b1 b2 := Int16.mk (b1.data + b2.data) -- todo: do the correct math

structure Int32 where
  data: UInt32

instance : TensorElement Int32 where
  add b1 b2 := Int32.mk (b1.data + b2.data) -- todo: do the correct math

structure Int64 where
  data: UInt8

instance : TensorElement Int64 where
  add b1 b2 := Int64.mk (b1.data + b2.data) -- todo: do the correct math


#eval (fun x => x + 5) 7

#eval (List.map UInt64.mk [1, 2, 3])

#eval Shape (TensorData.td_root (List.map Int8.mk [1, 2, 3]))

#eval Shape (
  TensorData.td_node [
    (TensorData.td_root (List.map Int8.mk [1, 2, 3])),
    (TensorData.td_root (List.map Int8.mk [1, 2, 3])),
  ])

end TensorLib
