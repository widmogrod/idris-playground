data Num = Z | S Num
add : Num -> Num -> Num
add Z y = y
add (S x) y = S (add x y)

even : Num -> Bool
even Z = True
even (S x) = even x

data Vect : Nat -> Type -> Type where
   Nil  : Vect Z a
   (::) : a -> Vect k a -> Vect (S k) a

testList : Vect 2 Nat
testList = 1 :: 2 :: Nil

data Fin : Nat -> Type where
  FZ : Fin (S x)
  FS : Fin n -> Fin (S n)

index : Fin n -> Vect n a -> a
index FZ     (x :: xs) = x
index (FS k) (x :: xs) = index k xs

-- index (FS(FZ)) testList


data T : Nat -> Type where
  A : T a
  B : Bool -> T a
  C : T k -> T k
  D : Bool -> Bool -> T a
