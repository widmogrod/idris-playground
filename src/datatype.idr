%default total

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


data Fin : Nat -> Type where
  FZ : Fin (S x)
  FS : Fin n -> Fin (S n)

-- index : Fin n -> Vect n a -> a
index : {a:Type} -> {n:Nat} -> Fin n -> Vect n a -> a
index FZ     (x :: xs) = x
index (FS k) (x :: xs) = index k xs

testList : Vect 2 Nat
testList = 1 :: 2 :: Nil

r : Nat
-- Not working example, because list does not have "3" elements
-- r = Main.index (FS(FS(FZ))) testList
-- Works, because list has two elemnts
r = Main.index (FS(FZ)) testList

data T : Nat -> Type where
  D : Bool -> Bool -> T a

e1 : T n
e1 = D True False

data DNat : Nat -> Type where
  DZ : DNat n

testNumber'' : DNat 2
testNumber'' = ?asd
