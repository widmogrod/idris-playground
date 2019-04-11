module DT

%default total

data Num
  = Z
  | S Num

interface Shhow a where
  show: a -> String

Shhow Num where
  show Z = "z"
  show (S k) = "s" ++ show k

add : Num -> Num -> Num
add Z y = y
add (S x) y = S (add x y)

even : Num -> Bool
even Z = True
even (S x) = even x

data Vect : Nat -> Type -> Type where
   Nil  : Vect Z a
   (::) : a -> Vect k a -> Vect (S k) a


Shhow a => Shhow (Vect n a) where
  show xs = "[" ++ show' xs ++ "]" where
    show' : Vect n a -> String
    show' Nil = ""
    show' (x :: Nil) = show x
    show' (x :: xs) = show x ++ "," ++ show' xs

testList' : Vect 2 Num
testList' = Z :: S(S(Z)) :: Nil

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
r = index (FS(FZ)) testList

data T : Nat -> Type where
  D : Bool -> Bool -> T a

e1 : T n
e1 = D True False

data DNat : Nat -> Type where
  DZ : DNat n

testNumber'' : DNat 2
testNumber'' = ?asd
