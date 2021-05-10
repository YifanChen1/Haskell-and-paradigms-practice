--q1

data Formula = Name String | Neg Formula | Conj (Formula) (Formula) | Disj (Formula) (Formula) deriving Show

--Q1b
e = Neg (Conj(Neg (Name "p")) (Neg (Name "q"))) 
fm = Conj (Name "x") (Neg (Disj (Name "y") (Neg (Name "z")) ))

showFormula :: Formula -> String
showFormula (Name f) = f
showFormula (Neg f) = "~" ++ showFormula f
showFormula (Conj f g) = "(" ++ showFormula f ++ " ^ " ++ showFormula g ++ ")"
showFormula (Disj f g) = "(" ++ showFormula f ++ " V " ++ showFormula g ++ ")"


rewrite :: Formula -> Formula
rewrite (Neg(Neg(f))) = f
rewrite (Neg(Conj f g)) = rewrite(Disj (rewrite(Neg f)) (rewrite(Neg g)))
rewrite (Neg(Disj f g)) = rewrite(Conj (rewrite(Neg f)) (rewrite(Neg g)))
rewrite f = f


--q3  3b broken and does not work

lastElem :: [a] -> a
lastElem = foldr1 (\_ a -> a)

unanimous :: [a -> Bool] -> a -> Bool
unanimous [] _ = True
--unanimous xs e = foldr1 (&&) (map xs e)
--unanimous (x:xs) e = head(map x e) && unanimous xs e
--unanimous (x:xs) e = map e x && unanimous e xs


selectiveMap :: (a->Bool) ->(a->b) ->[a] -> [b]
selectiveMap pred f xs = map f (filter pred xs)


--q4

double :: Integer -> Integer
double n = 2*n

iter :: Integer -> (a -> a) -> a -> a
iter 0 f x = x
iter n f x = iter(n-1) f (f x) 

powerOfTwo :: Integer -> Integer
powerOfTwo 0 = 1
powerOfTwo n = iter n double 1
