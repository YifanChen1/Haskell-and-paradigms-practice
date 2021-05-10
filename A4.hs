module A4 where

--q1
data Poly = PConst Integer | PVar | PAdd Poly Poly | PMul Poly Poly

compilePoly :: Poly -> (Integer -> Integer)
compilePoly (PConst x) = (\_ -> x)
compilePoly PVar = id
compilePoly (PAdd p1 p2) = (\x -> (compilePoly p1 x) + (compilePoly p2 x))
compilePoly (PMul p1 p2) = (\x -> (compilePoly p1 x) * (compilePoly p2 x))


--q4
runningSums :: [Integer] -> [Integer]
runningSums list = a where a = 0: zipWith (+) list a