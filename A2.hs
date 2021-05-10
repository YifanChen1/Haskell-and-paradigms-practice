module A2 where
--Yifan Chen Fall 2020 CPSC449
--30073072
type Poly = [Integer]


addpoly :: Poly -> Poly -> Poly
addpoly [] [] = []
addpoly a [] = a
addpoly [] b = b
addpoly (x:xs) (y:ys) = (x + y): (addpoly xs ys)

multpoly :: Poly -> Poly -> Poly
multpoly [] [] = []
multpoly x [] = []
multpoly [] ys = []
multpoly (x:xs) ys = addpoly ([x*n| n <- ys]) (0: multpoly xs ys)


mergeLists :: [Integer] -> [Integer] -> [Integer]
mergeLists x [] = x
mergeLists [] xs = xs
mergeLists (x:xs) (y:ys)
    |x < y = [x] ++ (mergeLists xs (y:ys))
    |otherwise = [y] ++ (mergeLists (x:xs) ys)


splitList :: [Integer] -> ([Integer], [Integer])
splitList [] = ([],[])
splitList [x] = ([x],[]) 
splitList x = ([b| (b,i) <- zip x [0..], even i],[a| (a,i) <- zip x [0..], odd i])

mSort :: [Integer] -> [Integer]
mSort [] = []
mSort [x] = [x]
mSort x = mergeLists (mSort a) (mSort b)
    where (a, b) = splitList x