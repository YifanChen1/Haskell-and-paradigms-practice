--CPSC449 FALL 2020
--T04 Student id: 30073072
--Yifan Chen

module Assignment1 where
import Test.QuickCheck hiding (scale)
import Data.List
--q1
-- intakes 2 parameters, the first of which is the base (b)
-- the second argument is the number whos log is to be computed
-- returns the largest non negative integer y such that b^y = x
myLog :: Integer -> Integer -> Integer
myLog x y
    | x > y = 0
    | x == y = 1
    | otherwise = (1 + myLog x (div y x))

--q2

type Person = String
type Book = String

type Database = [(Person, [Book])]

dBase :: Database
dBase = [ ("Alice", ["Brave New World"]),
          ("Bob", ["Nineteen Eighty Four","Brave New World"]),
          ("Carol", ["The Handmaid's Tale"])
        ]

--takes database and person, and outputs the list of books they have borrowed
books :: Database -> Person -> [Book]
books db per
    | [b | (p, b) <- db, p == per] == [] = []
    | otherwise = head([b | (p, b) <- db, p == per])

--takes database and books, and outputs the list of people with that book
borrowers :: Database -> Book -> [Person]
borrowers db bk = [p | (p, b) <- db, elem bk b]

--takes database and book, and returns if a book is currently borrowed
borrowed :: Database -> Book -> Bool
borrowed db bk = (borrowers db bk) /= []

--takes database and person, and returns how many books that person is currently loaning
numBorrowed :: Database -> Person -> Int
numBorrowed db per = length(books db per)

--removes a person from the database
removeInfo :: Database -> Person -> Database
removeInfo db per = [(p,b) | (p,b) <- db, p /= per]

--takes database, person, book and loans a book. If the person has already borrowed the book, do not allow them to loan again.
--if no previous info on a person, make a new database of [(person, [book])
makeLoan :: Database -> Person -> Book -> Database
makeLoan db per bk
    | elem per (borrowers db bk) == True = db
    | numBorrowed db per == 0 = db ++ [(per,[bk])]
    | otherwise = removeInfo db per ++ [(per, books db per ++ [bk])]

--takes database, person and book, if the person owns the book, remove it from their list, if that was the only book they owned, remove them from the database.
--if no such books were loaned, no nothing.
returnLoan :: Database -> Person -> Book -> Database
returnLoan db per bk
    | numBorrowed db per == 0 = db
    | borrowers db bk == [] = db
    |(numBorrowed db per == 1)  && (elem per (borrowers db bk)) = removeInfo db per
    | otherwise = removeInfo db per ++ [(per, delete bk (books db per))]

--q3
--takes a picture p, and scales it by second argument, n
type Picture = [[Char]]

p :: Picture
p = [['#','.','#'],['.','.','#']]

scale :: Picture -> Integer -> Picture
scale p n 
    | n == 0 = [[]]
    | otherwise = concat([replicate(fromIntegral n) (helper1 c n)|c <- p])

helper1 :: [Char] -> Integer -> [Char]
helper1 p n = concat([replicate(fromIntegral n) c|c <- p])


--q4
--finds the list of common friends in social graph, list of pairs, g
type Graph = [(Integer, Integer)]
g :: Graph
g = [(1,3), (4,2), (4,1), (2,3),(1,5)]

--gets all the people A is friends with
friendswithA :: Graph -> Integer -> [Integer]
friendswithA graph person = [friends | (a, friends) <- graph, person == a] ++ [friends | (friends, a) <- graph, person == a]

--gets all the people b is friends with
friendswithB :: Graph -> Integer -> [Integer]
friendswithB graph person = [friends | (a, friends) <- graph, person == a] ++ [friends | (friends, a) <- graph, person == a]

--find the intersecting numbers between a and b's friends
commonFriends :: Graph -> Integer -> Integer -> [Integer]
commonFriends g a b = intersect (friendswithA g a) (friendswithB g b)