import Data.List

-- "terminal"
-- Eps
-- <Nonterminal>
-- <Production> -> <Y>"terminal"
data Node = Term String | Eps | NonTerm String | Prod String [Node]

instance Show Node where 
    show (Term x) = x
    show (Eps) = "eps"
    show (NonTerm x) = "<"++ x ++">"
    show (Prod x ns) = x ++ ", [ " ++ foldr (\x y -> x++" "++y) "" ((map (show) ns)) ++ "]"

instance Eq Node where
    n1 == n2 = (getNodeValue n1) == (getNodeValue n2)

getProdName :: Node -> String
getProdName (Prod name _) = name
getProdName _ = ""

getNodeValue :: Node -> (Int, String)
getNodeValue (Eps) = (0, "eps")
getNodeValue (Term x) = (1, x)
getNodeValue (NonTerm x) = (2, x)
getNodeValue (Prod x ns) = (2, x)

-- The Language --
prods = [top, stmt, stmt1, var, var1, var2, num, cond, cond1]
top = Prod "s" [NonTerm "stmt"]
stmt = Prod "stmt" [Term "if", NonTerm "cond", Term "then", NonTerm "stmt", Term "else", NonTerm "stmt"]
stmt1 = Prod "stmt" [NonTerm "var", Term "=", NonTerm "num"]
stmts = Prod "stmts" [NonTerm "stmt", Term ";", NonTerm "stmt"]
var = Prod "var" [Term "x"]
var1 = Prod "var" [Term "y"]
var2 = Prod "var" [Eps]
num = Prod "num" [Term "1"]
cond = Prod "cond" [Term "TRUE"]
cond1 = Prod "cond" [Term "FALSE"]

main :: IO()
main = do print (nullable prods stmt)

-- Get nonterminal productions --
resolve :: [Node] -> Node -> [Node]
resolve (p:ps) v@(NonTerm x) | x == getProdName p = [p] ++ resolve ps v 
                             | otherwise = resolve ps v
resolve _ _ = []

-- Are symbols nullable
nullable' :: [Node] -> [Node] -> Bool
nullable' t s = foldr (&&) True (map (nullable t) s) 

-- Is symbol nullable --
nullable :: [Node] -> Node -> Bool
nullable _ Eps = True
nullable _ (Term _) = False
nullable t n@(NonTerm x) = let ps = (resolve t n) in foldr (||) False (map (nullable t) ps)
nullable t p@(Prod x (ns)) = foldr (&&) True (map (nullable t) ns)

-- First set of list of symbols --
first' :: [Node] -> [Node] -> [String]
first' t (n:ns) | nullable t n = union (first t n \\ ["eps"]) (first' t ns)
                | otherwise = first t n
first' t [] = ["eps"]

-- First set of symbol --
first :: [Node] -> Node -> [String]
first _ Eps = ["eps"]
first _ (Term x) = [x]
first t n@(NonTerm x) = let ps = (resolve t n) in foldr (union) [] (map (first t) ps)
first t p@(Prod x c@(n:ns)) = (first' t c) 


-- Follow set for production --
follow' :: [Node] -> Node -> Node -> [String]
follow' t n p@(Prod m (n1:[])) | n == n1 && n /= p = follow t p
                               | otherwise = []
follow' t n p@(Prod m (n1:n2:ns)) | n == n1 && (not (nullable t n2)) = union (first t n2) (follow' t n (Prod m (n2:ns)))
                                  | n == n1 && nullable t n2 = union ((first t n2) \\ ["eps"]) (follow' t n (Prod m (n2:ns)))
                                  | otherwise = follow' t n (Prod m (n2:ns))

-- Follow set across all productions -- 
follow :: [Node] -> Node -> [String]
follow t@(p:ps) n = foldr (union) [] (map ((follow' t) n) t) 
follow _ _ = []


select :: [Node] -> Node -> [String]
select t p@(Prod m c@(n:ns)) | nullable' t c = union (first' t c \\ ["eps"]) (follow t p)
                             | otherwise = first' t c
select _ _ = error "not a production"


