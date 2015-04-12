import Data.List

-- "terminal"
-- Eps
-- <Nonterminal>
-- <Production> -> <Y>"terminal"
data Node = Term String | Eps | NonTerm String | Prod String [Node]

instance Show Node where 
    show (Term x) = x
    show (Eps) = "eps"
    show (NonTerm x) = x
    show (Prod x ns) = x ++ ", [" ++ foldr (++) "" ((map (show) ns)) ++ "]"


getProdName :: Node -> String
getProdName (Prod name _) = name
getProdName _ = ""


-- The Language --

prods = [stmt, stmt1, var, var1, var2, num]
stmt = Prod "if" [Term "if", NonTerm "cond", Term "then", NonTerm "stmt", Term "else", NonTerm "stmt"]
stmt1 = Prod "assign" [NonTerm "var", Term "->", NonTerm "num"] 
var = Prod "var" [Term "x"]
var1 = Prod "var" [Term "y"]
var2 = Prod "var" [Eps]
num = Prod "num" [Term "1"]

main :: IO()
main = do print (nullable prods stmt)

-- Get nonterminal productions --
resolve :: [Node] -> Node -> [Node]
resolve (p:ps) v@(NonTerm x) | x == getProdName p = [p] ++ resolve ps v 
                             | otherwise = resolve ps v
resolve _ _ = []

-- Is symbol nullable --
nullable :: [Node] -> Node -> Bool
nullable _ Eps = True
nullable _ (Term _) = False
nullable t n@(NonTerm x) = foldr (||) False (map (nullable t) ps)
    where ps = resolve t n
nullable t p@(Prod x (n:ns)) = nullable t n

-- first set of list of symbols --
first' :: [Node] -> [Node] -> [String]
first' t (n:ns) | nullable t n = union (first t n \\ ["eps"]) (first' t ns)
                | otherwise = first t n
first' t [] = ["eps"]

-- first set of symbol --
first :: [Node] -> Node -> [String]
first _ Eps = ["eps"]
first _ (Term x) = [x]
first t n@(NonTerm x) = foldr (union) [] (map (first t) ps)
    where ps = resolve t n
first t p@(Prod x c@(n:ns)) | nullable t p = (first' t c) 
                            | otherwise = first t n

