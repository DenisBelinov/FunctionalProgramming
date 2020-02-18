import Data.Map (Map,filter,(!),member,keys)
import Data.List (union,(\\),sort,nub,sortBy)
import qualified Data.Map as Map

type Node = Integer
type Graph = Map Node [Node]

n#g
  | (member n g)= g ! n
  | 1<2=[]

n%g=keys (Map.filter (elem n) g)

c n g h = let
  allp= all (`elem` n) parents
  allc= all (`elem` n) children
  nop= not (any (`elem` n) parents)
  noc= not (any (`elem` n) children)
  in (allp && noc) || (allc && nop)
  where
   parents=h%g
   children=h#g

isFamily n g = all ((n`c`)g) n

cfh :: Graph -> (([Node],[Node]) -> [Node])

(?)=isFamily

cfh g (p,c)
 |((p,c)==(p1,c1)) = union p c
 | 1<2=cfh g (p1,c1)
 where
  p1=union p (foldl union [] (map (%g) c))
  c1=union c (foldl union [] (map (#g) p))

minIncluding n g
 |f1?g && f2?g = if((length f1) < (length f2)) then f1 else f2
 |f1?g = f1
 |f2?g = f2
 |1<2=[]
 where
  f1= cfh g ([n],[])
  f2= cfh g ([],[n])

tree = Map.fromList [(1, [2,3]), (2, [4]), (3, []), (4, [])]
