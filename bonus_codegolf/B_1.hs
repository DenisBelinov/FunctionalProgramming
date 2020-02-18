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

cfh :: Graph -> ([Node] -> [Node])

--l=isFamily

maxAvoiding f u g
  |r==[]
  where
    r = union u#g u%g


tree = Map.fromList [(1, [2,3]), (2, [4]), (3, []), (4, []), (5, [6]), (6, [])]
