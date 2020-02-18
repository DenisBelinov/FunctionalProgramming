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

moveChildren parents family g =
  | children == newChildren = children
  | (1<2) = moveChildren newChildren newFamily g
  where
    parents = foldl union [] map (%g) children
    newChildren = union children parents
    newFamily = family \\ newChildren


moveParents children family g =
  | children == newChildren = children
  | (1<2) = moveChildren newChildren newFamily g
  where
    parents = foldl union [] map (%g) children
    newChildren = union children parents
    newFamily = family \\ newChildren

-- use this generic vatiant with (%) and (#) for func
move f1 f2 g func =
  | f1 == newf1 = f1
  | (1<2) = move newf1 newf2 g ??? 
  where
     toMove = foldl union []  (map (func g) f1)
     newf1 = union f1 toMove
     newf2 = f2 \\ toMove

splitFamily fam g =
  where
    randomMember = head fam
    children = randomMember#g
