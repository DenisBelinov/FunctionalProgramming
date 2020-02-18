import Data.Function

areOverlapping 0 _ = False
areOverlapping _ 0 = False
areOverlapping x y = on (&&) (on (==) (`rem` 2) 1) x y || on areOverlapping ((flip div) 2) x y

h 0 = 0
h n = head [ x | x <- [0..], not (any (\k -> (areOverlapping k n) && ((h k) == x)) [0..n-1]) ]

sigert = map h [1..]


--areOverlapping x y = ((&&) ((==) (rem x 2) 1) ((==) (rem y 2) 1)) || (areOverlapping (div x 2) (div y 2))
