import Data.List
i s n k=2*s!!(n-k)-s!!(n-2*k)
f s n=([1..]\\(i s n<$>[1..div n 2]))!!0

s =f s<$>[0..]
forestFire=s
