import Data.List
s k l=sum[k|(a,b,c)<-l,a==s]

k s l=sum[1|(a,b,c)<-l,a==s]
g s l=sum[c|(a,b,c)<-l,a==s]
p s l=fst(minimumBy(\(_,a)(_,b)->compare a b)(map(\(a,b,c)->(b,abs(c-(g s l/fromIntegral(k s l)))))(filter(\(a,b,c)->a==s)l)))
r l=map(\x->(x,g x l,p x l))(nub(map(\(a,b,c)->a)l))

p1 = ("a", "cat1", 3.0)
p2 = ("a", "cat2", 9.0)
p3 = ("a", "cat3", 6.0)

p4 = ("b", "cat1", 10.0)
p5 = ("b", "cat2", 5.0)

ps = [p1, p2, p3, p4, p5]
