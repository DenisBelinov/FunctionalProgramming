type Purchase = (String, String, Double)

--averagePrices :: [Purchase] -> [Purchase]
averagePrices [] = []
averagePrices l@((shop, cat, price):xs) = (shop, cat, averagePrice) : (averagePrices restOfPurchases)
  where currentPurchases = filter (\(a,b,c) -> (a == shop) && (b == cat)) l
        averagePrice = (averageBy (\(a,b,c) -> c) currentPurchases)
        restOfPurchases = filter (\(a,b,c) -> (a /= shop) || (b /= cat)) l

averageBy :: (Purchase -> Double) -> [Purchase] -> Double
averageBy _ [] = 0
averageBy f xs = (sum (map f xs)) / (fromIntegral (length xs))
