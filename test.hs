main = print (repartition 7 3 1 1 0)

repartition :: Int -> Int -> Int -> Int -> Int -> (Bool, [Int])
repartition nbP n a b m
	| sub > 0 = let (ok, list) = repartition sub n a b m in if ok then (ok, nb:list) else let (ok2, list2) = decrease in if ok2 then (ok2, nb:list2) else increase
	| sub == 0 = (True, [nb])
	| otherwise = decrease
	where
		nb = n-m
		sub = nbP-nb
		decrease = if m<b && nb>2 then (repartition nbP n a b (m+1)) else (False, [])
		increase = if m<a && nb>2 then (repartition nbP n a b (m-1)) else (False, [])
