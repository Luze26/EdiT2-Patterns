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


repartitionUniform :: Int -> Int -> Int -> Int -> Int -> (Bool, [Int])
repartitionUniform nbP n a b m
	| m<=a && mod nbP nb == 0 = (True, replicate (div nbP nb) nb)
	| m<=b && nb>1 && mod nbP nb1 == 0 = (True, replicate (div nbP nb1) nb1)
	| m<a || m<b = repartitionUniform nbP n a b (m+1)
	| otherwise = (False, [])
	where
		nb = n+m
		nb1 = n-m 


readInt :: String -> Int
readInt str = read str

createList n = print $ show $ cl n

cl :: Int -> [String]
cl 0 = []
cl n = ('e' : (show n)) : cl (n-1)
