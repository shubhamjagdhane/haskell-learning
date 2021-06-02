-- 1. let x = 3; y = 1000 in x * 3 + y

ans1 = (\x -> \y -> x * 3 + y) 3 1000

-- 2. let y = 10; x = 10 * 5 + y in x * 5
y = 10
ans2 = (\x -> x * 5 ) (10 * 5 + y) 

-- 3. let x = 7; y = negate x; z = y * 10 in z / x + y
x = 7
y1 = negate x
ans3 = (\z -> z / x + y1) (y1 * 10)
