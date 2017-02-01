id x = x

factorial n = if n < 2 then n
				else n * factorial (n - 1)

factorial' 0 = 1
factorial' 1 = 1
factorial' n = n * factorial(n - 1)

factorial'' n
		| n < 2 = 1
		| otherwise = n * factorial''(n - 1)

factorial''' n = product[1 .. n]

a = [1, 2, 3]

plus a b = a + b

count_digits n = if abs n < 10
		then 1
		else 1 + count_digits (div n 10)

evendiv2 lst = filter even (map (div 10) lst)

len [] = 0
len [x] = 1 -- izlishno
len(x:xs)= 1 + len xs

fizzbuzz x =
	if mod x 3 == 0 && mod x 5 == 0
		then "fizzbuzz"
	else if mod x 3 == 0
		then "fizz"
	else if mod x 5 == 0
		then "buzz"
	else show x

fb lst = map fizzbuzz lst

fb' lst = [ fizzbuzz n | n <- lst]