add_and_double :: Num a => a -> a -> a
add_and_double x y = (x + y) * 2

(+*) :: Num a => a -> a -> a
a +* b = a `add_and_double` b

solve_quadratic_equation :: Double -> Double -> Double -> (Double, Double)
solve_quadratic_equation a b c = (quadratic_funct (+), quadratic_funct (-))
  where quadratic_funct = quadratic_root a b c
        quadratic_root a b c f = ((-b) `f` (determinant a b c)) / (2 * a)
        determinant a b c = sqrt (b ** 2 - 4 * a * c)

first_n :: Int -> [Int]
first_n n = take n [1..]

first_n_integers :: Integer -> [Integer]
first_n_integers n = take_integers n [1..]
  where take_integers 0 _ = []
        take_integers _ [] = error "Too short of a list"
        take_integers n (x:xs) = (toInteger x):(take_integers (n - 1) xs)

double_factorial :: Integer -> Integer
double_factorial 0 = 1
double_factorial n = (factorial n) * double_factorial (n - 1)
  where factorial 0 = 1
        factorial n = n * factorial (n - 1)

double_factorial' :: Integer -> Integer
double_factorial' n = foldl (*) 1 (map factorial (take (fromIntegral n) [1..]))
  where factorial 0 = 1
        factorial n = foldl (*) 1 (take n [1..])

factorials :: [Integer]
factorials = 1:(zipWith (*) factorials [2..])
