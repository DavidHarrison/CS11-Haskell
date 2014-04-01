import qualified Data.List as List

-- type definition of AbstractInteger
-- (all operations assume that there cannot be a Prec to a Succ or vice-versa)
data AbstractInteger = Prec AbstractInteger | Zero | Succ AbstractInteger
  deriving (Eq, Show)

-- make AbstractInteger an instance of Ord (can be compared)
instance Ord AbstractInteger where
  compare = ai_Compare
  x < y = ai_Compare x y == LT
  x >= y = not (x < y)
  x > y = ai_Compare x y == GT
  x <= y = not (x > y)
  max = ai_Max
  min x y = ai_Max y x

-- make AbstractInteger an instance of Num (can be calculated on)
instance Num AbstractInteger where
  (+) = ai_Sum
  (-) = ai_Diff
  (*) = ai_Prod
  negate = ai_Neg
  abs = ai_Abs
  signum = ai_SigNum
  fromInteger = ai_fromInteger

-- compare two AbstractIntegers
ai_Compare :: AbstractInteger -> AbstractInteger -> Ordering
-- if both Zero, they are equal
ai_Compare Zero Zero = EQ
-- if the is Zero, while the second is a Prec, the first is greater
ai_Compare Zero (Prec b) = GT
-- if the is Zero, while the second is a Succ, the first is less
ai_Compare Zero (Succ b) = LT
-- if the first is a Prec, while the second is Zero, the first is less
ai_Compare (Prec a) Zero = LT
-- if the first is a Succ, while the second is Zero, the first is greater
ai_Compare (Succ a) Zero = GT

-- find the max of two AbstractIntegers
ai_Max :: AbstractInteger -> AbstractInteger -> AbstractInteger
ai_Max a b
  | a > b = a
  | otherwise = b

-- the sum of two AbstractIntegers
ai_Sum :: AbstractInteger -> AbstractInteger -> AbstractInteger
-- if one is zero, the sum is the other
ai_Sum a Zero = a
ai_Sum Zero b = b
-- move the "sign" to the other and add the two
ai_Sum (Prec a) (Prec b) = ai_Sum (Prec (Prec a)) b
ai_Sum (Succ a) (Succ b) = ai_Sum (Succ (Succ a)) b
-- cancel each other out, cannot have both a Succ and a Prec
ai_Sum (Prec a) (Succ b) = ai_Sum a b
ai_Sum (Succ a) (Prec b) = ai_Sum a b

-- the difference of two AbstractIntegers
ai_Diff :: AbstractInteger -> AbstractInteger -> AbstractInteger
-- the sum of the first and the negative of the second
ai_Diff a b = a + (-b)

-- the product of two AbstractIntegers
ai_Prod :: AbstractInteger -> AbstractInteger -> AbstractInteger
-- the product of a number an zero is zero
ai_Prod a Zero = 0
-- if the multiplier is negative, return the sum of the first argument
--   negated and the product of the first argument and
--   (the second argument with an absolute value reduced by one)
ai_Prod a (Prec b) = (-a) + (a * b)
-- if the multiplier is positive, return the sum of the first argument
--   and the product of the first argument and
--   (the second argument with an absolute value reduced by one)
ai_Prod a (Succ b) = a + (a * b)

-- negate an AbstractInteger
ai_Neg :: AbstractInteger -> AbstractInteger
-- if it is Zero, it cannot be negated
ai_Neg Zero = Zero
-- if it is a Prec, change it to a Succ of the recursively negated inner value
ai_Neg (Prec a) = Succ (ai_Neg a)
-- if it is a Succ, change it to a Prec of the recursively negated inner value
ai_Neg (Succ a) = Prec (ai_Neg a)

-- the absolute value of an AbstractInteger
ai_Abs :: AbstractInteger -> AbstractInteger
-- if the sign is negative negate it
ai_Abs (Prec a) = Succ (negate a)
-- otherwise (positive/zero) it is itself
ai_Abs a = a

-- the sign number of an AbstractInteger
ai_SigNum :: AbstractInteger -> AbstractInteger
-- the sign number of Zero is Zero
ai_SigNum Zero = Zero
-- the sign number of a negative is -1
ai_SigNum (Prec a) = -1
-- the sign number of a positive is 1
ai_SigNum (Succ a) = 1

-- convert and Integer to an AbstractInteger
ai_fromInteger :: Integer -> AbstractInteger
-- base case: 0 is a Zero
ai_fromInteger 0 = Zero
-- recursively add PrecS or SuccS as needed
ai_fromInteger a
  -- a negative number is a Prec to a value <= Zero
  | a < 0 = Prec (ai_fromInteger (a + 1))
  -- a positive number is a Succ to a value >= Zero
  | a > 0 = Succ (ai_fromInteger (a - 1))

-- convert and AbstractInteger to an Integer
ai_toInteger :: AbstractInteger -> Integer
-- base case: Zero is 0
ai_toInteger Zero = 0
-- a Prec signals one less (-1) than a number
ai_toInteger (Prec a) = (ai_toInteger a) - 1
-- a Succ signals one greater (+1) than a number
ai_toInteger (Succ a) = (ai_toInteger a) + 1

-- non-tail-recursive factorial
factorial :: (Ord a, Num a) => a -> a
-- base case: the factorial of 0 is 1
factorial 0 = 1
-- recursively multiply the current value by all values below it (until 0)
factorial a = a * factorial (a - 1)

-- tail-recursive factorial
factorial' :: (Ord a, Num a) => a -> a
factorial' a = factorialWith a 1
        -- the factorial of 0 (1) is does not affect the accumulator z
  where factorialWith 0 z = z
        -- multiply the accumulator by the current value, passing it
        --   and the next value down to the function in recursion
        factorialWith a z = factorialWith (a - 1) (a * z)

------------------------------------------------------------------------------

-- define the Quaternion type with fields of real and imaginary coefficients
data Quaternion = Quaternion { real :: Double,
                               ic :: Double,
                               jc :: Double,
                               kc :: Double }
  -- that derives Eq for basic, element-wise, equality
  deriving Eq

-- make Quaternion an instance of Show
instance Show Quaternion where
  showsPrec d = showString . qn_Str
  show = qn_Str
  -- showList ommited (not necessary for minimum spec)

-- make Quaternion and instance of Show
instance Num Quaternion where
  (+) = qn_Sum
  (-) = qn_Diff
  (*) = qn_Prod
  negate = qn_Neg
  abs = qn_Abs
  signum = qn_SigNum
  fromInteger = qn_fromInteger

-- convert a Quaternion into a (pretty) String
qn_Str :: Quaternion -> String
qn_Str (Quaternion n i j k) =
  show n ++ " + " ++ show i ++ "i + "
  ++ show j ++ "j + " ++ show k ++ "k"

-- add (element-wise) two Quaternions
qn_Sum :: Quaternion -> Quaternion -> Quaternion
qn_Sum (Quaternion n1 i1 j1 k1) (Quaternion n2 i2 j2 k2) =
  Quaternion (n1 + n2) (i1 + i2) (j1 + j2) (k1 + k2)

-- subtract (element-wise) two Quaternions
qn_Diff :: Quaternion -> Quaternion -> Quaternion
qn_Diff (Quaternion n1 i1 j1 k1) (Quaternion n2 i2 j2 k2) =
  Quaternion (n1 - n2) (i1 - i2) (j1 - j2) (k1 - k2)

-- multiply (not element-wise) two Quaternions
qn_Prod :: Quaternion -> Quaternion -> Quaternion
qn_Prod (Quaternion n1 i1 j1 k1) (Quaternion n2 i2 j2 k2) =
  Quaternion n' i' j' k'
    where n' = (n2 * n1) - (i2 * i1) - (j2 * j1) - (k2 * k1)
          i' = (n2 * i1) + (i2 * n1) - (j2 * k1) + (k2 * j1)
          j' = (n2 * j1) + (i2 * k1) + (j2 * n1) - (k2 * i1)
          k' = (n2 * k1) - (i2 * j1) + (j2 * i1) + (k2 * n1)

-- negate a Quaternion
-- TODO, find out if actually this way or not
qn_Neg :: Quaternion -> Quaternion
qn_Neg (Quaternion n i j k) = (Quaternion n' i' j' k')
  where n' = negate n
        i' = negate i
        j' = negate j
        k' = negate k

-- find the absolute value of a Quaternion
--   (the root of the sum of the squares of each component (radial distance))
qn_Abs :: Quaternion -> Quaternion
-- place the absolute value in the real component
--   of an otherwise empty Quaternion
qn_Abs (Quaternion n i j k) = Quaternion av 0 0 0
  -- i, j, k all square to -1, so they are subtracted
  where av = sqrt (n^2 - i^2 - j^2 - k^2)

-- the sign number of a Quaternion
qn_SigNum :: Quaternion -> Quaternion
-- divide each value of the Quaternion by a scalar
--   that is the absolute value of the Quaternion
qn_SigNum (Quaternion n i j k) = Quaternion (n / s) (i / s) (j / s) (k / s)
  -- scalar value
  where s = real (abs (Quaternion n i j k))

-- convert and Integer to a Quaternion
qn_fromInteger :: Integer -> Quaternion
-- place the integer in the real component of an otherwise empty Quaternion
qn_fromInteger n = Quaternion (fromIntegral n) 0 0 0

-- Quaternions cannot usefully implemented the Ord typeclass
--   because they have four dimensions, only if one were to 
--   consider the radial distance (absolute value) could they
--   be compared effectively

-- test the multiplication of base Quaternion components
--   i, j and k in all their permutations
testQuaternions :: IO ()
testQuaternions = printStrs descrips_with_products
  where
    -- list of strings of descriptions (a x b) with Quaternion products
    -- (with space)
    descrips_with_products =
      (zipWith (\a b -> a++" "++b) product_descrip qn_strs) 
    -- the strings of the product Quaternions
    qn_strs :: [String]
    qn_strs = map show products
    -- the product Quaternions
    products :: [Quaternion]
    products = [a * b | a <- ucs, b <- ucs]
    -- list of unit components
    ucs = [i, j, k]
    -- the descriptions (a x b) of the product Quaternions
    product_descrip :: [String]
    product_descrip = [a ++ " x " ++ b ++ ": " | a <- ids, b <- ids ]
    -- list of descriptions
    ids = ["i", "j", "k"]
    -- unit components
    i = Quaternion 0 1 0 0
    j = Quaternion 0 0 1 0
    k = Quaternion 0 0 0 1

-- print a list of strings (one on each line)
printStrs :: [String] -> IO ()
printStrs [] = return ()
printStrs (q:qs) = do
                putStrLn q
                printStrs qs
