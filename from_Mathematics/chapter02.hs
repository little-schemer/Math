-- import Data.Bits

multiply0 :: Int -> Int -> Int
multiply0 n a
  | n == 1    = a
  | otherwise = (multiply0 (n - 1) a) + a


-- odd' :: Integer -> Bool
-- odd' n = testBit n 0


-- half :: Int -> Int
-- half n = shiftR n 1


multiply1 :: Int -> Int -> Int
multiply1 n a
  | n == 1     = a
  | odd n      = result + a
  | otherwise  = result
  where result = multiply1 (div n 2) (a + a)


mult_acc0 :: Int -> Int -> Int -> Int
mult_acc0 r n a
  | n == 1    = r + a
  | odd n     = mult_acc0 (r + a) (div n 2) (a + a)
  | otherwise = mult_acc0 r (div n 2) (a + a)


mult_acc1 :: Int -> Int -> Int -> Int
mult_acc1 r n a
  | n == 1 = r + a
  | otherwise = mult_acc1 r' (div n 2) (a + a)
  where r' = if odd n then r + a else r


mult_acc2 :: Int -> Int -> Int -> Int
mult_acc2 r n a
  | even n = mult_acc2 r (div n 2) (a + a)
  | n == 1 = r + a
  | otherwise = mult_acc2 (r + a) (div n 2) (a + a)
