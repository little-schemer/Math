import Data.List

--
-- 1. 素因数一意分解とユークリッドの互除法
--

--
-- アルゴリズム 1.7 ::
--
-- * ユークリッドの互除法を使って gcd(a, b) を計算する。
--
-- * 一方あるいは両方が負であっても、 a の最後の値の絶対値が最大公約数。
-- * 一方あるいは両方が 0 の場合は 0 を返す。
--
gcd' :: Int -> Int -> Int
gcd' a 0 = abs a
gcd' a b = gcd' b (rem a b)


--
-- アルゴリズム 1.8 ::
--
-- * gcd = m * a + n * b とした時に、gcd だけでなく、m と n も計算する
--   ドナルド・クヌースのアルゴリズム
--
-- * gcd'' a b   =>  (m, n, gcd)
-- * b < 0 の場合、答がおかしくなる。
--
gcd'' :: Int -> Int -> (Int, Int, Int)
gcd'' a b = loop (1, 0) (0, 1) (a, b)
  where
    loop (u1, _) (u2, _) (u3, 0)    = (u1, u2, u3)
    loop (u1, v1) (u2, v2) (u3, v3) = loop (f u1 v1) (f u2 v2) (f u3 v3)
      where
        q = div u3 v3
        f u v = (v, u - q * v)


--
-- 2. 素数と完全数
--

--
-- アルゴリズム 2.3 ::
--
-- * 与えられた整数 n 以下のすべての素数を求めるエラトステネスの篩
--
-- * MyModule/PrimeList.hs を参照。
--


--
-- アルゴリズム 2.4 ::
--
-- * 指定された最大値 (= max) まで、小手調べ除算によって素因数分解する。
--
-- * 最終的な形式は n = p1^e1 * p2^e2 * ... * pr^er * f
-- * 完全に素因数分解できた場合、f = 1 で、できなかった場合、f は素因数
--   分解し残した部分
--
factorize :: Integral a => a -> a -> ([(a, Int)], a)
factorize 1 _    = ([], 1)
factorize n maxN = loop n [] (2 : 3 : concat [[6 * x - 1, 6 * x + 1] | x <- [1 ..]])
  where
    format ps = reverse [(head fs, length fs) | fs <- group ps]
    loop f ps ds@(d : ds')
      | d > maxN = (format ps, f)
      | d * d > f = (format (f : ps), 1)
      | rem f d == 0 = loop (div f d) (d : ps) ds
      | otherwise = loop f ps ds'


--
-- アルゴリズム 2.9 :: リュカ - レーマーのアルゴリズム
--
-- * M(n) = 2^n - 1 が素数かどうかの判定。3 以上の奇数 n について有効。
--
isMersennePrime :: Int -> (Integer, Bool)
isMersennePrime n = (m, if (foldr f 4 [3 .. n]) == 0 then True else False)
  where
    m = 2^n - 1                 -- メルセンヌ数
    f s _ = rem (s * s - 2) m