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
gcd' :: Integer -> Integer -> Integer
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
gcd'' :: Integer -> Integer -> (Integer, Integer, Integer)
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
isMersennePrime :: Integral a => a -> (a, Bool)
isMersennePrime n = (m, if (foldr f 4 [3 .. n]) == 0 then True else False)
  where
    m = 2^n - 1                 -- メルセンヌ数
    f s _ = rem (s * s - 2) m



--
-- 3. フェルマー、オイラー、擬素数
--

--
-- アルゴリズム 3.3 ::
--
-- * b >= 0 について mod (a^b) m を計算する
--
powerMod :: (Integral a, Integral t) => t -> a -> t -> t
powerMod a b m = loop a b 1
  where
    loop a 0 n = n
    loop a b n = loop (mod (a * a) m) (div b 2) n'
      where n' = if odd b then mod (n * a) m else n


--
-- 見込み素数 (probable prime) の判定
--
isProbablePrime :: Integral a => a -> Bool
isProbablePrime n = and [powerMod p (n - 1) n == 1 | p <- [2, 3, 5, 7]]


--
-- 練習問題 3.11
--
binaryExpansion :: Integer -> [Integer]
binaryExpansion n
  | n < 2     = [n]
  | otherwise = (rem n 2) : binaryExpansion (div n 2)


--
-- 練習問題 3.12
--
farmer'sMultiplication :: Integer -> Integer -> Integer
farmer'sMultiplication a b = loop a b 0
  where
    loop _ 0 n = n
    loop a b n = loop (a + a) (div b 2) (if odd b then n + a else n)


--
-- 練習問題 3.16
--
-- オイラーのφ関数 (素因数分解を使用しないもの)
--
phiFunction :: Integral a => a -> Int
phiFunction n = length [i | i <- [1 .. n], gcd i n == 1]


--
-- 4. RSA 公開鍵暗号系
--

--
-- アルゴリズム 4.5
--
-- * 中国剰余定理
--
