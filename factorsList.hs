-------------------------------------------------------------
--
-- 素因数分解のリスト Data.Vector 版
--
-- Coding : Little Schemer
--
-------------------------------------------------------------


import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as M
import Control.Monad.ST
import Control.Monad


-------------------------------------------------------------
-- 0 〜 n までの素因数分解のリスト
-- ex : factorizeList 5  =>  [[],[],[2],[3],[2,2],[5]]
-------------------------------------------------------------
factorizeList :: Int -> [[Int]]
factorizeList n = map reverse $  V.toList $ sieve
  where
    sieve = runST $ do
      vec <- M.replicate (n + 1) []
      mapM_ (consP vec) [2 .. n]
      V.freeze vec
    consP vec p = do
      v <- M.read vec p
      when (null v) $ mapM_ f $ takeWhile (<= n) $ iterate (* p) p
        where
          f i = mapM_ g [i, 2 * i .. n]
          g i = do
            ps <- M.read vec i
            M.write vec i (p : ps)
