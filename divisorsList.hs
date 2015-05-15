-------------------------------------------------------------
--
-- 約数のリスト Data.Vector 版
--
-- Coding : Little Schemer
--
-------------------------------------------------------------

import qualified Data.Vector.Mutable as M
import qualified Data.Vector         as V
import Control.Monad.ST


-------------------------------------------------------------
-- 0 〜 n までの約数のリスト
-- ex : divisorsList 5  =>  [[],[1],[1,2],[1,3],[1,2,4],[1,5]]
-------------------------------------------------------------
divisorsList :: Int -> [[Int]]
divisorsList n = V.toList $ runST $ do
  mVec <- M.replicate (n + 1) []
  mapM_ (setDivs mVec) [n, n - 1 .. 1]
  V.freeze mVec
    where
      setDivs vec i = mapM_ (setNum vec i) [i, 2 * i .. n]
      setNum vec n i = do
        lst <- M.read vec i
        M.write vec i (n : lst)
