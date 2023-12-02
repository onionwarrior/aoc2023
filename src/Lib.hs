module Lib
    ( solveDayOne
    ) where
import Control.Arrow ( Arrow((***), (&&&)), (>>>) )

import Data.Char (ord, isDigit)

solveDayOne :: String -> Int
solveDayOne = lines >>> (map $ filter isDigit >>> (head &&& last) >>> ( ord *** ord ) >>> (\(x,y) -> x * 10 + y - 528)) >>> sum