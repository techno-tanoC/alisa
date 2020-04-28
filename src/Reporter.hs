module Reporter where

import Data.Word (Word64)

data Reporter = Reporter String Word64 Word64 deriving (Show, Eq)
