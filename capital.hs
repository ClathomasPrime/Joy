import Data.Char
import Control.Applicative

powList :: Int -> [a] -> [[a]]
powList 0 as = [[]]
powList n as = [ a:ps | a <- as, ps <- powList (n-1) as ]

capShuffle :: String -> [String]
capShuffle s = let ffs = powList (length s) [toLower, toUpper]
                in fmap (getZipList . (<*> ZipList s) . ZipList) ffs
                -- in fmap (`zipApp` s) ffs

-- zipApp :: [a -> b] -> [a] -> [b]
-- zipApp (f:fs) (a:as) = f a : zipApp fs as
-- zipApp _ _ = []
