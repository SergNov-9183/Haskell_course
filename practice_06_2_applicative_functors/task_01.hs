import Control.Applicative (ZipList(ZipList), getZipList)

infixl 4 >$<
(>$<) :: (a -> b) -> [a] -> [b]
(>$<) = (<$>)

infixl 4 >*<
(>*<) :: [a -> b] -> [a] -> [b]
xs >*< ys = getZipList $ ZipList xs <*> ZipList ys