import Data.List
newtype Matrix a = Matrix [[a]]

instance Show a => Show (Matrix a) where
    show (Matrix a) = showlines True a
      where
        showlines True [[]] = "EMPTY" 
        showlines True [] = "EMPTY"  
        showlines False [] = ""
        showlines _s_first (d) = concat . intersperse "\n" . map show $ d