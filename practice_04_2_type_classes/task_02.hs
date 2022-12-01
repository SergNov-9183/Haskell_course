import Data.Complex

newtype Cmplx = Cmplx (Complex Double) deriving Eq
 
instance Show Cmplx where
  show (Cmplx z) = (show $ realPart z) ++ (if im >= 0 then "+" else "-") ++ "i*" ++ (show $ abs im) where im = imagPart z
 
instance Read Cmplx where
  readsPrec _ = myReadsCmplx
 
myReadsCmplx s = [(Cmplx (x :+ y), t) | (x,u) <- readRealPart s, (y,t) <- readImagPart u]
 
readRealPart s = reads s :: [(Double, String)]
readImagPart ('+':'i':'*':s) = reads s :: [(Double, String)]
readImagPart ('-':'i':'*':s) = [(-re,u) | (re,u) <- reads s] :: [(Double, String)]