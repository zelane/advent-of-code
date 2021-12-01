import Data.Binary (Word16)
import Data.Bits (Bits (complement, shiftL, shiftR, (.&.), (.|.)))
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe, isJust)
import Debug.Trace (traceShow)
import Text.Read (readMaybe)
import Text.Regex (matchRegex, mkRegex)

data Exp
  = Var String
  | Val Int
  | Bin String String (Int -> Int -> Int)
  | Uni String (Int -> Int)

type Wires = M.Map String Exp

type Cache = M.Map String Int

instance Show Exp where
  show (Var i) = i
  show (Bin a b _) = a ++ b
  show (Uni a _) = a

eval :: Wires -> Exp -> Int
eval wires (Val a) = a
eval wires (Var a) = x
  where
    (nw, x) = lookup' wires a
eval wires (Uni a f) = f x
  where
    (nw, x) = lookup' wires a
eval wires (Bin a b f) = f x y
  where
    (nw0, x) = lookup' wires a
    (nw1, y) = lookup' nw0 b

lookup' :: Wires -> String -> (Wires, Int)
lookup' wires k = (newWires, val)
  where
    newWires = M.insert k (Val val) wires
    x = fromJust $ M.lookup k wires
    val = fromMaybe (eval wires x) $ readMaybe x

match :: String -> String -> (Bool, [String])
match re s = if isJust match then (True, fromJust match) else (False, [])
  where
    match = matchRegex (mkRegex re) s

parseExp :: String -> (String, Exp)
parseExp s
  | v = (vval !! 1, Var (head vval))
  | a = (aval !! 2, Bin (head aval) (aval !! 1) (.&.))
  | o = (oval !! 2, Bin (head oval) (oval !! 1) (.|.))
  | l = (lval !! 2, Bin (head lval) (lval !! 1) shiftL)
  | r = (rval !! 2, Bin (head rval) (rval !! 1) shiftR)
  | b = (bval !! 1, Uni (head bval) comp)
  where
    (v, vval) = match "^([a-z0-9]+) -> ([a-z0-9]+)$" s
    (a, aval) = match "^([a-z0-9]+) AND ([a-z0-9]+) -> ([a-z]+)$" s
    (o, oval) = match "^([a-z0-9]+) OR ([a-z0-9]+) -> ([a-z]+)$" s
    (l, lval) = match "^([a-z0-9]+) LSHIFT ([a-z0-9]+) -> ([a-z]+)$" s
    (r, rval) = match "^([a-z0-9]+) RSHIFT ([a-z0-9]+) -> ([a-z]+)$" s
    (b, bval) = match "^NOT ([a-z0-9]+) -> ([a-z0-9]+)$" s
    comp x = fromEnum $ complement (toEnum x :: Word16)

main :: IO ()
main = do
  test <- lines <$> readFile "test.txt"
  let wires = M.fromList $ map parseExp test
  print $ map (iter wires) ["d", "e", "f", "g", "h", "i", "x", "y"]

  input <- lines <$> readFile "input.txt"
  let wires = M.fromList $ map parseExp input
  print $ iter wires "ee"
