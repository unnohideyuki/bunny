import Data.Char
import Numeric
import Data.Foldable (foldr', foldl')

data Categ = UniWhite | UniSmall | UniLarge | UniSymbol | UniDigit | OtherCateg

categorize c (whites, smalls, larges, symbols, digits) =
  case categ (chr c) of
    UniWhite  -> (c:whites, smalls, larges, symbols, digits)
    UniSmall  -> (whites, c:smalls, larges, symbols, digits)
    UniLarge  -> (whites, smalls, c:larges, symbols, digits)
    UniSymbol -> (whites, smalls, larges, c:symbols, digits)
    UniDigit  -> (whites, smalls, larges, symbols, c:digits)
    _         -> (whites, smalls, larges, symbols, digits)
  where
    categ c =
      case generalCategory c of
        UppercaseLetter       -> UniLarge -- upper
        LowercaseLetter       -> UniSmall -- lower
        TitlecaseLetter       -> UniLarge -- upper
        OtherLetter           -> UniSmall -- lower -- see #1103
        DecimalNumber         -> UniDigit -- digit
        OtherNumber           -> UniDigit -- digit -- see #4373
        ConnectorPunctuation  -> UniSymbol -- symbol
        DashPunctuation       -> UniSymbol -- symbol
        OtherPunctuation      -> UniSymbol -- symbol
        MathSymbol            -> UniSymbol -- symbol
        CurrencySymbol        -> UniSymbol -- symbol
        ModifierSymbol        -> UniSymbol -- symbol
        OtherSymbol           -> UniSymbol -- symbol
        Space                 -> UniWhite -- space
        _                     -> OtherCateg

(whites, smalls, larges, symbols, digits) = foldr' categorize ([],[],[],[],[]) [0x80..0x10ffff]

sects xs = 
    let
      (pend, acc) =
              foldl'
              (\(pend, acc) i ->
                if pend == [] then
                  ([i], acc)
                else
                  if i /= (last pend) + 1 then
                    ([i], acc ++ [pend])
                  else
                    (pend ++ [i], acc)
              )
              ([], [])
              xs
    in
      acc ++ [pend]

psecs ss =
  let
    hex i = "\\x" ++ showIntAtBase 16 intToDigit i "" 

    f xs = if length xs == 1 then
             hex (head xs) ++ " "
           else
             hex (head xs) ++ "-" ++ hex (last xs) ++ " "
  in
    concat $ fmap f ss

pmacro label xs =
  putStrLn $ "$" ++ label ++ " = [ " ++ (psecs $ sects xs) ++ "]"

main = do
  pmacro "uniwhite" whites
  pmacro "unismall" smalls
  pmacro "unilarge" larges
  pmacro "unisymbol" symbols
  pmacro "unidigit" digits
