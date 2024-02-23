module Tokenizer where
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Maybe (fromMaybe)

data TokenType = Alpha | Digit | Spec | White

matchToken x
  | isAlpha x = Alpha
  | isDigit x = Digit
  | isSpace x = White
  | otherwise = Spec

specialChars = "()+-*%/^&|~:=;!"
isSpCh c = foldl (\b x -> x==c || b) False specialChars

processPrefix [] = []
processPrefix (a:st) = [reverse (a:st)]


_tokenize _ pref [] = processPrefix pref
_tokenize t pref (a:str) = case (t,matchToken a, a) of
    (_,_,'(') -> processPrefix pref ++ "(" : _tokenize Spec [] str
    (_,_,')') -> processPrefix pref ++ ")" : _tokenize Spec [] str
    (_,White,_) -> processPrefix pref ++ _tokenize White [] str
    (Alpha,Alpha,a) -> _tokenize Alpha (a:pref) str
    (Alpha,Digit,a) -> _tokenize Digit (a:pref) str
    (Digit,Digit,a)-> _tokenize Digit (a:pref) str
    (Spec,Spec,a) -> _tokenize Spec (a:pref) str
    (_,new_type,a) ->  processPrefix pref ++ _tokenize new_type [a] str

tokenize :: [Char] -> [[Char]]
tokenize = _tokenize White ""