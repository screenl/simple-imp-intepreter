
module Parser where
import Expression
import Data.Char (isAlpha, isDigit, isSpace)
import Data.Maybe (fromMaybe)
data Option t = Some t | None String


unwrapOpt f (Some t) = Some (f t)
unwrapOpt f (None s) = None s

tryOpt f (Some t) _ = f t
tryOpt f (None _) a = a



type Parser t = [String] -> Option (t,[String])

_many :: Parser t -> [t] -> Integer -> [String] -> Option ([t],[String])
_many _ _ 0 _ = None "Recursion limit exceeded"
_many p acc steps strs = case p strs of
    Some (h,strs') -> _many p (h:acc) (steps-1) strs'
    None _ -> Some (reverse acc, strs)

many :: Parser t -> Integer -> Parser [t]
many p = _many p []

expectHead :: String -> Parser t -> [String] -> Option (t,[String])
expectHead head _ [] = None ("Expected "++ head)
expectHead head p (h:strs) = if head==h then p strs else None ("Expected "++head)

expect t = expectHead t (\x -> Some ((),x))

parseId :: Parser String
parseId [] = None "Expected identifier"
parseId (a:l) = if all (\x -> isAlpha x || isDigit x) a && not (isDigit h) then Some (a,l) else None ("Illegal identifier at "++ a)
    where h = case a of
                (c:s) -> c
                [] -> 'a'

parseNumber :: Parser Integer
parseNumber [] = None "Expected number"
parseNumber (a:l) = if all isDigit a then Some (read a,l) else None ("Expected  umber at "++ a)



parsePrimaryExp 0 strs = None "Recursion limit exceeded"
parsePrimaryExp step strs = case parseId strs of
    Some (i,rest) -> Some (AId i, rest)
    None _ -> case parseNumber strs of
        Some (n, rest) -> Some (ANum n, rest)
        None _ -> case expectHead "(" (parseSumExp (step-1)) strs of
            Some (l, rest) -> case expect ")" rest of
                Some (r,rest') -> Some (l,rest')
                None a -> None a
            None a -> None a 

sumDecode e1 (x,e) = if x==1 then APlus e1 e else AMinus e1 e

parseSumExp 0 _ = None "Recursion limit exceeded"
parseSumExp step strs = case parseProductExp (step-1) strs of
    Some (exp,rest) -> case many ( parseSumExpInner step) (step-1) rest of
        None t -> None t
        Some (expp, restt) -> Some ( foldl sumDecode exp expp, restt )
    None t -> None t

parseSumExpInner step strs = case expectHead "+" (parseProductExp (step-1)) strs of
    Some (expp,restt) ->  Some ((1,expp),restt)
    None t -> case expectHead "-" (parseProductExp (step-1)) strs of
        Some (expp,restt) ->  Some ((0,expp),restt)
        None t -> None t

productDecode e1 (x,e)
  | x==0 = AMul e1 e
  | x==1 = ADiv e1 e
  | otherwise = AMod e1 e

parseProductExpInner step strs = case expectHead "*" (parsePrimaryExp (step-1)) strs of
    Some (expp, restt) -> Some ((0,expp), restt)
    None t -> case expectHead "/" (parsePrimaryExp (step-1)) strs of
        Some (expp, restt) -> Some ((1,expp), restt)
        None t -> case expectHead "%" (parsePrimaryExp (step-1)) strs of
            Some (expp, restt) -> Some ((2,expp), restt)
            None t -> None t

parseProductExp 0 _ = None "Recursion limit exceeded"
parseProductExp step strs = case parsePrimaryExp (step-1) strs of
    Some (exp, rest) -> case many (parseProductExpInner step) (step-1) rest of
        Some (expp, restt) -> Some (foldl productDecode exp expp,restt)
        None t -> None t
    None t -> None t

parseArithExp = parseSumExp

parseAtomicExp 0 _ = None "Recursion limit exceeded"
parseAtomicExp step strs = tryOpt (\(_,y) -> Some (BTrue,y)) (expect "true" strs)
                            (tryOpt (\(_,y) -> Some (BFalse,y)) (expect "false" strs)
                            (tryOpt (\(e,y) -> Some (BNot e,y)) (expectHead "~" (parseAtomicExp (step-1)) strs)
                            (tryOpt (\(e,y) -> unwrapOpt
                                    (\(u,y') -> (e,y')) (expect ")" y))
                                (expectHead "(" (parseConjunctionExp (step-1)) strs)
                            (case parseProductExp (step-1) strs of
                                None t -> None t
                                Some (e,y) ->
                                    tryOpt (\ (ee,y') -> Some (BEq e ee, y')) (expectHead "=" (parseArithExp (step-1)) y)
                                    (tryOpt (\ (ee,y') -> Some (BLe e ee, y')) (expectHead "<=" (parseArithExp (step-1)) y)
                                    (tryOpt (\ (ee,y') -> Some (BGe e ee, y')) (expectHead ">=" (parseArithExp (step-1)) y)
                                    (tryOpt (\ (ee,y') -> Some (BGt e ee, y')) (expectHead ">" (parseArithExp (step-1)) y)
                                    (tryOpt (\ (ee,y') -> Some (BLt e ee, y')) (expectHead "<" (parseArithExp (step-1)) y)
                                    (unwrapOpt (\ (ee,y') -> (BNe e ee, y'))
                                        (expectHead "!=" (parseArithExp (step-1)) y)
                                    )))))))))


conjDecode e1 (x,e)
  | x==0 = BAnd e1 e
  | x==1 = BOr e1 e
  | otherwise = BXor e1 e

parseConjunctionExpInner step strs = case expectHead "&&" (parseAtomicExp (step-1)) strs of
    Some (expp, restt) -> Some ((0,expp), restt)
    None t -> case expectHead "||" (parseAtomicExp (step-1)) strs of
        Some (expp, restt) -> Some ((1,expp), restt)
        None t -> case expectHead "^" (parseAtomicExp (step-1)) strs of
            Some (expp, restt) -> Some ((2,expp), restt)
            None t -> None t

parseConjunctionExp::Integer->Parser BoolExp
parseConjunctionExp 0 _ = None "Recursion limit exceeded"
parseConjunctionExp step strs = case parseAtomicExp (step-1) strs of
                                None t -> None t
                                Some (exp,rest) -> unwrapOpt (\(expp,restt) -> (foldl conjDecode exp expp,restt))
                                             (many (parseConjunctionExpInner step) (step-1) rest )

parseBoolExp = parseConjunctionExp


parseSimpleCommand 0 _ = None "Recursion limit exceeded"
parseSimpleCommand step strs = tryOpt (\(u,rest) -> Some(CSkip,rest)) (expect "skip" strs) 
                               (tryOpt id (case  expectHead "if" (parseBoolExp (step-1)) strs of
                                    None t -> None t
                                    Some (e,rest) -> case expectHead "then" (parseSequencedCommand (step-1)) rest of
                                        None t -> None t
                                        Some (c1,restt) -> case expectHead "else" (parseSequencedCommand (step-1)) restt of
                                            None t -> None t
                                            Some (c2,resttt) -> unwrapOpt (\(ee,restttt) -> Some (CIf e c1 c2,restttt)) (expect "end" resttt))
                                (tryOpt id (case  expectHead "while" (parseBoolExp (step-1)) strs of
                                    None t -> None t
                                    Some (e,rest) -> case expectHead "do" (parseSequencedCommand (step-1)) rest of
                                        None t -> None t
                                        Some (c1,restt) -> unwrapOpt (\(ee,resttt) -> Some (CWhile e c1, resttt)) (expect "end" restt)) 
                                (unwrapOpt id (case parseId strs of
                                    None t -> None "Expecting a command"
                                    Some (i,rest) -> unwrapOpt (\(e,restt) -> (CAsgn i e, restt)) (expectHead ":=" (parseArithExp (step-1)) rest)
                                ))))

parseSequencedCommand 0 _ = None "Recursion limit exceeded"
parseSequencedCommand step strs = case parseSimpleCommand (step-1) strs of 
            None t -> None t
            Some (c,rest) -> tryOpt 
                (\(cc,restt) -> Some(CSeq c cc,restt)) (expectHead ";" (parseSequencedCommand (step-1)) rest) 
                (Some (c,rest))

