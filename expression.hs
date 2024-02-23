module Expression where

import Data.Bits (Bits(xor))
import TMap 

data ArithExp = ANum Integer
              | AId String
              | APlus ArithExp ArithExp
              | AMinus ArithExp ArithExp
              | AMul ArithExp ArithExp
              | ADiv ArithExp ArithExp
              | AMod ArithExp ArithExp

arithEval :: ArithExp -> State -> Integer
arithEval (ANum n) st = n
arithEval (AId i) st = st i
arithEval (APlus a b) st = arithEval a st + arithEval b st
arithEval (AMinus a b) st = arithEval a st - arithEval b st
arithEval (AMul a b) st = arithEval a st * arithEval b st
arithEval (ADiv a b) st = if bb==0 then 0 else div (arithEval a st) bb
    where bb = arithEval b st
arithEval (AMod a b) st = if bb==0 then 0 else mod (arithEval a st) bb
    where bb = arithEval b st


data BoolExp = BTrue
             | BFalse
             | BAnd BoolExp BoolExp
             | BOr BoolExp BoolExp
             | BNot BoolExp
             | BXor BoolExp BoolExp
             | BEq ArithExp ArithExp
             | BLe ArithExp ArithExp
             | BGe ArithExp ArithExp
             | BLt ArithExp ArithExp
             | BGt ArithExp ArithExp
             | BNe ArithExp ArithExp

boolEval :: BoolExp -> State -> Bool
boolEval BTrue st = True
boolEval BFalse st = False
boolEval (BAnd a b) st = boolEval a st && boolEval b st
boolEval (BOr a b) st = boolEval a st || boolEval b st
boolEval (BXor a b) st = xor (boolEval a st) (boolEval b st)
boolEval (BNot a) st = not (boolEval a st)
boolEval (BEq a b) st = arithEval a st == arithEval b st
boolEval (BLe a b) st = arithEval a st <= arithEval b st
boolEval (BGe a b) st = arithEval a st >= arithEval b st
boolEval (BLt a b) st = arithEval a st < arithEval b st
boolEval (BGt a b) st = arithEval a st > arithEval b st
boolEval (BNe a b) st = arithEval a st /= arithEval b st


data Command = CSkip
            | CAsgn String ArithExp
            | CSeq Command Command
            | CWhile BoolExp Command
            | CIf BoolExp Command Command

commEval :: Command -> State -> State
commEval CSkip st = st
commEval (CAsgn var ae) st = updateTMap st var (arithEval ae st)
commEval (CSeq c1 c2) st = commEval c2 (commEval c1 st)
commEval (CWhile b c) st = if boolEval b st
                            then commEval (CWhile b c) (commEval c st)
                            else st
commEval (CIf b ct cf) st = if boolEval b st
                             then commEval ct st
                             else commEval cf st