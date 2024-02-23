import Tokenizer
import Parser
import TMap
import Expression

parse str = case parseSequencedCommand 1000 tokens of
    None t -> None t
    Some (c,[]) -> Some c
    Some (_,t:l) -> None ("Trailing tokens remaining: "++show (t:l))
    where tokens = tokenize str

run commstr = case parse commstr of
    Some c -> commEval c emptyState
    None t -> updateTMap emptyState "err" (-1)

check x = case x of
    Some c -> "ok"
    None t -> t

exFastExp = "x:=3; n:=700; m:=7477; \
 \    if n = 0 \
 \       then a := 1 \ 
 \    else\ 
 \        y := 1;\ 
 \        while n > 1 do\ 
 \            if (n%2)=1 then\ 
 \                y := x * y % m;\ 
 \                n := n - 1\ 
 \            else skip end;\ 
 \            x := x * x % m;\ 
 \            n := n / 2\ 
 \        end;\ 
 \        a:=x*y % m\
 \    end"

exEuclid = " a:=24826148; b:=45296490;\
 \    while b != 0 do \
 \       t := b; \
 \       b := a % b; \
 \       a := t \
 \    end"

main = do
    let t = run exEuclid
    print (t "a")
    let t = run exFastExp
    print (t "a")

