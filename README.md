An interpreter of simple imperative code made with purely functional Haskell. Insipred by software foundations. 

The state is represented with a total function from `String` to `Integer`. A `Command` is a function from `State` to `State`. The `parse` function tries to translate procedural code into purely functional `Command`, and detects possible errors. 

## Examples:

Euclidean algorithm: 
```
a:=24826148; b:=45296490;
while b != 0 do 
  t := b; 
  b := a % b; 
  a := t 
end
```

Exponentiation by squaring: 
```
x:=3; n:=700; m:=7477; 
if n = 0 
  then a := 1  
else 
   y := 1; 
   while n > 1 do 
       if (n%2)=1 then 
           y := x * y % m; 
           n := n - 1 
       else skip end; 
       x := x * x % m; 
       n := n / 2 
   end; 
   a:=x*y % m
end
```



