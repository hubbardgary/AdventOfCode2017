let mutable a = 1
let mutable b = 0
let mutable c = 0
let mutable d = 0
let mutable h = 0

b <- 81
c <- b

if a = 1 then
    b <- b * 100
    b <- b + 100000
    c <- b + 17000

while b <= c do
    d <- 2
    let mutable breakLoop = false
    
    // If b is divisible by d with no remainder (i.e. it is not prime), increment h
    // h is counting non-prime numbers between b and c inclusive, increasing b by 17 after each iteration.
    while d < b && not breakLoop do
        if b % d = 0 then h <- h + 1
                          breakLoop <- true
        d <- d + 1
    
    b <- b + 17

printfn "Part 2 = %i" h