# recursion
let f1 = \x -> (x < 10) ? f2(x+1): x^2 ;
let f2 = \x -> (x < 10) ? f1(x+1): x^2;

#showstack();
#assert(false);
assert(f1(1) == 100)
assert(f2(1) == 100)
f1(1);
