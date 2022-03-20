x = 1.00001;
x;
1.1;
1;
1e10;
.1;
0000000;
x + 1;
y = 1.1;
-2^3;
+x^y;
-x^y;
-x^(y+1);
-x^y+1;
-1*x^y;
-1+x^y;
v=0;
-1+(v+1)^(y+1);
2^3^4;
a = 2;
b = 3;
c = 4;
a/b/c;
y < y;
clock();
assert(y <= y);
f = \x -> { x^2; };;
f(1)

result = f(2.0)^f(2.0);
assert(result == 4.0^4.0)

f;
#f2 = \x y -> (x^2 + y^2);;
#f2;

"";

"🎁 🐪 𝕭𝖔𝖎𝖑𝖊𝖗⸗𝖕𝖑𝖆𝖙𝖊 𝖋𝖔𝖗 𝖀𝖓𝖎𝖈𝖔𝖉𝖊⸗𝕬𝖜𝖆𝖗𝖊 𝕮𝖔𝖉𝖊 🐪 🎁";
f(x)^f(x);
{ x = 1; };;
\x -> {
        y;
};;
\x -> y;;

cond = ((1 > 2) ? x : y)
assert(cond == y)

// "x" + "y"

asdf = 1 + 2
asdf

assert(asdf == 3)

# more comments

asdf

f = \x -> x^2
assert(4 == f(2))
f2 = \x -> f(x)
f2(3)

# verify that we can use non-local variables for calculationsa in a closure
nonlocal_x = 1
x = 2
f = \x -> x + nonlocal_x
assert(4 == f(3))

# verify that we are able to modify non local variables from within the closure
f = \x -> {
        nonlocal_x = 2
        x + 1
}
assert(2 == f(1))
assert(nonlocal_x == 2)

# check to make sure closures don't leak
# TODO: it currently does

f = \x -> {
  # temporary variable created inside of the closure
  super_local = 1
  nonlocal_x = 2
  x + 1
}
f(1)
super_local

# asdf should not be visible outside the block
{
        asdf = "sadf"
}
# asdf should be freed up now
asdf

# recursion
f1 = \x -> (x < 10) ? f1(x+1): false
f1(1)

