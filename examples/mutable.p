let mut mx = 1;
mx = 3;
assert(mx == 3)
{ mx = 2; };;
assert(mx == 2)

# define something in the block
{ 
        let mx = 10;
        let mx2 = 1;
};;

# still 2
assert(mx == 2)

# mx should still be mutable
mx = 9

showstack()

# what's in the block should no longer be visible
assert(mx2 == 1)

