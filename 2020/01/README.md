# Day 1

## Part 1

Took some time to remember how to read input.

I assumed that I could build the list of pairs from
the start and never tracing back (order doesn't matter).

## Part 2

Have to take the previous solution and deal with combinations of three values.

Hardest part is figuring out how to take a list of numbers and return a list
of 3 tuples for the combinations of pairs:

```hs
triples xs = sequence $ replicate 3 xs
```

This produces some additional data but I assume these entries won't be false positives.
