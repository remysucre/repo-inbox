- goal of tool: make programmers happy
- why are they unhappy: 
    1. currently leaking space
    2. don't know where to put bang
    3. put in bad bangs
    4. bangs work for some input, not others
- how do we make them happy
    1 & 2. add bangs to plug thunk leak
    3. only add helpful bangs
    4. specialize bangs to benchmarks
- how we might make them unhappy
    1. change semantics, cause non-termination
    2. tool runs too long
    3. tool doesn't work
- how do we avoid making them unhappy
    1.1 to genetic algorithm, non-termination = slow, won't survive
    1.2. pass demand analyzer to ensure un-covered code also preserve semantics
    2. parallelize profiling, localize the search space of genetic algorithm
    3. show them the experiment results! 
