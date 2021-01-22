# Notes

## TODO
- SearchTask exploration: We wanna be able to do all the ProcessingElements in parallell and
quickly do the comparisons
- DataDistributor: We should always request full memory width and then handle the distribution as it comes
- Interfaces: Consider adding tlast signal to avoid having multiple similar state machines
- PEs. Either all must process in parallell or we only need 1. That is probably to very different implementations.
The first is a parallell processing parallell Search the second is sequential and tiny everything
- We must write the results back to memory. Not that hard, just define a function that does it?




## Log
### January 18 SearchTask
- We need to consider cases when nPEs are less then problemsize 
maybe currently forget about optimizing it? 

1. Build a tree of comps + lastWinner
2. Add another stage where winner compares with lastWInner 
and maybe updates it
3. Add a tlast signal to the interface such that we now when to pipe out the results
instead of just updating the lastWinner variable.

### January 12
- Move everything to a own repo
- Submodule fpga-tidbits
- Setup Github Actions for CI