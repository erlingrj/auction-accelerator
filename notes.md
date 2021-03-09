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
## March 8: Memory integratio work better
- Unaligned memory access now works.
- Need to test more with multiple rounds.
- Need to test evictions 
- Should we implement Auction in Scala?
- Should we add a valid bit to assignments and prices?
- We should get a full C++ version running with Verilator. We need a reg-file-driver++, but FPGA-tidbits should help
with that. Then should we try getting it onto the PYNQ? First ZedBoard with C++



## March 5: Integration tests
- Basic 4x4 super simple problem works.
- Working with unaligned example (5x4) problem where we get skewed memory stuff
- New memory assumption:
EACH ROW STARTS IS ALIGNED TO 64 BIT MEM ROWS
- Its implemented in MemoryController.
- NEXT TIME: fix the setting up of memory in TestAucition. Consider implementing SW auction in Scala

## March 4: Debugging TesterWrapper for memory interactions
- MPORT = write-port
- io_memReadData_MPORT = read-port

## March 3:
- Finished testing all submodules. Now ready for the auction algorithm itself.
- Currently fails at MemoryController due to a misaligned memory access. Figure that shit out. 
- The challenge is, as always, in the memory interface
- We need to get this stuff to the BRAM

### Feb 23:
- All parts are connected. We need to test everything
- TestDataDistributor: Done

#### Milica meeting
- Formalize performance mathematically: Depends on
1. nPEs
2. problem-size
3. bitwidth
4. memory width
- Work towards getting a minmal version up and running on the bloody ZYnq.

### Feb 9: More dataflow
- meeting with milica
-> Do software simulations to see how a "fuzzy" approach would do
-> If not we NEED to stall between data MUX and the PEs to get the correct prices

### Feb 3: Dataflow
- Proposal here: 
1. When start goes high we initialzie the qUnassigned with all the agents. 
2. Memory controller has the dequeue side of that queue
3. Memory controller dequeues and the enqueues them into the qRequested
4. AuctionController has the dequeue of qRequested and also the enqueue of qUnassigned
5. This way we get a beautifully decoupled arch

### Feb 2: Memory controller seems to work
- Using Verilator backend solved the timing issues of Chiseltester
- Moving towards a streaming architecture rather than a central controller architecture.
- The controller still passes the prices to the PEs and maintains the free and assigned vec's.
- Probably wanna assign one BRAM per AA and have a small part of it be the result.
- Q: What happens if a price isnt updated correctly? A: It still works

### Jan 27: MemoryController getting there
- Made a simpler Memory Controller that can access the main memory and mask the result and pass to DD
- Fucking wierd problem with ChiselTester. The timing is all wierd getting signals that are high for only half a CC
- Next time: check out lates version of chisel-tester (publishLocal??) and see if that fixes it
- Then Its on to the AuctionController and then we can try to get this thing UP AND RUNNING GAWD DAMN IT
- Also think about moving data to/from BRAM. 


### January 22 SearchTask done
- Its done but it is un-pipelined. But that can easily be fixed later. as long as we dont make it dependent on that oter places
- Fixed PE also but next time update the tests.
- Currently DataDistributor always requests memWidth from the memory. It probably should be it shouldnt be necessary to
need the whole

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