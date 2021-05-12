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
## Last time
- The changes I made to the TesterWrappeMemory is fucking it up somehow.
- in the 4x4 PEs example we dont get the expected order of mem resps back. What could be the issue??
-

## April 23
- Passes all Verilator tests approx 10x improvement over SW (thats not that impressive though, but we are dealing with simple problems)
- Does not meet timing: There are problems in DRAM2BRAM worst path is:
Queue.deq -> regElCnt (update) -> regBramLine

We need a quite substantial rewrite of DRAM2BRAM we should introduce a pipeline there:
Stage 1:
read DRAM word, split into BramEls with col-info
-> rowFinishd++
stage 2:
-> Do popcount + Compactor and remaining




## April 20
- Upgrade DRAM2BRAM module, now rows are bram-word aligned also
- BramController needs tests (Boiler plate is done)

## April 19
- New PEWithExtPrice looks good
- Started working the BramController. But had to make changes to DRAM2BRAM
- Just finished with utility function for calculating the AgentRowInfo stuff. Should test that we now also get the length
- Then finish up BramController and Accountant


## April 6
- DRAM2BRAM looks to be about done
- RegStore looks good.
- Next up: Move prices into RegStore and adapt Accountant and PEs.
- Then=> Move from DRAM to BRAM with sparsity. Requires a couple of things:
1. BRAMMemCtroller which fetches data until last bit is set. and feeds a stream of data out (with last bits)
2. A AgentRowAddress storage based on RegStore
3. Connect BRAM module to all this

## April 2
- Got the first test of DRAM2BRAM working with the golden model an everything. Now its just a matter of testing all the edge cases
- Gotta test:
1. DRAM rows with no valid entry
2. Problem row spread over multiple DRAM rows filling multiple BRAM rows
3. 
- Then: Make the RegStore with one writer and multiple readers. Consider making it based on LUTRAMs also
- Then we need to move both AgentRowAddresses and Prices into RegStore
- Then adapt Accountant and PEs to use RegStore for Prices
- Then make the new AuctionBRAMController

## April 1
- Started testing DRAM2BRAM. Just fixed bug in the Compactor.
- Probably the respQ is not needed as we have no need to stall incoming data.
- Just follow the signs.

## March 25
- Some minor things missing in the DRAM2BRAM module before you can start making real unit tests:
1. We have to count the received dram-words so that we know when its "finished"
2. We must possibly do a last round if we overflow on the last dram-word

- Then what remains is simply: 
1. Adapt controller to FIRST start DRAM2BRAM and schedule all the BRAM reads
2. Make a BRAM controller that just fetches the data straight from BRAM 
3. Make the AgentRowStartAddress Register storage
4. Make the Prices Register storage
5. Adapt Accountant to write to the Prices Register Storage
6. Yiddi yaddi ya. It is actually quite alot to do here. Fuck

## March 12: Run
- A really small variant of the accelerator with 8PEs runs on the ZedBoard with 100MHz (I think) clock.
- I have spent HOURS trying to execute a simple shell command in Scala using the environmental variables
- Should checkout Rosetta-code for fast deployment on the PYNQ. Also I need to synthesis the Vivado project making
and the synthesis of the project. Use good ol TCL?
- OPS: The accelerator only works a single time, so probably it doesnt reset like it should

## March 11: Build
- Setting up build and characterize flow using oh-my-xilinx
- should probably download and install oh-my-xilinx properly.

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