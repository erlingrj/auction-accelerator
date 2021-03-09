# A mathmatical framework

Here I will try to build a mathematical framework for decribing the performance of the Auction Algorithm.

## What are the tunable parameters
1. W = Bit width
2. P = PEs
3. M = Memory interface width

## What is the user interested in?
1. Throughput
2. Latency
3. Energy


- Throughput and latency is the same in this case (we dont pipeline multiple auction problems)
- Energy can be measured

- bitwidth trades accurcay for area/power (you cuold keep area constant and increa nPEs)
- The problem is that we are really constrained by the fact that we dont know how many iterations its gonna take.

We have problem of X agents and Y objects (tracks vs detections)
this is a X x Y 

Throughput (iterations per second) is
- each row consists of XxW bits. 
- Each CC we can fetch M/W values of width W.  XxW = c*M/W 
- P=M/W

## Proposal
User supplies:
- Ta = target accuracy
- Twl = target worst-case latency
- Ta => w
- Max problem size => X,Y


## Clocks per iteration
- MemoryController: 2CC (from memRsp -> FIFO -> dataMux)
- DataMux: 0CC (forwards directly to PEs)
- PE: 3CC (latch, compute, output)
- SearchTask: log(nPEs) + 2 (for 4PEs = 4CC)
- Accountant: 1CC
- Evict and re-request: 2CC

Giving a total of: 9CC + log(nPEs) per iteration. Assuming that memory width = nPE*bitWidth

## Processing breakdown
For PE and Search. Assuming data available from memory always

| nPEs | 4x4 | 8x8 | 16x16 | 32x32 | 64x64 | 128x128|
|:------|:---|:---|:-------|:------|:------|:-------|
| **4** | 7 | 14  | 28| 56| 112| 224|
| **8** | 8 | 8| 16  |  32 |  64|   128|  
| **16** | 9  | 9  |   9|   18|       36|       72| 
| **32** | 10  |   10|  10 |  10 |    20   |       40|       
| **64** | 11  | 11  | 11  |11   | 11      | 22      |     
| **128** |12   | 12    | 12  | 12  |   12    |      12 | 
 
| nPEs | 4x4 | 8x8 | 16x16 | 32x32 | 64x64 | 128x128|
|:------|:---|:---|:-------|:------|:------|:-------|
| **4** | 1     | 1.8  | 3.1| 5.6| 10.2| 20.4|
| **8** | 1.1   | 1     | 1.8 |  3.2 |  5.8|   10.7|  
| **16** | 1.3  | 1.1   | 1|   2.0 |       3.3|       6| 
| **32** | 1.4  | 1.3   |  1.1 |  1 |    1.8   |       3.3|       
| **64** | 1.6  | 1.4   | 1.2  |1.1   | 1      | 1.83      |     
| **128** |1.7   | 1.5    | 1.33  | 1.2  |   1.1    |      1 | 
