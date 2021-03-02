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
Ta = target accuracy
Twl = target worst-case latency
Ta => w
Max problem size => X,Y

Twl => P/M