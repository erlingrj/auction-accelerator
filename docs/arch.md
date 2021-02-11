# Action Algorithm architecture



## Accountant

### Non-pipelined

- Stages: s1/waitForBid, s2/update
#### waitForBid: 
- Waiting until searchResultOut and memoryRequested are both valid
- When they are, latch them in

#### update
- Compare bid with already existing bid and potentially overwrite
- add next guy to the queue

### Pipelined



## Search Task


## Memory controller