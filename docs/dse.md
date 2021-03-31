# Design Space Exploration

## Memories

### BRAMs

512 bits data 8bits address = 64 elements 
512*64 = 32Kbits
- 512 wide BRAMs Fmax ~333MHz

Actually each BRAM can be:
1,2,4,9,18,36 or 72 bit wide
512 doesnt match that well

Let say we are targeting 6bits data, we max at 64x64 problems ==> 8bit colomun = 14bits per entry

We wanna target Simple Dual Port where one port can be 72 bits wide BUT we must have only one read and one write port
Then we wanna use:

72 bit BRAM gets 330Mhz

