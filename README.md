Accelerating the Auction Algorithm on a FPGA
=======================

This repo contains RTL for a parameterizable FPGA accelerator for the Auction Algorithm.
It uses fpga-tidbits heavily and is written in Chisel3. Can be simulated in Verilator or deployed to a Zedboard.

For more information check out our paper [Solving Sparse Assignment Problems on FPGAS](https://dl.acm.org/doi/full/10.1145/3546072) published in ACM TACO in 2022. If you use the auction accelerator in your research please cite:
```
@article{jellum2022auction,
author = {Jellum, Erling and Orlandi\'{c}, Milica and Brekke, Edmund and Johansen, Tor and Bryne, Torleiv},
title = {Solving Sparse Assignment Problems on FPGAs},
year = {2022},
issue_date = {December 2022},
publisher = {Association for Computing Machinery},
address = {New York, NY, USA},
volume = {19},
number = {4},
issn = {1544-3566},
url = {https://doi.org/10.1145/3546072},
doi = {10.1145/3546072},
abstract = {The assignment problem is a fundamental optimization problem and a crucial part of many systems. For example, in multiple object tracking, the assignment problem is used to associate object detections with hypothetical target tracks and solving the assignment problem is one of the most compute-intensive tasks. To enable low-latency real-time implementations, efficient solutions to the assignment problem is required. In this work, we present Sparse and Speculative (SaS) Auction, a novel implementation of the popular Auction algorithm for FPGAs. Two novel optimizations are proposed. First, the pipeline width and depth are reduced by exploiting sparsity in the input problems. Second, dependency speculation is employed to enable a fully pipelined design and increase the throughput. Speedups as high as 50 \texttimes{} are achieved relative to the state-of-the-art implementation for some input distributions. We evaluate the implementation both on randomly generated datasets and realistic datasets from multiple object tracking.},
journal = {ACM Trans. Archit. Code Optim.},
month = {dec},
articleno = {55},
numpages = {20},
keywords = {Assignment problem, FPGA, Auction method, object tracking}
}
```
