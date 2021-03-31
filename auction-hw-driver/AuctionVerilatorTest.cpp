#include <iostream>
using namespace std;

#include "Auction.hpp"
#include "platform.h"


double sc_time_stamp() {
    return 0;
}

bool Run_Auction(WrapperRegDriver * platform) {
	Auction t(platform);


	uint8_t rew_mat[160] = {
    7,   51,  52,  87, 38,  60,  74,  66,
    0,   20,   0,   0,   0,   0,   0,   0,
    50,  12,   0,  64,   8,  53,   0,  46,
    76,  42,   0,   0,   0,   0,   0,   0,
    27,  77,   0,  18,  22,  48,  44,  13,
    0,  57,   0,   0,   0,   0,   0,   0,
    62,   0,   3,   8,   5,   6,  14,   0,
    26,  39,   0,   0,   0,   0,   0,   0,
    0,   97,   0,   5,  13,   0,  41,  31,
    62,  48,   0,   0,   0,   0,   0,   0,
    79,  68,   0,   0,  15,  12,  17,  47,
    35,  43,   0,   0,   0,   0,   0,   0,
    76,  99,  48,  27,  34,   0,   0,   0,
    28,   0,   0,   0,   0,   0,   0,   0,
    0,   20,   9,  27,  46,  15,  84,  19,
    3,  24,   0,   0,   0,   0,   0,   0,
    56,  10,  45,  39,   0,  93,  67,  79,
    19,  38,   0,   0,   0,   0,   0,   0,
    27,   0,  39,  53,  46,  24,  69,  46,
    23,  1,   0,   0,   0,   0,   0,   0,
    };
    uint32_t res[20];
    unsigned int bufsize_res = 20 * sizeof(res[0]);

    unsigned int bufsize = 160 * sizeof(rew_mat[0]);


    void * accelBuf = platform->allocAccelBuffer(bufsize);
    platform->copyBufferHostToAccel(rew_mat, accelBuf, bufsize);

    void * accelResBuf = platform->allocAccelBuffer(bufsize_res);
    t.set_rfIn_baseAddr((AccelDblReg) accelBuf);
    t.set_rfIn_baseAddrRes((AccelDblReg) accelResBuf);
    t.set_rfIn_nAgents(10);
    t.set_rfIn_nObjects(10);

    t.set_rfIn_start(1);
    t.set_rfIn_start(1);

    while (t.get_rfOut_finished() != 1);

    platform->copyBufferAccelToHost(accelResBuf, res, bufsize_res);
    //
    cout <<"Cycles=" <<t.get_rfOut_cycleCount() <<endl;
    for (int i = 0; i<20; i++) {
        cout <<"i=" <<i <<" res=" <<res[i] <<endl;
    }


    platform->deallocAccelBuffer(accelBuf);
    platform->deallocAccelBuffer(accelResBuf);
    delete [] rew_mat;
    delete [] res;

    return 0;
    }

int main(int argc, char** argv)
{
    cout <<"Running Auction Test program" <<endl;

    WrapperRegDriver * platform = initPlatform();
    Run_Auction(platform);
    deinitPlatform(platform);


    return 0;
}
