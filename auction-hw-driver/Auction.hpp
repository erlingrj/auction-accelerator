
#ifndef Auction_H
#define Auction_H
#include "wrapperregdriver.h"
#include <map>
#include <string>
#include <vector>

// template parameters used for instantiating TemplatedHLSBlackBoxes, if any:


using namespace std;
class Auction {
public:
  Auction(WrapperRegDriver * platform) {
    m_platform = platform;
    attach();
    if(readReg(0) != 0x1159cc0f)  {
      throw "Unexpected accelerator signature, is the correct bitfile loaded?";
    }
  }
  ~Auction() {
    detach();
  }

    void set_rfIn_baseAddrRes(AccelDblReg value) { writeReg(1, (AccelReg)(value >> 32)); writeReg(2, (AccelReg)(value & 0xffffffff)); }
  void set_rfIn_nObjects(AccelReg value) {writeReg(3, value);} 
  void set_rfIn_nAgents(AccelReg value) {writeReg(4, value);} 
  void set_rfIn_baseAddr(AccelDblReg value) { writeReg(5, (AccelReg)(value >> 32)); writeReg(6, (AccelReg)(value & 0xffffffff)); }
  void set_rfIn_start(AccelReg value) {writeReg(7, value);} 
  AccelReg get_rfOut_cycleCount() {return readReg(8);} 
  AccelReg get_rfOut_finished() {return readReg(9);} 
  AccelReg get_signature() {return readReg(0);} 


  map<string, vector<unsigned int>> getStatusRegs() {
    map<string, vector<unsigned int>> ret = { {"rfOut_cycleCount", {8}} ,  {"rfOut_finished", {9}} ,  {"signature", {0}} };
    return ret;
  }

  AccelReg readStatusReg(string regName) {
    map<string, vector<unsigned int>> statRegMap = getStatusRegs();
    if(statRegMap[regName].size() != 1) throw ">32 bit status regs are not yet supported from readStatusReg";
    return readReg(statRegMap[regName][0]);
  }

protected:
  WrapperRegDriver * m_platform;
  AccelReg readReg(unsigned int i) {return m_platform->readReg(i);}
  void writeReg(unsigned int i, AccelReg v) {m_platform->writeReg(i,v);}
  void attach() {m_platform->attach("Auction");}
  void detach() {m_platform->detach();}
};
#endif
    