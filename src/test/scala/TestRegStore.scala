package auction


import chisel3.experimental.BundleLiterals._
import org.scalatest._
import chiseltest._
import chisel3._

class TestRegStore extends FlatSpec with ChiselScalatestTester with Matchers {
  def initClocks(c: RegStore[MyData]): Unit = {
    c.io.wPorts.map(_.req.initSource().setSourceClock(c.clock))
    c.io.rPorts.map(_.req.initSource().setSourceClock(c.clock))

    c.io.wPorts.map(_.rsp.initSink().setSinkClock(c.clock))
    c.io.rPorts.map(_.rsp.initSink().setSinkClock(c.clock))
  }

  class MyData extends Bundle {
    val a = UInt(12.W)
    val b = UInt(11.W)
    val last = Bool()
  }

  val p = new RegStoreParams(
    nReadPorts = 8, nWritePorts = 1, addrBits = 8
  )

 def writeData(c: RegStore[MyData], data: Tuple3[Int,Int,Boolean], addr: Int, port: RegStoreTransaction[MyData]): Unit = {
   port.req.enqueue(chiselTypeOf(port.req).bits.Lit(
     _.wen -> true.B,
     _.addr->addr.U,
     _.wdata.a -> data._1.U,
     _.wdata.b -> data._2.U,
     _.wdata.last -> data._3.B
   ))
   c.clock.step()
 }

 def expectData(c: RegStore[MyData], data: Tuple3[Int,Int,Boolean], addr: Int, port: RegStoreTransaction[MyData]): Unit = {
   fork {
     port.req.enqueue(chiselTypeOf(port.req).bits.Lit(
       _.wen -> false.B,
       _.addr -> addr.U,
       _.wdata.a -> 0.U,
       _.wdata.b -> 0.U,
       _.wdata.last -> false.B
     ))
   }. fork {
     port.rsp.expectDequeue(chiselTypeOf(port.rsp).bits.Lit(
       _.rdata.a -> data._1.U,
       _.rdata.b -> data._2.U,
       _.rdata.last -> data._3.B
     ))
   }.join()
   c.clock.step()
 }


   behavior of "RegStore"
  it should "Initialize correctly" in {
    test(new RegStore(new MyData, p)) { c =>
      initClocks(c)
      c.io.rPorts.map(_.req.ready.expect(true.B))
      c.io.wPorts.map(_.req.ready.expect(true.B))
    }
  }
  it should "it should write and read data" in {
    test(new RegStore(new MyData, p)) { c =>
      initClocks(c)
      c.io.rPorts.map(_.req.ready.expect(true.B))
      val t = (10,12,true)
      writeData(c, t, 8, c.io.wPorts(0))
      fork {
        expectData(c,t,8,c.io.rPorts(0))
      }.fork {
        expectData(c,t,8,c.io.rPorts(1))
    }.fork {
      expectData(c,t,8,c.io.rPorts(2))
    }.fork {
        expectData(c,t,8,c.io.rPorts(3))
      }.fork {
        expectData(c,t,8,c.io.rPorts(4))
      }.fork {
        expectData(c,t,8,c.io.rPorts(5))
      }.fork {
        expectData(c,t,8,c.io.rPorts(6))
      }.fork {
        expectData(c,t,8,c.io.rPorts(7))
      }.join()
  }
  }
}
