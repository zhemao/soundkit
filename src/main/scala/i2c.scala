package soundkit

import Chisel._
import Chisel.AdvTester._

class I2CIO extends Bundle {
  val sclk = Bool(OUTPUT)
  // I2C is actually tri-state with pull-up resistors
  // Setting sdat_out to true will allow input
  val sdat_out = Bool(OUTPUT)
  val sdat_in = Bool(INPUT)
}

class I2CController extends Module {
  val io = new Bundle {
    val i2c = new I2CIO
    val ack = Valid(Bool())
    val data = Decoupled(UInt(width = 24)).flip
  }

  val data = Reg(UInt(width = 24))
  val sclk_switch = Counter(64).inc()
  val sclk = Reg(init = Bool(false))

  when (sclk_switch) { sclk := !sclk }

  val set_sclk = sclk_switch && !sclk
  val clr_sclk = sclk_switch && sclk

  val sdat = Reg(init = Bool(true))
  val ack = Reg(init = Bool(true))
  val bit_ctr = Counter(8)
  val byte_ctr = Counter(3)

  val (s_idle :: s_start :: s_data :: s_ack :: s_stop :: Nil) = Enum(Bits(), 5)
  val state = Reg(init = s_idle)

  val clock_en = Reg(init = Bool(false))

  io.i2c.sclk := !clock_en || sclk
  io.i2c.sdat_out := sdat

  io.data.ready := (state === s_idle)
  io.ack.valid := (state === s_stop) && clr_sclk
  io.ack.bits := ack

  switch (state) {
    is (s_idle) {
      when (io.data.valid) {
        data := io.data.bits
        ack := Bool(false)
        sdat := Bool(true)
        state := s_start
      }
    }
    is (s_start) {
      when (set_sclk) { sdat := Bool(false) }
      when (clr_sclk) { state := s_data; clock_en := Bool(true) }
    }
    is (s_data) {
      when (set_sclk) {
        sdat := data(23)
      }
      when (clr_sclk) {
        data := data << UInt(1)
        when (bit_ctr.inc()) { state := s_ack }
      }
    }
    is (s_ack) {
      when (set_sclk) { sdat := Bool(true) }
      when (clr_sclk) {
        ack := ack || io.i2c.sdat_in
        when (byte_ctr.inc()) {
          state := s_stop
        } .otherwise {
          state := s_data
        }
      }
    }
    is (s_stop) {
      when (set_sclk) {
        clock_en := Bool(false)
        sdat := Bool(false)
      }
      when (clr_sclk) {
        sdat := Bool(true)
        state := s_idle
      }
    }
  }
}

class I2CConfig extends Module {
  val io = new Bundle {
    val i2c = new I2CIO
    val finished = Bool(OUTPUT)
  }

  val config = new Array[Int](11)

  config(0) = 0x0c10 // power on everything except out
  config(1) = 0x0017 // left input
  config(2) = 0x0217 // left output
  config(3) = 0x0479 // left output
  config(4) = 0x0679 // right output
  config(5) = 0x08d4 // analog path
  config(6) = 0x0a04 // digital path
  config(7) = 0x0e01 // digital IF
  config(8) = 0x1020 // sampling rate
  config(9) = 0x0c00 // power on everything
  config(10) = 0x01201 // activate

  val config_rom = ROM(config.map(UInt(_, 16)))
  val rom_addr = Reg(init = UInt(0, 4))

  val i2c_addr = Wire(init = UInt(0x34, 8))
  val i2c_data = config_rom.read(rom_addr)

  val s_start :: s_ack :: s_done :: Nil = Enum(Bits(), 3)
  val state = Reg(init = s_start)

  val i2c = Module(new I2CController)
  i2c.io.data.valid := (state === s_start)
  i2c.io.data.bits := Cat(i2c_addr, i2c_data)

  io.i2c <> i2c.io.i2c
  io.finished := (state === s_done)

  switch (state) {
    is (s_start) {
      when (i2c.io.data.ready) { state := s_ack }
    }
    is (s_ack) {
      when (i2c.io.ack.valid) {
        when (rom_addr === UInt(10)) {
          state := s_done
        } .otherwise {
          rom_addr := rom_addr + UInt(1)
          state := s_start
        }
      }
    }
    // do nothing in this state
    is (s_done) {}
  }
}

class I2CTester(c: I2CConfig) extends AdvTester(c) {
  // default state of sdat_in is high
  wire_poke(c.io.i2c.sdat_in, 1)

  for (i <- 0 until c.config.size) {
    // check start symbol
    until (peek(c.io.i2c.sdat_out) == 0) { expect(c.io.i2c.sclk, 1) }
    until (peek(c.io.i2c.sclk) == 0) {}

    var value = BigInt(0)
    // for each byte
    for (_ <- 0 until 3) {
      // receive bits
      for (_ <- 0 until 8) {
        until (peek(c.io.i2c.sclk) == 1) {}
        value = (value << 1) | peek(c.io.i2c.sdat_out)
        until (peek(c.io.i2c.sclk) == 0) {}
      }
      // send ack
      wire_poke(c.io.i2c.sdat_in, 0)
      until (peek(c.io.i2c.sclk) == 1) {}
      until (peek(c.io.i2c.sclk) == 0) {}
      wire_poke(c.io.i2c.sdat_in, 1)
    }
    // check stop symbol
    until (peek(c.io.i2c.sclk) == 1) {}
    expect(c.io.i2c.sdat_out, 0)
    until (peek(c.io.i2c.sdat_out) == 1) { expect(c.io.i2c.sclk, 1) }

    val expected = 0x340000 | c.config(i)
    if (value != expected)
      println(f"Expected $expected%x, got $value%x")
  }

  until (peek(c.io.finished) == 1) { expect(c.io.i2c.sclk, 1) }
}

object I2CMain {
  def main(args: Array[String]) {
    chiselMain.run(args, () => new I2CConfig,
      (c: I2CConfig) => new I2CTester(c))
  }
}
