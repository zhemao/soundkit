package soundkit

import Chisel._

class AudioIO extends Bundle {
  val adclrck = Bool(OUTPUT)
  val adcdat = Bool(INPUT)
  val daclrck = Bool(OUTPUT)
  val dacdat = Bool(OUTPUT)
  val bclk = Bool(OUTPUT)
}

case object AudioSampleWidth extends Field[Int]
case object AudioNChannels extends Field[Int]
case object AudioBclkDivide extends Field[Int]
case object AudioLrckDivide extends Field[Int]

trait AudioParameters extends UsesParameters {
  val audioSampleWidth = params(AudioSampleWidth)
  val audioNChannels = params(AudioNChannels)
  val audioBclkDivide = params(AudioBclkDivide)
  val audioLrckDivide = params(AudioLrckDivide)
  val stereoAudio = audioNChannels == 2
}

abstract class AudioModule extends Module with AudioParameters
abstract class AudioBundle extends Bundle with AudioParameters

class AudioStreamIO extends AudioBundle {
  val out = Decoupled(UInt(width = audioSampleWidth))
  val in = Decoupled(UInt(width = audioSampleWidth)).flip
}

class Codec extends AudioModule {
  val io = new Bundle {
    val streams = Vec(new AudioStreamIO(), audioNChannels).flip
    val aud = new AudioIO
  }

  require(audioBclkDivide % 2 == 0)
  require(audioNChannels > 0 && audioNChannels <= 2)

  val (bclk_count, bclk_switch) = Counter(Bool(true), audioBclkDivide / 2)
  val bclk = Reg(init = Bool(false))

  when (bclk_switch) { bclk := !bclk }

  val set_bclk = bclk_switch && !bclk
  val clr_bclk = bclk_switch && bclk
  val (lrck_count, lrck_switch) = Counter(
    clr_bclk, audioLrckDivide / audioBclkDivide / 2)
  val lrck = Reg(init = Bool(true))

  when (lrck_switch) { lrck := !lrck }

  val shift_out = Reg(init = UInt(0, audioSampleWidth))
  val shift_in = Reg(init = UInt(0, audioSampleWidth))

  when (clr_bclk) { shift_out := shift_out << UInt(1) }
  when (set_bclk && lrck_count < UInt(audioSampleWidth)) {
    shift_in := Cat(shift_in(audioSampleWidth - 2, 0), io.aud.adcdat)
  }

  for (channel <- 0 until audioNChannels) {
    val stream = io.streams(channel)
    val set_capture_valid = Wire(init = Bool(false))
    val last_capture_bit = lrck_count === UInt(audioSampleWidth - 1)
    val capture_valid = Reg(init = Bool(false))

    stream.in.valid := capture_valid
    stream.in.bits := shift_in

    if (channel == 0) {
      stream.out.ready := lrck_switch && !lrck
      set_capture_valid := set_bclk && last_capture_bit && lrck
    } else {
      stream.out.ready := lrck_switch && lrck
      set_capture_valid := set_bclk && last_capture_bit && !lrck
    }

    when (set_capture_valid) { capture_valid := Bool(true) }
    when (stream.in.fire() || lrck_switch) { capture_valid := Bool(false) }

    when (stream.out.fire()) { shift_out := stream.out.bits }
  }

  if (!stereoAudio) {
    val saved_sample = Reg(init = UInt(0, audioSampleWidth))
    val stream = io.streams.head
    when (stream.out.fire()) { saved_sample := stream.out.bits }
    when (lrck_switch && lrck) { shift_out := saved_sample }
  }

  io.aud.bclk := bclk
  io.aud.adclrck := lrck
  io.aud.daclrck := lrck
  io.aud.dacdat := shift_out(audioSampleWidth - 1)
}

class CodecTester(c: Codec) extends Tester(c, false) {
  val nTests = 4

  for (_ <- 0 until nTests) {
    val samp = rnd.nextInt(1 << c.audioSampleWidth)
    poke(c.io.streams(0).out.valid, 1)
    poke(c.io.streams(0).out.bits, samp)

    while (peek(c.io.streams(0).out.ready) == 0) { step(1) }

    step(1)
    poke(c.io.streams(0).out.valid, 0)

    var word = BigInt(0)

    for (j <- c.audioSampleWidth - 1 to 0 by -1) {
      poke(c.io.aud.adcdat, (samp >> j) & 1)
      step(c.audioBclkDivide / 2)
      word = (word << 1) | peek(c.io.aud.dacdat)
      step(c.audioBclkDivide / 2)
    }

    if (word != samp)
      println(f"expected output $samp%x, got $word%x")

    while (peek(c.io.streams(0).in.valid) == 0) { step(1) }

    word = peek(c.io.streams(0).in.bits)
    if (word != samp)
      println(f"expected input $samp%x, got $word%x")
  }
}

object CodecMain {
  def main(args: Array[String]) {
    chiselMain.run(args, () => new Codec, (c: Codec) => new CodecTester(c))
  }
}
