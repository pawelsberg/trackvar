package com.pawelsberg.scala.trackvar

import java.util.concurrent.TimeUnit
import java.time.Instant
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}
// ------------------------------------------TIME------------------------------------------>
// ===============changes with timestamps==============  ============flattened==============
// now, writing period, reading instant, reading period, flattened instant, flattened period
// |   writing only  |  |        reading only         |  |      no operations allowed      |

case class InputOutput(
                        updatedAt: Instant,
                        changes: List[(Instant, Char)] = List(), // todo - make generic
                        writingDuration: Duration = Duration(50, TimeUnit.MILLISECONDS),
                        readingDuration: Duration = Duration(1, TimeUnit.SECONDS),
                        flattened: String = ""
                      ):
  def readingInstant(now: Instant = updatedAt): Instant = now.sub(writingDuration)
  def flattenedInstant(now: Instant = updatedAt): Instant = readingInstant(now).sub(readingDuration)

  private def flattenedChangesNotAfter(at: Instant): String =
    changes.notAfter(at).sortChronologically.asString

  def write(now: Instant, char: Char, at: Instant): Try[InputOutput] =
    if now.isBefore(at)
    then Failure(IllegalArgumentException("Current time is before the time of the event"))
    else
      update(now) match
        case Success(io) =>
          if at.isAfter(io.readingInstant())
          then Success(io.copy(changes = changes.appended((at,char))))
          else Failure(IllegalArgumentException("Trying to write too far in past"))
        case Failure(exception) => Failure(exception)

  def update(now: Instant): Try[InputOutput] =
    if now.isBefore(updatedAt)
    then Failure(IllegalArgumentException("Current time is before updateAt"))
    else
      val newFlattenedInstant = flattenedInstant(now)
      Success(
        copy(
          updatedAt = now,
          changes = changes.after(newFlattenedInstant),
          flattened = flattened + changes.notAfter(newFlattenedInstant).sortChronologically.asString
        )
      )

  def readAt(now: Instant, at: Instant): Try[(String,InputOutput)] =
    if now.isBefore(at)
    then Failure(IllegalArgumentException("Current time is before the read time"))
    else
      update(now) match
        case Success(io) =>
          if at.isAfter(io.flattenedInstant()) && !at.isAfter(io.readingInstant())
          then Success((io.flattened + io.flattenedChangesNotAfter(at),io))
          else Failure(IllegalArgumentException("Trying to read out of bounds"))
        case Failure(exception) => Failure(exception)

  def read(now: Instant): Try[(String,InputOutput)] =
    update(now) match
      case Success(io) => io.readAt(now, readingInstant(now))
      case Failure(exception) => Failure(exception)

// Tools
extension (i: Instant)
  def sub(d: Duration): Instant = i.minusNanos(d.toNanos)

extension (l:List[(Instant, Char)])
  def after(i: Instant):List[(Instant, Char)] = l.filter((t, _) => t.isAfter(i))
  def notAfter(i: Instant):List[(Instant, Char)] = l.filterNot((t, _) => t.isAfter(i))
  def sortChronologically:List[(Instant, Char)] = l.sortWith((a, b) => a._1.isBefore(b._1))
  def asString:String = String(l.map(_._2).toArray)

// Testing
case object InputOutput:
  def test(): Unit =
    val io1 = InputOutput(Instant.now())
    val io2 = io1.write(Instant.now(),'a',Instant.now()) match
      case Success(io) => io.write(Instant.now(),'b', Instant.now()) match
        case Success(io_) => io_
    Thread.sleep(5000)
    val io3 = io2.write(Instant.now(),'c',Instant.now()) match
      case Success(io) => io
    val rd = io3.read(Instant.now()) match
      case Success(res)=>res
    println(rd._1)
    Thread.sleep(100)
    val rd2 = rd._2.readAt(Instant.now(),Instant.now().sub(Duration(60, TimeUnit.MILLISECONDS))) match
      case Success(res)=>res
    println(rd2._1)

@main
def main(): Unit = {
  InputOutput.test()
}


