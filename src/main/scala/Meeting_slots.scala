import scala.collection.mutable.ListBuffer

object Meeting_slots {
  /*
    Given booked slots of 2 people as lists of pairs,
    and bounds on availability time ...
    and a meeting duration in mins,
    return a list of durations when the meeting can be scheduled

    input booked slots are not overlapping, end and begin may touch

    TODO : extend to multiple people
   */

  case class Slot(start: Int, stop: Int) {
    require(start <= stop, s"$start, $stop")

    val duration: Int = stop - start

    def contains(time: Int): Boolean = {
      start <= time && time <= stop
    }

    def isBefore(that: Slot): Boolean = {
      this.stop <= that.start
    }
  }

  def newSlot(time: Int): Slot = Slot(time, time)

  def toMinutesOfDay(time: String): Int = {
    val Array(hours, minutes) = time.split(":").map(s => s.trim.toInt)
    require(hours >= 0 && hours < 24)
    require(minutes >= 0 && minutes < 60)

    hours * 60 + minutes
  }

  def toClock(time: Int): String = {
    val hours = time / 60
    val minutes = time % 60

    s"$hours:$minutes"
  }

  def overlaps(lhs: Slot, rhs: Slot): Boolean = {
    rhs.contains(lhs.start) || rhs.contains(lhs.stop) ||
    lhs.contains(rhs.start) || lhs.contains(rhs.stop)
  }

  def coalesce(lhs: Slot, rhs: Slot): Slot = {
    Slot(math.min(lhs.start, rhs.start), math.max(lhs.stop, rhs.stop))
  }

  def intersection(slots: Seq[Slot]): Slot = {
    // assume all are mutually overlapped
    val start = slots.map(_.start).max
    val stop = slots.map(_.stop).min

    Slot(start, stop)
  }

  def toSlot(slotTimes: (String, String)): Slot = {
    slotTimes match {
      case (start, stop) =>
        Slot(toMinutesOfDay(start), toMinutesOfDay(stop))
    }
  }
  def toSlots(stringSlots: Seq[(String, String)]): Seq[Slot] = {
    stringSlots.map(toSlot)
  }

  def sort(slots: Seq[Slot]): Seq[Slot] = {
    slots.sortBy(s => (s.stop, s.start))
  }

  def merge(lhs: Seq[Slot], rhs: Seq[Slot]): List[Slot] = {
    if (lhs.isEmpty) rhs.toList
    else if (rhs.isEmpty) lhs.toList
    else {
      var lhsPos = 0
      var rhsPos = 0

      val result = ListBuffer.empty[Slot]
      while (lhsPos < lhs.length && rhsPos < rhs.length) {
        val left = lhs(lhsPos)
        val right = rhs(rhsPos)

        if (left.isBefore(right)) {
          result += left
          lhsPos += 1
        } else if (right.isBefore(left)) {
          result += right
          rhsPos += 1
        } else if (overlaps(left, right)) {
          result += coalesce(left, right)
          lhsPos += 1
          rhsPos += 1
        } else {
          throw new IllegalStateException(s"Incomparable $left $right")
        }
      }

      if (lhsPos < lhs.length)
        result ++= lhs.drop(lhsPos)
      else if (rhsPos < rhs.length)
        result ++= rhs.drop(rhsPos)

      result.toList
    }
  }

  // assumes sorted
  def inBetweenSlots(slots: List[Slot]): List[Slot] = {
    slots
      .sliding(2)
      .filterNot(l => overlaps(l(0), l(1)))
      .map(l => Slot(l(0).stop, l(1).start))
      .toList
  }

  // assumes sorted and disjoint
  def boundedSlots(bound: Slot, slots: List[Slot]): List[Slot] = {
    val trimmed =
      slots.filter(slot => bound.start < slot.stop || slot.start < bound.stop)

    if (trimmed.nonEmpty) {
      adjustHead(bound.start, adjustLast(bound.stop, trimmed))
    } else
      trimmed
  }

  def adjustHead(start: Int, slots: List[Slot]): List[Slot] = {
    if (slots.head.start < start) {
      Slot(start, slots.head.stop) +: slots.tail
    } else
      slots
  }

  def adjustLast(stop: Int, slots: List[Slot]): List[Slot] = {
    if (stop < slots.last.stop) {
      slots.init :+ Slot(slots.last.start, stop)
    } else
      slots
  }

  /*

  sort all booked lists

  merge into single list of bookings, like merge sort, coalesce overlapping slots

  get complement list of slots (free slots)
    - take care of free time at start and end of bounds (dummy meetings)

  filter by intersection of effective bounds for each person
    - may need to edit bounds on 1st and last slots

  filter by meeting duration

   */

  def compatibleSlots(bookings1: Seq[Slot],
                      bounds1: Slot,
                      bookings2: Seq[Slot],
                      bounds2: Slot,
                      duration: Int): Seq[Slot] = {

    if (bookings1.isEmpty || bookings2.isEmpty ||
        !overlaps(bounds1, bounds2) ||
        intersection(Seq(bounds1, bounds2)).duration < duration)
      Seq.empty[Slot]
    else {
      val mergedBookings = merge(sort(bookings1), sort(bookings2))
      printSlots("mergedBookings", mergedBookings)

      val boundsIntersection = intersection(Seq(bounds1, bounds2))
      val boundedBookings = boundedSlots(boundsIntersection, mergedBookings)
      printSlots("boundedBookings", boundedBookings)

      val freeSlots = inBetweenSlots(
        newSlot(boundsIntersection.start) +:
          boundedBookings :+
          newSlot(boundsIntersection.stop)
      )
      printSlots("freeSlots", freeSlots)

      freeSlots.filter(_.duration >= duration)
    }
  }

  def compatibleSlots(bookings1: Seq[(String, String)],
                      bounds1: (String, String),
                      bookings2: Seq[(String, String)],
                      bounds2: (String, String),
                      duration: Int): Seq[Slot] = {

    compatibleSlots(
      toSlots(bookings1),
      toSlot(bounds1),
      toSlots(bookings2),
      toSlot(bounds2),
      duration
    )
  }

  def main(args: Array[String]): Unit = {
    val bookings1 =
      Seq(("9:00", "10:30"), ("12:00", "13:00"), ("16:00", "18:00"))
    val bounds1 = ("9:00", "20:00")

    val bookings2 = Seq(
      ("10:00", "11:30"),
      ("12:30", "14:30"),
      ("14:30", "15:00"),
      ("16:00", "17:00")
    )
    val bounds2 = ("10:00", "18:30")

    val duration = 30

    val viableSlots =
      compatibleSlots(bookings1, bounds1, bookings2, bounds2, duration)

    printSlots("viableSlots", viableSlots)
  }

  def printSlots(prefix: String, slots: Seq[Slot]): Unit = {
    println(s"$prefix = ${slots.map(s => (toClock(s.start), toClock(s.stop)))}")
  }
}
