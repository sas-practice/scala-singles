object Max_footfall_ts {
  /*
Given list of triplets, indicating entry/exit from a mall, with timestamp of recording,
find TS when max number of people were inside

input:  data = [
[1487799425, 14, 1],
[1487799425, 4,  0],
[1487799425, 2,  0],
[1487800378, 10, 1],
[1487801478, 18, 0],
[1487801478, 18, 1],
[1487901013, 1,  0],
[1487901211, 7,  1],
[1487901211, 7,  0]
]


output: 1487800378

# since the increase in the number of people 
# in the mall is the highest at that point

List of (ts, num of ppl, entry(1) or exit), sorted by ts
   */

  def main(args: Array[String]): Unit = {
    val input = Vector(
      Footfall(1487799425, 14, true),
      Footfall(1487799425, 4, false),
      Footfall(1487799425, 2, false),
      Footfall(1487800378, 10, true),
      Footfall(1487801478, 18, false),
      Footfall(1487801478, 18, true),
      Footfall(1487801479, 1, true),
      Footfall(1487901013, 1, false),
      Footfall(1487901211, 7, true),
      Footfall(1487901211, 7, false)
    )

    val output = getMaxInsideTs(input)

    println(output)
  }

  case class Footfall(ts: Long, count: Int, isEntry: Boolean)

  def getMaxInsideTs(footfalls: Seq[Footfall]): Option[Long] = {

    if (footfalls.isEmpty) Option.empty[Long]
    else {
      val footfallPerTs =
        footfalls
          .groupBy(_.ts)
          .view
          .mapValues(getNetFootfallInTs)

      val cumulativeFootfallPerTs =
        footfallPerTs.toSeq
          .sortBy(_._1)
          .scanLeft((0L, 0)) {
            case ((_, acc), (ts, count)) =>
              (ts, count + acc)
          }

      Option(cumulativeFootfallPerTs.maxBy(_._2)._1)
    }
  }

  def getNetFootfallInTs(footfalls: Seq[Footfall]): Int = {
    footfalls.map(f => if (f.isEntry) f.count else -f.count).sum
  }
}
