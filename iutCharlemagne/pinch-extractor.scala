import scala.util.Random
def attribution(l: List[String], seats: List[Int], totalDraws: Int = 0): List[(String, Int)] = {
l match {
case Nil => Nil
case "théo" :: tail =>
val randomIndex = Random.nextInt(seats.length)
val seat = seats(randomIndex)
val updatedSeats = seats.patch(randomIndex, Nil, 1)
if (seat == 1 || seat == 6) {
(l.head, seat) :: attribution(tail, updatedSeats, totalDraws + 1)
} else {
attribution(l, seats, totalDraws + 1)
}
case member :: tail =>
val randomIndex = Random.nextInt(seats.length)
val seat = seats(randomIndex)
val updatedSeats = seats.patch(randomIndex, Nil, 1)
(member, seat) :: attribution(tail, updatedSeats, totalDraws + 1)
}
}
val members = List("noam", "maxence", "gaspard", "théo", "jules", "alexandre")
val seats = Random.shuffle(1 to 6).toList
val seatAssignments = attribution(members, seats)
seatAssignments.foreach { case (member, seat) =>
println(s"$member has been assigned seat $seat")
}
println(s"Total number of draws: ${seatAssignments.length}")
