case class Id(value: Long) extends AnyVal

trait Mutual_friends_suggestions {
  // given
  def getFriends(id: Id): Set[Id]

  // implement suggestions for friends who have the most mutual friends with user
  def suggestedFriends(id: Id, limit: Int): Set[Id] = {
    require(limit > 0)
    require(id != null)

    val friends = getFriends(id)
    val friendsOfFriends =
      friends.toSeq
        .flatMap(f => (getFriends(f).diff(friends) - id).toSeq)

    friendsOfFriends
      .groupBy(identity)
      .toSeq
      .sortBy(_._2.size)(Ordering[Int].reverse)
      .take(limit)
      .map(_._1)
      .toSet
  }
}
