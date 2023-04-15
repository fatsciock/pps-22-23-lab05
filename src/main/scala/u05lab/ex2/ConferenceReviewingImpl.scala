package u05lab.ex2

import u05lab.ex2.Question.*
import u05lab.ex2.ConferenceReviewing

class ConferenceReviewingImpl extends ConferenceReviewing:
  private var reviews: List[(Int, Map[Question, Int])] = List()

  override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
    reviews = reviews.appended((article, scores))

  override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
    loadReview(article, Map(
      RELEVANCE -> relevance,
      SIGNIFICANCE -> significance,
      CONFIDENCE -> confidence,
      FINAL -> fin))

  override def orderedScores(article: Int, question: Question): List[Int] =
    reviews.filter(r => r._1 == article).map(a => a._2(question)).sorted

  override def averageFinalScore(article: Int): Double =
    average(reviews.filter(r => r._1 == article).map(r => r._2(FINAL)))

  override def acceptedArticles(): Set[Int] =
    reviews.filter((id, q) => averageFinalScore(id) >= 5 && q(RELEVANCE) >= 8).map((id, _) => id).toSet

  override def sortedAcceptedArticles(): List[(Int, Double)] =
    acceptedArticles().map(id => (id, averageFinalScore(id))).toList.sortWith(_._2 < _._2)

  override def averageWeightedFinalScoreMap(): Map[Int, Double] =
    reviews.map((id, _) => id -> weightedFinalScore(id)).toMap

  private def weightedFinalScore(article: Int): Double =
    average(reviews.filter(r => r._1 == article).map(r => r._2(CONFIDENCE) * r._2(FINAL) / 10.0))

  private def average(marks: List[Double]): Double =
    marks.sum / marks.size

