package u05lab.ex2

import u05lab.ex2.Question.RELEVANCE

enum Question:
  case RELEVANCE
  case SIGNIFICANCE
  case CONFIDENCE
  case FINAL

trait ConferenceReviewing:
  def loadReview(article: Int, scores: Map[Question, Int]): Unit
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit
  def orderedScores(article: Int, question: Question): List[Int]
  def averageFinalScore(article: Int): Double
  def acceptedArticles(): Set[Int]
  def sortedAcceptedArticles(): List[(Int,Double)]
  def averageWeightedFinalScoreMap(): Map[Int, Double]

import Question.*

class ConferenceReviewingImpl(var reviews: List[(Int, Map[Question, Int])]) extends ConferenceReviewing:
  override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
    reviews = reviews.appended((article, scores))

  override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
    reviews = reviews.appended((article, Map().updated(RELEVANCE, relevance)
      .updated(SIGNIFICANCE, significance)
      .updated(CONFIDENCE, confidence)
      .updated(FINAL, fin)
    ))

  override def orderedScores(article: Int, question: Question): List[Int] =
    reviews.filter(r => r._1 == article).map(a => a._2(question)).sorted

  override def averageFinalScore(article: Int): Double =
    average(reviews.filter(r => r._1 == article).map(r => r._2(FINAL)))

  override def acceptedArticles(): Set[Int] =
    reviews.filter(r => averageFinalScore(r._1) >= 5 && r._2(RELEVANCE) >= 8).map(r => r._1).toSet

  override def sortedAcceptedArticles(): List[(Int, Double)] =
    acceptedArticles().map(a => (a, averageFinalScore(a))).toList.sorted((a1, a2) => a1._2.compareTo(a2._2))

  override def averageWeightedFinalScoreMap(): Map[Int, Double] =
    reviews.map(r => (r._1, weightedFinalScore(r._1))).toMap

  private def weightedFinalScore(article: Int): Double =
    average(reviews.filter(r => r._1 == article).map(r => r._2(CONFIDENCE) * r._2(FINAL) / 10.0))

  private def average(marks: List[Double]): Double =
    marks.sum / marks.size
