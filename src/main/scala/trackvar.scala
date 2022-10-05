package com.pawelsberg.scala.trackvar

case class TrackVar[A](value: A, history: List[A] = List[A]()):
  def set(newValue: A) : TrackVar[A] = TrackVar(newValue, history.appended(value))
  def get : A = value
  def cleanHistory : TrackVar[A] = TrackVar(value)

case object TrackVar:
  def test(): Unit =
    val tv1 = TrackVar(1)
    println(tv1.get)
    val tv_1 = tv1.cleanHistory
    println(tv_1.get)
    val tv2 = tv1.set(5)
    println(tv2.get)
    val tv3 = tv2.set(8)
    println(tv3.get)
    val tv_3 = tv3.cleanHistory
    println(tv_3.get)

