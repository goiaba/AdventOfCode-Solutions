package day02

class Box(val l: Int, val w: Int, val h: Int) {
  val area = (2*l*w + 2*w*h + 2*h*l)
  val volume = l*w*h  
}

object Solution {

  def dimensionsAsList(box: Box) = List(box.l,box.w,box.h) 
    
  def smallestSideArea(box: Box) =
    dimensionsAsList(box).sorted.dropRight(1).foldLeft(1)(_ * _)
    
  def smallestPerimeter(box: Box) =
    dimensionsAsList(box).sorted.dropRight(1).foldLeft(0)(_ + _*2)
  
  def paperNeededToWrapTheBox(box: Box) =
    box.area + smallestSideArea(box)
    
  def ribbonNeededToWrapTheBox(box: Box) =
    box.volume + smallestPerimeter(box)
  
  def totalRequiredWrappingPaper(giftList: List[Box]) =
    giftList.foldLeft(0)(_ + paperNeededToWrapTheBox(_))

  def totalRequiredRibbon(giftList: List[Box]) =
    giftList.foldLeft(0)(_ + ribbonNeededToWrapTheBox(_))

}