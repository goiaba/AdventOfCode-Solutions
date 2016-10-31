package day01

object Solution {
  
  def step1(list: List[Char]) = {
    list.foldLeft(0)((acc,elm) => if (elm == '(') acc+1 else acc-1)
  }
  
  @deprecated("Generates the wrong answer if the basement is never reached", "10/31/2016")
  def step2UsingFoldLeft(list: List[Char]): Int = {
    list.foldLeft((0,0)) { 
      case ((idx, sum), el) => 
        if (sum == -1) (idx, sum) 
        else (idx+1, if (el == '(') sum+1 else sum-1)
      }._1
  }
  
  /**
   * This is a better solution than the one using foldLeft, since 
   *  the recursion can be stopped one step after reaching the 
   *  basement (sum == -1). Plus, we do not return the wrong result
   *  when the basement is not reached.
   */
  def step2UsingTailRecursion(list: List[Char]): Int = {
		def iter(l: List[Char], sum: Int, index: Int): Int = l match {
			case Nil =>
				if (sum == -1) index
				else throw new NoSuchElementException("Never reached the basement")
			case h::t =>
				if (sum == -1) iter(Nil, sum, index)
				else iter(t, if (h == '(') sum+1 else sum-1, index+1)
		}
		iter(list, 0, 0)
	} 
}