package day02;

import day02.SolutionFixtures.fixture
import day02.Solution._
import org.scalatest.FunSuite

class SolutionTest extends FunSuite {

  test("Amount of paper to wrap all the gifts given by the site") {
    assert(totalRequiredWrappingPaper(fixture) == 1606483)    
  }
  
  test("Amount of ribbon to wrap all the gifts given by the site") {
    assert(totalRequiredRibbon(fixture) == 3842356)
  }
}
