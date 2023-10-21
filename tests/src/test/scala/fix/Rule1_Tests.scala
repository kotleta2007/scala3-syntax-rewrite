package fix

import scalafix.testkit.AbstractSemanticRuleSuite
import org.scalatest.funsuite.AnyFunSuiteLike
import scalafix.testkit.AbstractSyntacticRuleSuite

class RuleSuite extends AbstractSemanticRuleSuite with AnyFunSuiteLike {
  // Doesn't really work; bugs with Scalameta

  /*
  val baseDirectory = new File(".").getCanonicalPath

  val inputDirectory = new File(baseDirectory + "/input/src/main/scala/fix")
  val outputDirectory = new File(baseDirectory + "/output/src/main/scala/fix")

  val rule = new Rule1()
  
  for (inputFile <- inputDirectory.listFiles()) {
    val maybeOutputFile = inputDirectory.listFiles().find(_.getName() == inputFile.getName())
    
    maybeOutputFile match {
      case Some(outputFile) => 
        val inputFileContents  = Files.readString(inputFile.toPath())
        val outputFileContents = Files.readString(outputFile.toPath())
        
        check(rule, "Rule 1 Test: " + inputFile.getName(), inputFileContents, outputFileContents)
      case None => ()
    }
  }
  */
  runAllTests()
}