package fix

import scalafix.v1._
import scala.meta._
import metaconfig.Configured

case class IndentationSyntaxParameters(
  addEndMarkers: Boolean
)

object IndentationSyntaxParameters {
  val default = IndentationSyntaxParameters(false)
  implicit val surface: metaconfig.generic.Surface[IndentationSyntaxParameters] = metaconfig.generic.deriveSurface[IndentationSyntaxParameters]
  implicit val decoder: metaconfig.ConfDecoder[IndentationSyntaxParameters] = metaconfig.generic.deriveDecoder(default)
}

class IndentationSyntax(params: IndentationSyntaxParameters)
    extends SyntacticRule("IndentationSyntax") {
  
  def this() = this(IndentationSyntaxParameters.default)

  override def withConfiguration(config: Configuration): Configured[Rule] = 
    config.conf.getOrElse("IndentationSyntax")(this.params).map(newParams => new IndentationSyntax(newParams))
  
  override def fix(implicit doc: SyntacticDocument): Patch = {
    val isLeftBrace  = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.LeftBrace]
    val isRightBrace = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.RightBrace]

    val isWhitespace = (t: Token) => t.isInstanceOf[scala.meta.tokens.Token.Whitespace]

    doc.tree.collect {
      case block: Term.Block => Patch.empty
    }.asPatch
  }
}
