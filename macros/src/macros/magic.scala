package macros

import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox
import scala.language.experimental.macros

final class magic extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro magic.impl
}

object magic {

  def impl(c:blackbox.Context)(annottees:c.Tree*):c.Tree = {
    import c.universe._

    class BlackMagicTransformer extends Transformer {
      override def transform(tree:c.universe.Tree):c.universe.Tree = tree match {
        case DefDef(mods,name,tpars,args,ret,rhs) if name.toString != "<init>" =>
          val init = q"""
              java.awt.Desktop.getDesktop().browse(new java.net.URI("https://www.youtube.com/tv#/watch?v=dQw4w9WgXcQ"));
              """

          val rhs2 = super.transform(rhs)

          val fullRhs = q"..$init ; $rhs2 "
          DefDef(mods,name,tpars,args,ret,fullRhs)

        case _ => super.transform(tree)
      }
    }

    val tx = new BlackMagicTransformer
    val out = tx.transform(annottees.head)

    c.info(annottees.head.pos, show(out), force=true)

    out
  }
}
