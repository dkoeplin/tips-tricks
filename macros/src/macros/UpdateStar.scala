package macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox
import scala.language.experimental.macros

/**
  * Usage:
  *
  * class X {
  *   @UpdateStar
  *   def update[T](indices: Int*)(rhs: T) = ...
  * }
  *
  */

/**
  * class X {
  *   def __update[T](indices: Int*)(rhs: T) = ...
  *
  *   def update(values: Any*) = ** Expand to __update call **
  * }
  *
  * x(values*) = lastValue
  *
  * x.__update(values)(lastValue)
  *
  */

final class UpdateStar extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro UpdateStar.impl
}

object UpdateStar {
  def impl(c: blackbox.Context)(annottees: c.Tree*):c.Tree = {
    import c.universe._

    /** Step 2 **/
    def renameMethod(method: DefDef, newName: String): DefDef = {
      DefDef(
        method.mods,        // Method modifiers (e.g. private, protected, abstract, final, etc.)
        TermName(newName),  // Method name
        method.tparams,     // Method type parameters
        method.vparamss,    // Method parameters
        method.tpt,         // Return type
        method.rhs          // Implementation
      )
    }

    /** Step 3 **/
    def createUpdateAny() = q"""
      import scala.language.experimental.macros ;
      def update(values: Any*): Unit = macro UpdateStar.dispatcher ;
    """
    /** Step 1 **/
    val func = annottees.head
    val replacement = func match {
      case definition@DefDef(modifiers,methodName,typeParameters,args,returnType,funcRhs) if methodName.toString == "update" =>
        val renamed = renameMethod(definition, "__update")
        val newMethod = createUpdateAny()

        q"$renamed ; ..$newMethod"

      case func =>
        c.abort(func.pos, "UpdateStar annotation can only be used on a update defs")
    }

    /** Step 5 **/
    replacement
    //annottees.head
  }

  /** Step 4 **/
  def dispatcher(c: blackbox.Context)(values: c.Tree*): c.Tree = {
    import c.universe._

    val prefix = c.prefix.tree
    val indices = values.dropRight(1)   // All values except the last one (the indices)
    val rhs = values.last               // The last value (the right hand side in the update)
    q"$prefix.__update(..$values)($rhs)"
  }

}