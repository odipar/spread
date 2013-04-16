package spread

object Engine {

  trait Expr {
    def reduce: Expr
    def source: Expr

    def label: Option[Label]
    def localLabels: Set[Expr]
    def globalLabels: Set[Expr]

    def bind(b: Map[Expr,Expr]): Expr

    def asString: String
  }

  trait Atom extends Expr {
    def reduce = Reduction(this,this)
    def source = this
    def label = None
    def bind(b: Map[Expr,Expr]): Expr = this
    def localLabels = Set()
    def globalLabels = Set()
  }

  case class Symbol(s: String) extends Atom {
    def asString = s
  }

  case object NilExpr extends Atom {
    def asString = ""
  }

  case class IExpr(i: Int) extends Atom {
    def asString = i.toString
  }

  case class Reduction(v: Expr, source: Expr) extends Expr {
    def reduce = this
    def label = None
    def bind(b: Map[Expr,Expr]) = {
      val bb = b filterKeys localLabels // TODO: use fast set intersection
      if (bb.isEmpty) this
      else source.bind(bb).reduce
    }
    def localLabels = source.localLabels
    def globalLabels = source.globalLabels

    def asString = {
      if (v == source) v.asString
      else v.asString + "@(" + source.asString + ")"
    }
  }

  trait Label {
    def content: Expr
    def isLocal: Boolean
    def asString: String
  }

  case class LocalLabel(content: Expr) extends Label {
    def isLocal = true
    def asString = content.asString+"'"
  }

  case class GlobalLabel(content: Expr) extends Label {
    def isLocal = false
    def asString = content.asString+"''"
  }

  case class UnboundExpr(l: Label) extends Expr {
    def reduce = this
    def source = this
    def label = Some(l)
    def bind(b: Map[Expr,Expr]): Expr = {
      if (b.contains(l.content)) BoundExpr(l,b.get(l.content).get)
      else this
    }
    def localLabels = if (l.isLocal) Set(l.content) else Set()
    def globalLabels = if (!l.isLocal) Set(l.content) else Set()

    def asString = l.asString
  }

  case class BoundExpr(l: Label, e: Expr) extends Expr {
    def reduce = Reduction(e,this)
    def source = this
    def label = Some(l)
    def bind(b: Map[Expr,Expr]): Expr = {
      if (b.contains(l.content)) BoundExpr(l,b.get(l.content).get)
      else this
    }
    def localLabels = if (l.isLocal) Set(l.content) else Set()
    def globalLabels = if (!l.isLocal) Set(l.content) else Set()

    def asString = l.asString + e.asString
  }

  case class IAdd(arg1: Expr, arg2: Expr) extends Expr {
    def label = None
    def source = this
    def reduce: Expr = {
      val vadd = IAdd(arg1.reduce,arg2.reduce)

      (vadd.arg1,vadd.arg2) match {
        case (Reduction(IExpr(i1),_),Reduction(IExpr(i2),_)) => Reduction(IExpr(i1+i2),vadd)
        case _ => vadd
      }
    }
    def bind(b: Map[Expr,Expr]) = {
      val bb = b filterKeys localLabels // TODO: use fast set intersection
      if (bb.isEmpty) this
      else IAdd(arg1.bind(bb),arg2.bind(bb))
    }
    lazy val localLabels = arg1.localLabels ++ arg2.localLabels // TODO: use fast set union
    lazy val globalLabels = arg1.globalLabels ++ arg2.globalLabels

    def asString = arg1.asString + " " + arg2.asString + " +"
  }

  case class IMul(arg1: Expr, arg2: Expr) extends Expr {
    def label = None
    def source = this
    def reduce: Expr = {
      val vmul = IMul(arg1.reduce,arg2.reduce)

      (vmul.arg1,vmul.arg2) match {
        case (Reduction(IExpr(i1),_),Reduction(IExpr(i2),_)) => Reduction(IExpr(i1*i2),vmul)
        case _ => vmul
      }
    }
    def bind(b: Map[Expr,Expr]) = {
      val bb = b filterKeys localLabels // TODO: use fast set intersection
      if (bb.isEmpty) this
      else IMul(arg1.bind(bb),arg2.bind(bb))
    }
    lazy val localLabels = arg1.localLabels ++ arg2.localLabels // TODO: use fast set union
    lazy val globalLabels = arg1.globalLabels ++ arg2.globalLabels

    def asString = arg1.asString + " " + arg2.asString + " *"

  }

  case class Red(arg1: Expr) extends Expr {
    def label = None
    def source = this
    def reduce: Expr = {
      val vred = Red(arg1.reduce)

      (vred.arg1) match {
        case Reduction(MExpr(b),_) => {
            var nm:Map[Expr,Expr] = Map()
            for (k <- b.keys) {
              val v = b.get(k).get
              nm = nm + (k -> v.reduce)
            }
            Reduction(MExpr(nm),vred)
        }
        case _ => vred
      }
    }
    def bind(b: Map[Expr,Expr]) = {
      val bb = b filterKeys localLabels // TODO: use fast set intersection
      if (bb.isEmpty) this
      else Red(arg1.bind(bb))
    }
    lazy val localLabels = arg1.localLabels
    lazy val globalLabels = arg1.globalLabels

    def asString = arg1.asString + " $"
  }
  case class Bind(arg1: Expr, arg2: Expr) extends Expr {
    def label = None
    def source = this
    def reduce: Expr = {
      val vbind = Bind(arg1.reduce,arg2.reduce)

      (vbind.arg1,vbind.arg2) match {
        case (Reduction(MExpr(m1),_),Reduction(MExpr(m2),_)) => {
          Reduction(MExpr(m1).bind(m2),this)
        }
        case _ => vbind
      }
    }
    def bind(b: Map[Expr,Expr]) = {
      val bb = b filterKeys localLabels // TODO: use fast set intersection
      if (bb.isEmpty) this
      else Bind(arg1.bind(bb),arg2.bind(bb))
    }
    lazy val localLabels = arg1.localLabels ++ arg2.localLabels // TODO: use fast set union
    lazy val globalLabels = arg1.globalLabels ++ arg2.globalLabels

    def asString = arg1.asString + " " + arg2.asString + " !"
  }

  case class MExpr(m: Map[Expr,Expr]) extends Expr {
    def reduce = Reduction(this,this)
    def source = this
    def label = None
    def bind(b: Map[Expr,Expr]): Expr = {
      var nm:Map[Expr,Expr] = Map()
      for (k <- m.keys) {
        val v = m.get(k).get
        nm = nm + (k -> v.bind(b))
      }
      MExpr(nm)
    }
    lazy val localLabels = {
      var ll: Set[Expr] = Set()
      for (k <- m.keys) {
        ll = ll ++ m.get(k).get.localLabels
      }
      ll
    }
    def globalLabels = Set()
    def asString = {
      var i = 0
      var s = "["
      for (k <- m.keys) {
        if (i > 0) { s = s + ", "}
        s = s + k.asString + "=" + m.get(k).get.asString
        i = i + 1
      }
      s + "]"
    }
  }
}

// Implementation notes
//
// Each map should carry the following weak functional memoization tables
// - joinLocal(localLabels,LocalLabels) => localLabels
// - joinGlobal(globalLabels,globalLabels) => globalLabels
// - bind(expr,map) => map
// - reduce(expr) => expr

