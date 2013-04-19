package spread

object Engine {

  import scala.collection.immutable.SortedSet

  object ExprOrdering extends Ordering[Expr] {
    def compare(e1: Expr, e2: Expr): Int = (e1,e2) match {
      case (NilExpr,NilExpr) => 0
      case (NilExpr,_) => -1
      case (Symbol(s1),Symbol(s2)) => s1.compare(s2)
      case (Symbol(s1),_) => -1
      case (UnboundExpr(l1),UnboundExpr(l2)) => compare(l1.content,l2.content)
      case (UnboundExpr(l1),_) => -1
      case (BoundExpr(l1,e1),BoundExpr(l2,e2)) => {
        val c = compare(e1,e2)
        if (c == 0) compare(l1.content,l2.content)
        else c
      }
      case (BoundExpr(_,e1),e2) => compare(e1,e2)
      case (e1,BoundExpr(_,e2)) => compare(e1,e2)
      case (IExpr(i1),IExpr(i2)) => i1 - i2
      case (IExpr(_),_) => -1
      case (Reduction(r1,s1),Reduction(r2,s2)) => {
        val c = compare(r1,r2)
        if (c == 0) compare(s2,s2)
        else c
      }
      case (Reduction(r1,s1),_) => -1
      case (IMul(a11,a12),IMul(a21,a22)) => {
        val c = compare(a11,a21)
        if (c == 0) compare(a12,a22)
        else c
      }
      case (IMul(_,_),_) => -1
      case (Alternatives(a1),Alternatives(a2)) => {
        compare_s(a1,a2)
      }
      case (Alternatives(_),_) => -1
    }

    def compare_s(s1: SortedSet[Expr],s2: SortedSet[Expr]): Int = {
      if (s1.size > s2.size) -compare_s(s2,s1)
      else {
        var i1 = s1.iterator
        var i2 = s2.iterator
        var c = 0
        while (i1.hasNext && (c==0)) {
          c = compare(i1.next,i2.next())
        }
        c
      }
    }
  }

  implicit val exprOrdering = ExprOrdering

  trait Expr {
    def reduce: Expr
    def source: Expr

    def alternatives: Alternatives

    def label: Option[Label]
    def labels: SortedSet[Expr]

    def bind(b: Map[Expr,Expr]): Expr

    def asString: String
  }

  trait Atom extends Expr {
    def reduce = Reduction(alternatives,this)
    def alternatives = Alternatives(SortedSet[Expr](this))
    def source = this
    def label = None
    def labels = SortedSet[Expr]()

    def bind(b: Map[Expr,Expr]): Expr = this
  }

  case class Alternatives(v: SortedSet[Expr]) extends Expr {
    def alternatives = {
      var aa:SortedSet[Expr] = SortedSet[Expr]()
      for (a <- v) aa = aa ++ a.alternatives.v
      Alternatives(aa)
    }
    def reduce = {
      var aa:SortedSet[Expr] = SortedSet[Expr]()
      for (a <- alternatives.v) aa = aa + a.reduce.alternatives
      Reduction(Alternatives(aa),this)
    }
    def source = this
    def label = None
    lazy val labels = {
      var l:SortedSet[Expr] = SortedSet[Expr]()
      for (a <- v) l = l ++ a.labels
      l
    }

    def bind(b: Map[Expr,Expr]): Expr = {
      var aa:SortedSet[Expr] = SortedSet[Expr]()
      for (a <- v) aa = aa + a.bind(b)
      Alternatives(aa)
    }

    def asString = {
      var i = 0
      val size = v.size
      var s = ""
      if (size > 1) s = s + "{"
      for (a <- v) {
        if (i > 0) { s = s + ", "}
        s = s + a.asString
        i = i + 1
      }
      if (size > 1) s = s + "}"
      s
    }
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
    lazy val labels = v.labels ++ source.labels

    def alternatives = v.alternatives

    def bind(b: Map[Expr,Expr]) = {
      val bb = b filterKeys labels // TODO: use fast set intersection
      if (bb.isEmpty) this
      else {
        val bb2 = bb filterKeys source.labels
        if (bb2.isEmpty) {
          val bb3 = bb filterKeys v.labels
          if (bb3.isEmpty) this
          else Reduction(v.bind(bb3),source)
        }
        else {
          source.bind(b).reduce
        }
      }
    }

    def asString = {
      if (v == source.alternatives) v.asString
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

  case class UnboundExpr(l: Label) extends Expr {
    def reduce = this
    def source = this
    def alternatives = Alternatives(SortedSet[Expr](this))
    def label = Some(l)
    def labels = if (l.isLocal) SortedSet(l.content) else SortedSet[Expr]()
    def bind(b: Map[Expr,Expr]): Expr = {
      if (b.contains(l.content)) BoundExpr(l,b.get(l.content).get)
      else this
    }

    def asString = l.asString
  }

  case class BoundExpr(l: Label, e: Expr) extends Expr {
    def alternatives = Alternatives(SortedSet[Expr](this)) // TODO: iterate through e.alternatives and label them
    def reduce = {
      val nb = BoundExpr(l,e.reduce)
      (nb.e) match {
        case Reduction(v,_) => Reduction(v,nb)
        case _ => this
      }
    }
    def source = this
    def label = Some(l)
    def bind(b: Map[Expr,Expr]): Expr = {
      if (b.contains(l.content)) BoundExpr(l,b.get(l.content).get)
      else BoundExpr(l,e.bind(b))
    }
    lazy val labels = e.labels ++ SortedSet[Expr](l.content)

    def asString = l.asString + e.asString
  }

  case class IAdd(arg1: Expr, arg2: Expr) extends Expr {
    def label = None
    lazy val alternatives = {
     var s:SortedSet[Expr] = SortedSet[Expr]()
     val a1 = arg1.alternatives.v
     val a2 = arg2.alternatives.v
     for (a11 <- a1) {
       for (a22 <- a2) {
        s = s + IAdd(a11,a22)
       }
     }
     Alternatives(s)
    }
    def source = this
    def reduce: Expr = {
      val vadd = IAdd(arg1.reduce,arg2.reduce)

      val a1 = vadd.arg1.alternatives.v
      val a2 = vadd.arg2.alternatives.v

      var s:SortedSet[Expr] = SortedSet[Expr]()

      for (aa1 <- a1) {
         for (aa2 <- a2) {
            (aa1,aa2) match {
              case (IExpr(i1),IExpr(i2)) =>  s = s + IExpr(i1+i2)
              case _ =>
            }
         }
      }
      if (s.isEmpty) vadd
      else Reduction(Alternatives(s),vadd)
    }
    def bind(b: Map[Expr,Expr]) = {
      val bb = b filterKeys labels // TODO: use fast set intersection
      if (bb.isEmpty) this
      else IAdd(arg1.bind(bb),arg2.bind(bb))
    }
    lazy val labels = arg1.labels ++ arg2.labels // TODO: use fast set union

    def asString = arg1.asString + " " + arg2.asString + " +"
  }

  case class IMul(arg1: Expr, arg2: Expr) extends Expr {
    lazy val alternatives = {
      var s:SortedSet[Expr] = SortedSet[Expr]()
      val a1 = arg1.alternatives.v
      val a2 = arg2.alternatives.v
      for (a11 <- a1) {
        for (a22 <- a2) {
          s = s + IMul(a11,a22)
        }
      }
      Alternatives(s)
    }
    def label = None
    def source = this
    def reduce: Expr = {
      val vmul = IMul(arg1.reduce,arg2.reduce)

      //println("arg1: " + vmul.arg1)
      //println("arg2: " + vmul.arg2)


      val a1 = vmul.arg1.alternatives.v
      val a2 = vmul.arg2.alternatives.v

      //println("a1: " + a1)
      //println("a2: " + a2)

      var s:SortedSet[Expr] = SortedSet[Expr]()

      for (aa1 <- a1) {
        for (aa2 <- a2) {
          (aa1,aa2) match {
            case (IExpr(i1),IExpr(i2)) =>  s = s + IExpr(i1*i2).reduce
            case _ =>
          }
        }
      }
      if (s.isEmpty) vmul
      else Reduction(Alternatives(s),vmul)
    }
    def bind(b: Map[Expr,Expr]) = {
      val bb = b filterKeys labels // TODO: use fast set intersection
      if (bb.isEmpty) this
      else IMul(arg1.bind(bb),arg2.bind(bb))
    }
    lazy val labels = arg1.labels ++ arg2.labels // TODO: use fast set union

    def asString = arg1.asString + " " + arg2.asString + " *"

  }

  case class Red(arg1: Expr) extends Expr {
    def label = None
    def alternatives = Alternatives(SortedSet[Expr](this))
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
      val bb = b filterKeys labels // TODO: use fast set intersection
      if (bb.isEmpty) this
      else Red(arg1.bind(bb))
    }
    lazy val labels = arg1.labels

    def asString = arg1.asString + " $"
  }
  case class Bind(arg1: Expr, arg2: Expr) extends Expr {
    def label = None
    def alternatives = Alternatives(SortedSet[Expr](this))
    def source = this
    def reduce: Expr = {
      val vbind = Bind(arg1.reduce,arg2.reduce)

      (vbind.arg1,vbind.arg2) match {
        case (Reduction(MExpr(m1),s1),Reduction(MExpr(m2),_)) => {
          Reduction(MExpr(m1).bind(m2),vbind)
          /*val l = m2 filterKeys s1.labels
          if (l.isEmpty) Reduction(MExpr(m1).bind(m2),vbind)
          else {*/
           // Reduction(s1.bind(m2).reduce,vbind)
          //}
        }
        case _ => vbind
      }
    }
    def bind(b: Map[Expr,Expr]) = {
      val bb = b filterKeys labels // TODO: use fast set intersection
      if (bb.isEmpty) this
      else Bind(arg1.bind(bb),arg2.bind(bb))
    }
    lazy val labels = arg1.labels ++ arg2.labels // TODO: use fast set union

    def asString = arg1.asString + " " + arg2.asString + " !"
  }

  case class MExpr(m: Map[Expr,Expr]) extends Expr {
    def alternatives = Alternatives(SortedSet[Expr](this))
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
    lazy val labels = {
      var ll: SortedSet[Expr] = SortedSet[Expr]()
      for (k <- m.keys) {
        ll = ll ++ m.get(k).get.labels
      }
      ll
    }
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
// - joinLocal(labels,LocalLabels) => labels
// - joinGlobal(globalLabels,globalLabels) => globalLabels
// - bind(expr,map) => map
// - reduce(expr) => expr

