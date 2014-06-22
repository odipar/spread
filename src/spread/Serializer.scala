package spread

object Serializer {
  import IncrementalMemoization._
  import IncrementalArithmetic._
  import IncrementalTreap._
  import scala.xml._
  import scala.xml._
  import IncrementalTreap._

  case class IDMap(m: Map[Any, Long], id: Long) {
    def contains(v: Any) = m.contains(v)
    def put(v: Any) = IDMap(m + (v -> id), id + 1)
  }

  def toIds(v: Any): IDMap = {
    toIds(IDMap(Map(), 0), v)
  }
  def toIds(m: IDMap, v: Any): IDMap = {
    if (m.contains(v)) m
    else {
      val nm = m.put(v)
      v match {
        case t: F1[_, _] => toIds(toIds(nm, t.reduce), t.a)
        case t: FF1[_, _] => toIds(nm, t.a)
        case t: F2[_, _, _] => toIds(toIds(toIds(nm, t.reduce), t.a), t.b)
        case t: FF2[_, _, _] => toIds(toIds(nm, t.a), t.b)
        case t: F3[_, _, _, _] => toIds(toIds(toIds(toIds(nm, t.reduce), t.a), t.b), t.c)
        case t: FF3[_, _, _, _] => toIds(toIds(toIds(nm, t.a), t.b), t.c)
        /* case i: EI[_] => i.x match {
           case t: FTreap[_,_] => toIds(nm,t)
           case _ => nm
         }
         case t: FTreap[_,_] => {
           if (!t.isEmpty) {
             val l = t.left
             var r = t.right
             if (!l.isEmpty) {
               if (!r.isEmpty) toIds(toIds(nm,l),r)
               else toIds(nm,l)
             }
             else {
               if (!r.isEmpty) toIds(nm,r)
               else nm
             }
           }
           else nm
         }*/
        case _ => nm
      }
    }
  }
  def toID(i: Any)(implicit m: IDMap): String = {
    m.m(i).toString
  }
  def toNodes(implicit m: IDMap): Node = {
    val mm = m.m
    var ss: NodeSeq = <start></start>
    for (i <- mm) {
      val n = {
        <node id={i._2.toString}>
          {toSub(i._1)}
        </node>
      }
      ss = ss :+ n
    }
    <result>
      {ss}
    </result>
  }
  def toSub(t: Any)(implicit m: IDMap): Node = t match {
    /*case t: NFTreap[_,_] => {
     <treap>
        <content value="'"/>
      </treap>
    }
    case t: FTreap[_,_] => {
      <treap>
        <content value={t.value.toString}/>
        { if (!t.left.isEmpty) { <left id={toID(t.left)}/> } }
        { if (!t.right.isEmpty) { <right id={toID(t.right)}/> } }
      </treap>
    } */
    case l: F1[_, _] => {
      <f1>
        <function name={l.f.toString}/>
        <arg1 id={toID(l.a)}/>
        <reduce id={toID(l.reduce)}/>
      </f1>
    }
    case l: FF1[_, _] => {
      <f1>
        <function name={l.f.toString}/>
        <arg1 id={toID(l.a)}/>
      </f1>
    }
    case l: F2[_, _, _] => {
      <f2>
        <function name={l.f.toString}/>
        <arg1 id={toID(l.a)}/>
        <arg2 id={toID(l.b)}/>
        <reduce id={toID(l.reduce)}/>
      </f2>
    }
    case l: FF2[_, _, _] => {
      <f2>
        <function name={l.f.toString}/>
        <arg1 id={toID(l.a)}/>
        <arg2 id={toID(l.b)}/>
      </f2>
    }
    case l: F3[_, _, _, _] => {
      <f2>
        <function name={l.f.toString}/>
        <arg1 id={toID(l.a)}/>
        <arg2 id={toID(l.b)}/>
        <arg3 id={toID(l.c)}/>
        <reduce id={toID(l.reduce)}/>
      </f2>
    }
    case l: FF3[_, _, _, _] => {
      <f2>
        <function name={l.f.toString}/>
        <arg1 id={toID(l.a)}/>
        <arg2 id={toID(l.b)}/>
        <arg3 id={toID(l.c)}/>
      </f2>
    }
    /*case i: EI[_] => {
      i.x match {
        case t: FTreap[_, _] => toSub(t)
        case _ => <content value={i.x.toString}/>
      }
    } */
    case x => {
        <content value={x.toString}/>
    }
  }
}
