package spread

object Serializer {
  import IncrementalMemoization._
  import IncrementalArithmetic._
  import IncrementalTreap._
  import scala.xml._

  case class IDMap(m: Map[Any,Long],id: Long) {
    def contains(v: Any) = m.contains(v)
    def put(v: Any) = IDMap(m + (v -> id),id + 1)
    def rollback(v: Any) = IDMap(m - (v -> (id - 1)), id - 1)
  }

  def toIds(v: Any): IDMap = {
    toIds(IDMap(Map(),0),v)
  }

  def toIds(m: IDMap, v: Any): IDMap = {
    if (m.contains(v)) m
    else {
      val nm = m.put(v)
      v match {
        case t: Trace[_,_] => toIds(toIds(nm,t.from),t.to)
        case i: FI[_] => i.x match {
          case t: FTreap[_,_] =>  toIds(nm,t)
          case _ => nm
        }
        //case i: TInt =>
        case l: Lazy1[_,_] => toIds(nm,l.a)
        case l: Lazy2[_,_,_] => toIds(toIds(nm,l.a),l.b)
        case l: Lazy3[_,_,_,_] => toIds(toIds(toIds(nm,l.a),l.b),l.c)
        case t: FTreap[_,_] => {
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
    <result>{ss}</result>
  }

  def toSub(t: Any)(implicit m: IDMap): Node = t match {
    case t: FTreap[_,_] => {
      <treap>
        <content value={t.value.toString}/>
        { if (!t.left.isEmpty) {
          <left id={toID(t.left)}/>
      }
        }
        { if (!t.right.isEmpty) {
          <right id={toID(t.right)}/>
      }
        }
      </treap>
    }
    case t: Iterate[_,_] => {
      <iterate>
        <from id={toID(t.from)}/>
        <to id={toID(t.to)}/>
      </iterate>
    }
    case t: Finish[_,_] => {
      <finish>
        <from id={toID(t.from)}/>
        <to id={toID(t.to)}/>
      </finish>
    }
    case t: Reduce[_,_] => {
      <reduce>
        <from id={toID(t.from)}/>
        <to id={toID(t.to)}/>
      </reduce>
    }
    case i: FI[_] => {
      i.x match {
        case t: FTreap[_,_] => toSub(t)
        case _ => <content value={i.x.toString}/>
      }
    }
    case i: TInt => {
        <content value={i.i.toString}/>
    }
    case l: Lazy1[_,_] => {
      <lazy1>
        <function name={l.f.toString}/>
        <arg1 id={toID(l.a)}/>
      </lazy1>
    }
    case l: Lazy2[_,_,_] => {
      <lazy2>
        <function name={l.f.toString}/>
        <arg1 id={toID(l.a)}/>
        <arg2 id={toID(l.b)}/>
      </lazy2>
    }
    case l: Lazy3[_,_,_,_] => {
      <lazy3>
        <function name={l.f.toString}/>
        <arg1 id={toID(l.a)}/>
        <arg2 id={toID(l.b)}/>
        <arg2 id={toID(l.c)}/>
      </lazy3>
    }
    case x => {
        <content value={x.toString}/>
    }
  }
}
