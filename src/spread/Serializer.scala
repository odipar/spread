package spread

object Serializer {
  import IncrementalMemoization._
  import IncrementalArithmetic._
  import scala.xml._

  case class IDMap(m: Map[Any,Long],id: Long) {
    def contains(v: Any) = m.contains(v)
    def put(v: Any) = IDMap(m + (v -> id),id + 1)
  }

  def toIds(v: Any): IDMap = {
    toIds(IDMap(Map(),0),v)
  }

  def toIds(m: IDMap, v: Any): IDMap = {
    if (m.contains(v)) m
    else {
      val nm = m.put(v)
      v match {
        case t: Trace[_,_] => toIds(toIds(nm,t.origin),t.f)
        case i: FI[_] => toIds(nm,i.x)
        case i: TInt => toIds(nm,i.i)
        case l: Lazy1[_,_] => toIds(toIds(nm,l.f),l.a)
        case l: Lazy2[_,_,_] => toIds(toIds(toIds(nm,l.f),l.a),l.b)
        case l: Lazy3[_,_,_,_] => toIds(toIds(toIds(toIds(nm,l.f),l.a),l.b),l.c)
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
          {
            i._1 match {
              case t: Trace[_,_] => {
                <trace>
                    <from id={toID(t.origin)}/>
                    <to id={toID(t.f)}/>
                </trace>
              }
              case i: FI[_] => {
                <content value={i.x.toString}/>
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
        </node>
      }
      ss = ss :+ n
    }
    <result>{ss}</result>
  }

}
