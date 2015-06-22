package spread

import spread.IncrementalArithmetic._

object IncrementalVector{

  import scala.reflect.ClassTag
  import IncrementalMemoization._

  var ic: Long = 0
  var iic: Long = 0
  var iiic: Long = 0

  trait IVector[@specialized(Int,Long,Double) X] {
    {
      ic = ic + 1
    }
    def size: Int
    def level: Int
    def append(o: IVector[X]): IVector[X]
    def split: Array[IVector[X]]
    def slice(start: Int, end: Int): IVector[X]
    def map[@specialized(Int,Long,Double) Y: ClassTag](f: X => Y): IVector[Y]
    def fold[@specialized(Int,Long,Double) Y: ClassTag](s: Y, f: (Y,X) => Y): Y
  }

  final def minWidthOfNonRoot = 16
  final def maxWidth = 64

  val sz = new Array[Int](0)

  def treevector[X](a: Array[IVector[X]]) = {
    val sz = new Array[Int](a.length)
    var ts = 0
    var i = 0
    var s = a.length
    while (i < s) {
      val aa = a(i)
      ts = ts + aa.size
      sz(i) = ts
      i = i + 1
    }
    TreeVector(a,sz)
  }

  val hh = new scala.collection.mutable.HashMap[Int,Int]()
  val hhh = new scala.collection.mutable.HashMap[Any,Int]()

  var hhcount: Int = 0
  var hhhcount: Int = 0

  var monitor = false

  def count(h: Int) = {
    hh.get(h) match {
      case None => {
        hhcount = hhcount + 1
        hh.put(h,1)
      }
      case Some(x) => {
        hhcount = hhcount + 1
        hh.put(h,x+1)
      }
    }
  }

  def count2(h: Any) = {
    hhh.get(h) match {
      case None => {
        if (monitor) { println("new: " + h) }
        hhhcount = hhhcount + 1
        hhh.put(h,1)
      }
      case Some(x) => {
        hhhcount = hhhcount + 1
        hhh.put(h,x+1)
      }
    }
  }


  def treevector[X](a: Array[IVector[X]], s: Array[Int]) = TreeVector(a,s)

  /* nicked from Avail's TreeTuple: by Mark van Gulik */
  case class TreeVector[X](childs: Array[IVector[X]], sizes: Array[Int]) extends IVector[X] {
    { iic = iic + 1 }
    val size = sizes(sizes.length-1)

    val level = childs(0).level + 1
    def childAt(i: Int): IVector[X] = childs(i-1)
    def append(o: IVector[X]): IVector[X] = appendAtLeastOneTree(this,o)
    def slice(start: Int, end: Int) = sliceTree(this,start,end)
    def split = childs
    def map[@specialized(Int,Long,Double) Y: ClassTag](f: X => Y): IVector[Y] = {
      val nr = new Array[IVector[Y]](childs.length)
      var i = 0
      val s = childs.length
      val ch = childs
      while (i < s) {
        nr(i) = ch(i).map(f)
        i = i + 1
      }
      treevector(nr,sizes)
    }
    def fold[@specialized(Int,Long,Double) Y: ClassTag](sf: Y, f: (Y,X) => Y) = {
      var i = 0
      val s = childs.length
      val ch = childs
      var fold = sf
      while (i < s) {
        fold = ch(i).fold(fold,f)
        i = i + 1
      }
      fold
    }

    override def toString = {
      var r = "<"
      for (i <- childs) {
        r = r + i.toString + " "
      }
      r + ">"
    }
  }

  def amap[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) Y](x: Array[X], y: Array[Y], f: X => Y): Array[Y] = {
    val s = x.length
    var i = 0

    while (i < s) {
      y(i) = f(x(i))
      i = i + 1
    }
    y
  }

  def afold[@specialized(Int,Long,Double) X,@specialized(Int,Long,Double) Y](x: Array[X], sf: Y, f: (Y,X) => Y): Y = {
    val s = x.length
    var i = 0
    var fold = sf
    while (i < s) {
      fold = f(fold,x(i))
      i = i + 1
    }
    fold
  }

  def childSubscriptForIndex[X](v1: TreeVector[X], index: Int): Int = {
    val sizes = v1.sizes
    var i = 0

    if (index <= v1.size && index >= 1) {
      while (v1.sizes(i) < index) i = i + 1
      i + 1
    }
    else sys.error("index out of bounds")
  }

  def appendSameLevel[X](v1: TreeVector[X], v2: TreeVector[X]) = {
    val level = v1.level

    assert (level == v2.level)

    val count1 = v1.childs.length
    val count2 = v2.childs.length
    val tcount = count1 + count2

    if (count1 >= minWidthOfNonRoot && count2 >= minWidthOfNonRoot) createPair(v1,v2)
    else if (tcount <= maxWidth) {
      val nc: Array[IVector[X]] = new Array(tcount)
      v1.childs.copyToArray(nc,0)
      v2.childs.copyToArray(nc,count1)
      treevector(nc)
    }
    else {
      val r = v1.childs ++ v2.childs
      val (left,right) = r.splitAt((tcount + 1) >> 1)
      createPair(treevector(left),treevector(right))
    }
  }

  def appendAtLeastOneTree[@specialized(Int,Long,Double) X](vv1: IVector[X], vv2: IVector[X]): TreeVector[X] = {

    val size1 = vv1.size
    val size2 = vv2.size
    val level1 = vv1.level
    val level2 = vv2.level

    if (level1 == level2) appendSameLevel(vv1.asInstanceOf[TreeVector[X]], vv2.asInstanceOf[TreeVector[X]])
    else if (level1 > level2) {
      val v1 = vv1.asInstanceOf[TreeVector[X]]
      val childCount1 = v1.childs.length
      val oldLast = v1.childAt(childCount1)
      val newLast = oldLast.append(vv2)

      if (newLast.level == level1) {
        val nc: Array[IVector[X]] = new Array(childCount1-1)
        v1.childs.copyToArray(nc,0)
        appendSameLevel(treevector(nc),newLast.asInstanceOf[TreeVector[X]])
      }
      else {
        val nc = v1.childs.clone()
        nc(childCount1-1) = newLast
        treevector(nc)
      }
    }
    else {
      val v2 = vv2.asInstanceOf[TreeVector[X]]
      val childCount2 = v2.childs.length
      val oldFirst = v2.childAt(1)
      val newFirst = vv1.append(oldFirst)

      if (newFirst.level == level2) {
        appendSameLevel(newFirst.asInstanceOf[TreeVector[X]],treevector(v2.childs.tail))
      }
      else {
        val nc = v2.childs.clone()
        nc(0) = newFirst
        treevector(nc)
      }
    }
  }

  def offsetForChildSubscript[X](v1: TreeVector[X], index: Int): Int = {
    if (index == 1) 0
    else v1.sizes(index-2)
  }

  def sliceTree[X](v1: TreeVector[X], start: Int, end: Int): IVector[X] = {
    if (start == 1 && end == v1.size) v1
    else {
      val lowChildIndex = childSubscriptForIndex(v1, start)
      val highChildIndex = childSubscriptForIndex(v1, end)

      if (lowChildIndex == highChildIndex)
      {
        val offset = offsetForChildSubscript(v1,lowChildIndex)
        v1.childAt(lowChildIndex).slice(start - offset, end - offset)
      }
      else {
        val leftOffset = offsetForChildSubscript(v1,lowChildIndex)
        val rightOffset = offsetForChildSubscript(v1,highChildIndex)

        val leftPart = v1.childAt(lowChildIndex).slice(start - leftOffset, offsetForChildSubscript(v1,lowChildIndex+1) - leftOffset)
        val rightPart = v1.childAt(highChildIndex).slice(1,end - rightOffset)

        var accumulator = leftPart
        var childIndex = lowChildIndex + 1

        if (lowChildIndex + 5 < highChildIndex)
        {
          val nc: Array[IVector[X]] = new Array(highChildIndex-lowChildIndex-1)
          var dst = 0
          while (childIndex < highChildIndex) {
            nc(dst) = v1.childAt(childIndex)
            dst = dst + 1
            childIndex = childIndex + 1
          }
          accumulator = accumulator.append(treevector(nc))
        }
        else
        {
          while (childIndex < highChildIndex) {
            accumulator = accumulator.append(v1.childAt(childIndex))
            childIndex = childIndex + 1
          }
        }
        accumulator.append(rightPart)
      }
    }
  }
  var pp: Long = 0

  def createPair[X](v1: TreeVector[X], v2: TreeVector[X]) = {
    { pp = pp + 1 }

    val c: Array[IVector[X]] = Array(v1,v2)
    treevector(c)
  }

  case class ArrayVector[@specialized(Int,Long,Double) X: ClassTag](a: Array[X]) extends IVector[X] {
    lazy val hCode : Int = {
      var i = 0
      val s = a.size
      var h = 0
      while (i < s) {
        h = Hashing.jh(h) ^ a(i).hashCode
        i = i + 1
      }
      h
    }

    override def hashCode : Int = hCode
    def size: Int = a.length
    def first = a(0)
    def last = a(size.toInt - 1)
    def level: Int = 0
    def split = Array()
    def append(o: IVector[X]): IVector[X] = {
      val ts = size + o.size
      if (ts <= 64) {
        o match {
          case ArrayVector(x) => ArrayVector(a ++ x)
          case LeafVector(x) => ArrayVector(a :+ x)
          case _ => sys.error("not yet")
        }
      }
      else {
        if (this.level == 0 && o.level == 0) {
          val t: Array[IVector[X]] = Array(this,o)
          treevector(t)
        }
        else appendAtLeastOneTree(this,o)
      }
    }

    def slice(start: Int, end: Int): IVector[X] = {
      val s = start max 1
      val e = end min size

      ArrayVector(a.slice(s-1,e))
    }


    def map[@specialized(Int,Long,Double) Y: ClassTag](f: X => Y): IVector[Y] = {
      ArrayVector(amap(a,new Array[Y](size),f))
    }
    def fold[@specialized(Int,Long,Double) Y: ClassTag](s: Y, f: (Y,X) => Y) = {
      afold(a,s,f)
    }

    override def toString = {
      var r = "<"
      for (i <- a) {
        r = r + i.toString + " "
      }
      r + ">"
    }
  }

  case class LeafVector[@specialized(Int,Long,Double) X: ClassTag](x: X) extends IVector[X] {
    def size = 1
    def level = 0

    def append(o: IVector[X]): IVector[X] = {
      ArrayVector(Array[X](x)).append(o)
    }

    def split = Array()
    def slice(start: Int, end: Int): IVector[X] = {
      val s = start max 1
      val e = end min 1

      if (s == e && s == 1) this
      else sys.error("no slice: empty vector?")
    }

    def map[@specialized(Int,Long,Double) Y: ClassTag](f: X => Y): IVector[Y] = LeafVector(f(x))
    def fold[@specialized(Int,Long,Double) Y: ClassTag](s: Y, f: (Y,X) => Y) = f(s,x)

    override def toString = x.toString
  }

  object Add2 extends (Double => Double) {
    final def apply(d: Double) : Double = {
      d + 1.0
    }
  }

  object Add3 extends ((Double,Double) => Double) {
    final def apply(a1: Double, a2: Double) = a1 + a2
  }

  object Add4 extends ((Int,Int) => Int) {
    final def apply(a1: Int, a2: Int) = {
      iiic = iiic + 1

      a1 + a2
    }
  }

  object Concatenate extends ((IVector[Double],Double) => IVector[Double]) {
    final def apply(a1: IVector[Double], a2: Double): IVector[Double] = {
      a1.append(LeafVector(a2))
    }
  }

  object Mul3 extends (Int => Int) {
    final def apply(d: Int) : Int = {
      d * 3
    }
  }

  object fib extends FA1[Int,Int] {
    def apply(i: F0I) = {
      if (~i < 2) i
      else %(fib,i - 1) + %(fib,i - 2)
    }
    override def toString = "fib"
  }

  object sum extends FA1[IVector[Int],Int] {
    def apply(v: F0[IVector[Int]]): I = {
        val vv = ~v
        if (vv.level == 0) vv.fold(0,Add4)
        else {
          val spl: Array[IVector[Int]] = vv.split
          var i = 1
          var s = spl.size
          var r = %(sum,ei(spl(0)))

          while (i < s) {
            r = r + %(sum,ei(spl(i)))
            i = i + 1
          }

          r
        }
    }
  }

  final def main(args: Array[String]): Unit = {
    import Compress._

    //val a = "XXXXXXXXAXXXXXXXXAXXXXXXXXAXXXXXXXXAXXXXXXXXAXXXXXXXXAXXXXXXXXAXXXXXXXXAXXXXXXXXAXXXXXXXXAXXXXXXXXAXXXXXXXXAXXXXXXXXA"

    val a = "Wat een fantastisch evenement! Ronduit ongelooflijk om het te hebben mogen meemaken! Maandagmorgen van carnaval heet hier Jouvert (naar het Franse Jour Ouvert, voor de opening van het festijn), waarbij er om 4 uur des ochtends verzameld wordt in de straten van de steden (wij waren in het nabije Roxborough) met trucks vol schaarsgeklede vrouwen en mannen en rum. Er wordt gewined (vrouwen komen met hun kont tegen je voorkant schuren of andersom -de foto’s op Facebook komen eraan) in de duistere morgen dat het een lieve lust is. Wanneer de dames erg vlezig zijn en stevig tekeer gaan is het soms vrij pijnlijk voor jouw ‘bumper‘, heb ik gemerkt. Wanneer de zon dan opkomt is er de eerste climax, met nog meer muziek en drank. Aangezien wij die dag gingen duiken is er van rum of andere alcohol erg weinig sprake geweest. Na zonsopkomst zijn wij even gaan afkoelen in de zee, met een ‘wining-in-zwembroek-en-bikini’-sessie. Grappig, zeker als de golven ook nog eens meehelpen met het mensen tegen elkaar aanduwen. Toen we onze kleren vervolgens weer aanhadden trokken we weer door de straten van Roxborough voor het tweede hoogtepunt van de morgen. Modder! Naar aloude creoolse traditie/Afrikaans geloof dienen de geesten van de vierders gezuiverd te worden door het rondgooien van de betere ‘slurries’. Mooi hoor, iedereen smerig. Ik kan je garanderen dat het op basale beats en met oerkreten doorspekte socanummers, uitvoeren van paringsdansen met modderige vrouwen toch wel een fenomenale belevenis genoemd mag worden. Helaas heb ik er geen foto’s van, het fototoestel mocht niet smerig worden. Die dag is verder gespendeerd aan het duiken, de laatste hand leggen aan de kostuums voor de dag erna en het sparen van de energie. Dinsdag immers, stond gepland als ‘non-diving-day’. We zouden carnaval vieren met onze ‘mas band’, te vergelijken met een carnavalsvereniging, wat we in feite waren. Samen met de Speyside Rangers (‘ecologisten’ met als beroep visser die ijveren voor bescherming van de wateren in en om Speyside voor het vrijwaren van ons duikplezier en hun vangst) was ons thema ‘Ridge to Reef’ ofte ‘Regenwoud tot Koraalrif‘. Algemene boodschap: het ecosysteem ‘Tobago’ is één geheel; bescherm het voor de toekomst van je kinderen en inkomsten uit toerisme! Bijna de helft van ons allen was verkleed als ‘marine creature’ (dwz vis, papegaaivis, rog, …) en zowat de andere helft was oerwoudbewoner (aap, kolibri, papegaai, vlinder,…). Ook waren er enkele mensen verkleed als ’Ranger’ die dan het ecosysteem zouden beschermen van de kwade invloeden. Ik tenslotte, was verkleed als Kwade Invloed, genaamd ’Evil Spearfisher’. Ik was verkleed als een duivel, met een speer en hoorns en ik had me voor de gelegenheid ingesmeerd met houtskool. Ook had ik me kaal laten scheren, op een sik en mohawk (beiden blond!) na. De foto’s zijn behoorlijk ’evil’, ik moest weinig moeite doen om eruit te zien als de slechterik. Vroeg in de ochtend verzamelde onze mas band voor vertrek bij ons huis. Er moesten nog enkele kostuums gemaakt worden (in Tobago zijn mensen extreem ongeorganiseerd. Ronduit vervelend, zelfs voor iemand die in Europa niet als een planwonder beschouwd wordt…) voor we konden vertrekken, waardoor iedereen enkele uren rond ons huis aan het ‘limen’ was in plaats van carnaval vieren. Na aankomst in Scarborough (hoofdstad Tobago), was het tijd voor een goede lunch, gemaakt met lokaal voedsel (bestaande uit vlees- of visgerecht, vol stukken vet en een erg smakelijke saus; en zgn. ‘ground provisions’ ofte een mengsel van aardappelen, gekookte groene bananen -werken goed! als afrodisiacum-, zoete aardappelen, ….). Daarna was het tijd voor de ‘parade’. Wij achter onze truck, in vol ornaat en in de juiste muziek (playlistje van een uurtje; ken alle muziek intussen van buiten; prachtig souvenir; mensen die het afstudeerhok met mij gaan delen zullen het snel leren kennen) en zonder alcohol in het bloed; hebben een feestje gebouwd om U tegen te zeggen. Drie keer langs het parcours stond er een podium opgesteld voor de ‘jury’ en voor elk van deze podia deden we een soort van ‘voorstelling’ van wie we waren en wat we te vertellen hadden. Achteraf bleek dat we 4de zijn ge-eindigd. Een mooi resultaat voor de eerste keer dat deze mas-band haar ding deed. Na de stoet zelf, was het tijd voor het diner; en aangezien het 5.30pm geweest was, mochten we aan de alcohol. Clandestien was er al een en ander genuttigd, maar nu mochten er ook openbaar, volgens CoralCay-regels, 4 pilseners genuttigd worden. Immers, de dag erna was een duikdag en het is gevaarlijk om te duiken met teveel alcohol of afgebroken alcohol in je bloed. Uitdroging is de belangrijkste oorzaak van duikongevallen met blijvend letsel. Na het diner zagen we de andere mas-bands langskomen. Er zijn grofweg twee soorten. Je band is ofwel een ‘dance-band‘, zoals ons; of je bent een ‘steelband’. in het laatste geval heb je een gigantische truck vol met ‘steelpans’ ofte goed afgestemde olievaten, geschikt voor het produceren van calypso-muziek. Een LANGgekoesterde droom ging in vervulling. Corneel Jr. ging calypso-drummen op een steelpan. Ongelooflijk cool welke geluiden, ritmes en vibes je uit zo’n apparaat gesleept krijgt. Heerlijck ende puur genieten. De man met wie ik aan de praat raakte zei dat, op basis van mijn spel, ik duidelijk thuishoorde in de ritmesectie en dat ik een goede jazzdrummer was. Hij bood me zowaar een plek aan in zijn band. Ik moest hem helaas vertellen dat ik Europeaan was, met grote interesse maar geen tijd/mogelijkheid om zijn band te vervoegen. Vereerd verliet ik de truck…. Hierna trokken we verder de stad in, waar er blijkbaar terug modder was bovengehaald om het einde van carnaval te vieren. We dronken fris en helder Heineken bier (dat zowaar meer smaak heeft dan de lokale Stag- en Carib-brouwsels) en wat betere rumsoorten. Veel weet ik er niet meer van maar blijkbaar hebben anderen foto’s genomen. Men friste enkele dagen later mijn geheugen op en naar wat ik toen hoorde, moet het hét (of een van de) meest schandalige aangeschoten moment(en) zijn dat/die ik ooit heb meegemaakt. Gelukkig waren de duiken de dag erna zeer ondiep en makkelijk. Denk niet dat ik iets anders/meer had aangekund…. De waarde van onze activiteiten voor de locals. Wat hebben de locals aan onze aanwezigheid? Wij, gebruinde maar nog steeds blanken, worden hier niet helemaal gezien als toeristen, maar zeker ook niet als locals. Ergo: we zijn een leuke bron van inkomsten (er is een juwelenverkoop gehouden hier, voor de internetverbinding worden we afgezet, wil je limen met locals dan vragen ze je om hun rum te betalen,…) maar toch zijn locals niet echt opdringerig naar ons geld. Naar onze vrouwen daarentegen…. Er is/was een vaste groep van zo’n vijftal mannen van de Speyside Rangers (zie hoger) die langskwamen om met onze vrijgezelle dames te feesten/ze uit te vragen voor zaterdagavond/zondag en ze stukjes Tobago te laten zien die de rest van de aanwezigen hier op de site niet te zien krijgt, of toch niet zo goedkoop. De ‘indecent proposals’ (ooit gehoord van de uitnodiging ‘shall we do 20 toes and two noses’?) vliegen je om de oren; cru gesteld: ‘die negers denken maar aan één ding’. Naast bedpartners en geld voorzien we de locals gelukkig ook van data over de koraalriffen. Deze data gaan naar de Tobago House of Assembly (THA), vrij goed vergelijkbaar met de Vlaamse regering, want de regering van deelstaat Tobago en bevoegd voor land- en persoongebonden zaken zoals milieu en onderwijs. Met name deze twee gebieden zijn zaken waarbij CoralCay samenwerkt met de THA. Over de rifgegevens krijgen we geen feedback, voor zover wij weten verdwijnen onze data in een of ander archief. Er gebeurt immers erg weinig door de overheid op het vlak van koraalrifbescherming. Het enige is het ‘Marine Park’ rond Buccoo Reef, in het westen van het eiland, maar dat Marine Park bestaat vooral op papier. In dat natuurgebied wordt naar hartenlust gevaren, gevist, ge-jetskiet, gesurft enzovoort enzovoort. Er is geen enkele consequentie aan het overtreden van de regels van dat natuurgebied. Nochtans zijn de koraalriffen, samen met de bossen en de resorts, de belangrijkste bron van inkomsten voor het eiland. Begrijpe wie kan… Wat we wel weten: de locale Charlotteville-overheid gaat gebruik maken van onze data. Binnen de gemeenteraad wordt er gediscussieerd over de wenselijkheid van het verlengen van de pier (nu: 20 meter) naar een lengte van een kleine honderd meter, die dan tot diepwater zou lopen zodat cruiseschepen kunnen aanleggen. Deze zouden extra inkomsten voor het dorp betekenen, met aan de andere kant een grote druk op de riffen, met gevolgen voor de inkomsten uit visvangst en duikindustrie…. Hier kunnen de gegevens van CoralCay helpen in de beslissing….. Aangezien de jeugd de toekomst is, heeft CoralCay het beleid om de locale kinderen wat bij te brengen over milieu, natuurbescherming en zorgvuldig omgaan met grondstoffen en materialen. Om dat te doen wordt er toneel gespeeld, spellen gespeeld en geknutseld met de klein mannen. Momenteel zijn we ons aan het voorbereiden op het stuk ‘Danny’s dilemma’, waarbij een vis samen met de kinderen het koraal gaat redden door het rif niet meer te vervuilen. Leuk om een beetje met die kinderen te spelen. En tenminste iets waarbij de toegevoegde waarde van ons werk voor de locale bevolking onmiddellijk duidelijk is! En verder… Vind ik het duiken nog steeds (na meer dan 3 maanden) supertof. Ongeacht of je iets fascinerend ziet of niet, het gevoel onder water te leven en daar iets te doen is fantastisch. Na alle gesprekken die ik tot dusver met duikers gehad heb, blijkt dat er veel meer mogelijkheden voor een duikprofessional zijn dan alleen het lesgeven aan toeristen in een tropisch resort. Iets wat me overigens niet zozeer aanspreekt; na de controleduiken met gebrevetteerde duikers die ik tot dusver deed, merk ik dat het me enerveert dat gekwalificeerde duikers achteloos met bepaalde regels of vaardigheden omgaan, omdat ze ze niet kunnen of niet weten wat de achtergrond erachter is. Toeristen. Aan de andere kant is duikles geven echt wel tof aan beginnelingen, ik hou ervan om mensen iets uit te leggen, ze fouten te laten maken en vervolgens met een welgemikte en -meende glimlach uit te leggen wat er beter kan. Mensen zo goed mogelijk maken door ze zaken zo goed mogelijk uit te leggen. Fijn. Edoch, ik wil me eerst zwaar verder bekwamen en een zo goed mogelijke duiker (far beyond Divemaster) worden voor ik iets anders ga doen in deze sport. Wellicht zijn er mogelijkheden als duikende ingenieur (commerciële duikbedrijven, het leger, …), die overigens ook beter betalen. Muggensteek op woensdag 16 maart Vorige woensdag, 20 maart, ben ik gestoken door een mug. Op zich absoluut geen noemenswaardig feit (gebeurt tientallen keren per dag), ware het niet dat deze mug bezoekers in zich droeg en deze ongenode gasten in mijn enkel heeft binnengelaten. Reeds de vierde muggensteek die ontstoken raakt…! Erg vervelend, vooral aangezien het op een plaats is die, onderwater, erg veel schuren tegen neopreen te verwerken krijgt. Je huid is al weker onderwater en de wrijving erbij maakt een wonde erg snel gezwollen. Op het moment van schrijven heb ik een hele week met mijn vingers gedraaid want ik mocht niet duiken. De wonde moest helen. Na meer dan een week antibiotica is de ontsteking verleden tijd en ook alle pus in de wonde is er intussen uit. Wat overblijft is een stratovulkaan-vormige wonde die nu -hoe sneller hoe beter- moet helen. Pas als er terug huid is gegroeid mag ik weer het water in. Om met kabouter Wesley te spreken: ‘IK HAAT MUGGEN!’ Kwallen op vrijdag 18 maart Woensdag tot en met vrijdag hebben we weer in Speyside gedoken voor surveys. Typisch voor midden maart is het eerste plankton dat zich verzamelt in verschillende waterlagen. Zeer diverse wezens voeden zich met plankton waaronder grote manta rays (!!!) maar -helaas- ook kwallen. We zagen de kwalconcentratie dan ook stelselmatig toenemen vanaf woensdag. Van een enkel kwalletje op woensdag, over een waterlaag (5-7 meter diepte; ERG vervelend bij de safety stop van 3 minuten op 5 meter) met enkele kwallen op donderdag, tot een concentratie van zo’n 20 kwallen per kuub (OVERAL!) op vrijdag. Onvermijdelijk dat deze ellendige wezens je prikken. En prikken doen ze, verdomde bastaarden. Anticiperende op de kwallen had ik op vrijdag mijn ‘long’ wetsuit meegebracht. Dat neopreen apparaat heeft lange mouwen voor armen en benen en een kap voor je hoofd. Aangezien ik twee dagen ervoor volledig kaalhoofdig geworden was, was enige bescherming tegen de kwallen geboden. Het enige stuk van mijn lichaam waar de duivelse ongewervelden aankonden was mijn gezicht, minus gebied rond ogen en neus en de mond zelf. Mijn lippen kon ik helaas niet beschermen; ik heb mijn regulator 2 of 3 keer uitgespuwd in een paniekreflex omdat de tentakels mijn lippen raakten. Zeer intense pijn, kan ik je garanderen. In deze survey had ik de ‘physical’ rol, wat in principe de leidersrol is. Bepalen wanneer en waar de lijn uitgelegd en ingerold wordt en opvolgen of je groep van vieren het goed doet. Gelukkig had ik deze rol, zodat ik afgeleid was van de kwallen tijdens de survey. De hele tijd achter letterlijk en figuurlijk extreem geïrriteerde duikers aanzwemmen, ze geruststellen en vooruitsturen; deed je de intense pijn in je gezicht van je kwallen vergeten. Intense momenten voor ons allemaal. De luchtconsumptie van iedereen lag op zo’n 150 bar, voor 37 minuten op een gemiddelde diepte van 11 meter. Dat is extreem hoog. Ik hou ervan om in crisismomenten, wanneer iedereen zwaar geëmotioneerd is, zaken te doen vooruitrollen. Na het boven komen van de duik gebruikten we meer dan een liter azijn om de pijn te verzachten. Weer een van die duikjes die je een crisismoment opleveren, maar je wel een betere duiker en duikleider maken! We namen ons voor niet meer te duiken die dag. We gingen toch, maar alleen na de garantie dat er elders, waar geen plankton was ook geen kwallen waren. Dat bleek rechtstreeks voor de voordeur van het huis van Ian Fleming te zijn. Er is daar een onderwatervlakte op 5 meter, zeer schaarsbegroeid met koraal, maar die geeft het water wel een PRACHTIGE kleur. Wanneer je op die plek bent kan je je zo voorstellen hoe iemand inspiratie krijgt voor het schrijven van een verhaal over een geheim agent, knappe vrouwen, prachtige oorden en technische snufjes. Mooi hoor. Surfen op zondag 20 maart Na drie dagen aan de pijnstiller en antibiotica, was mijn wonde op zijn dieptepunt. Een zwarte vlek, mijn hele linkerenkel en -voet rood van de ontsteking en een zwelling van enkele centimeters op mijn enkel. Geen probleem voor de autorit naar de andere kant van het eiland. De taxichauffeur was megacool; een rasta die vanalles zat te roken en drinken onderweg naar ons, dat verbloemde met wierrook net voor aankomst bij ons, maar zijn spraak en gedrag spraken boekdelen. Hij draaide de hele tijd Bob Marley en gelijkaardigen en maakte fijne opmerkingen over ’vibes in de nature’, ’de spirits of de cow’ en hij sprak een sporadische hond aan met ’man’ om hem uit de weg te krijgen. We namen de ‘northern road‘, wat een weg is al enkele jaren niet meer onderhouden was. Na de laatste orkaan, in november vorig jaar, zijn er wat modderstromen geweest. Dat maakt dat de weg hier en daar niet meer verhard is en je dus wat door de modder moet ploeteren. Een en ander maakt de weg vrij onpopulair; maar dat zorgt ervoor dat je zonder tegenliggers kan genieten van de leuke uitzichten over zee, bossen en rotsen. Eenmaal in Mt. Irvine aangekomen, was het tijd voor een goeie roti om de maag te vullen en om zonder pijn te kunnen surfen zat ik maar aan de codeïne. Surfers blijken erg pragmatische mensen te zijn. De ducttape werd bovengehaald om mijn voet helemaal in te tapen en waterdicht te maken. Na wat yoga-achtige relaxatie- en flexoefeningen kregen we onderricht over de te nemen stappen om fatsoenlijk op een plank recht te staan. Na een babbel met 2 65-jarige Amerikanen met lange grijze haren en baarden (super cool zicht!), die 18 en surfer waren in Californië in de 60’s, was het tijd voor een poging om de golven te berijden. Fijn. Na wat halfslachtige pogingen lukte het me om rechtop te staan en te blijven staan op een golf. Wist niet dat die dingen zo snel konden gaan. Superleuk om met je plankje overgeleverd te zijn aan het watergeweld en door slim te laveren je koers te bepalen. Wel licht gevaarlijk omdat er koraal groeit op de rotsen waar we surften (met name ‘elkhorn’ is lastig; dat groeit als een hoorn van een rendier; bovenop een rots). De kunst is om ‘in’ de golf te vallen, eerder dan ‘uit’ de golf. Goed ook weer voor de conditie. Het kostte me flink wat yoga om de dag erna mijn spieren weer in balans te brengen en soepel te krijgen. De barbecue ‘on the beach’ die van tevoren aangekondigd was ging helaas niet door; de surfinstructor (weer een van die locals die uit zijn op blank neuckvleesch) had te hard gefeest met de dames van ons die op hun driedaagse vakantie waren, waardoor hij, met zijn kater, niet ook nog een beetje vuur en vlees kon organiseren. Jammer want de zonsondergang en de sfeer op het strand onder de wuivende (palm- en amandel-)bomen was prachtig. Veel wietrook ook en leuke reggaebeats, opnieuw. Bij terugkomst in de basis in Charlotteville waren we allemaal doodop en tevreden met wat was geweest, maar ook gefrustreerd met het feit dat we weer in het CoralCay-huis waren en moesten blijven. Klagen over CoralCay-regels. Een fantastische tijd heb ik hier en een van de dingen die je leert is dat perfect genot niet bestaat. Er zijn tegenvallers, met name op/over de leefwijze hier ‘on site’. De regels hier zijn dat je in het huis of de onmiddellijke omgeving ervan moet blijven, tenzij als je gaat duiken of onder toezicht van een ’staffmember’ bent. Die laatste mogelijkheid is er vooral voor een winkel-uitstapje als je je bijzonder proteïne-arme dieet wil aanvullen met wat bevroren kippenvlees of een ei. Heb de laatste drie-en-een-halve maand alles bij elkaar misschien 5 keer varken en/of rundvlees gegeten. Dit zijn eigenlijk de enige twee dingen waarover ik echt te klagen heb: voeding en de ’cabin-fever’ ofte wat er gebeurt als je de hele tijd met dezelfde (15+) mensen in een zeer beperkte ruimte (zo’n 100m²) leeft. Relativeren en het nemen van je eigen vrijheid is imperatief. Kleine frustraties bouwen onvermijdelijk op en kunnen niet ‘gelucht’ worden. Mensen reageren hun negatieve gevoelens dan maar af op elkaar, wat dan mensen weer pissed maakt op elkaar. Je krijgt al snel een gevangenisgevoel. Wat elke persoon die bij CoralCay verblijft dan ook het meeste mist is ‘vrijheid’ en privacy. Dat laatste kan -tot op zekere hoogte en slechts voor specifieke activiteiten- geregeld worden in het washok van het complex (afgelegen, heeft een matras (!) en een deur die niet op slot gaat maar wel van binnenuit kan gesloten worden). Voor het nemen van zoveel mogelijk vrijheid heb ik mijn vuistregel: elke dag (minstens) 1 CoralCay-regel breken. Om ervoor te zorgen dat iedereen (staff en andere volunteers) me blijven vertrouwen ben ik open over mijn vuistregel en informeer ik altijd een staflid als ik een regel ga breken; meestal heeft die te maken met een beetje rum of ’blijf in de buurt van het huis’. Er is een prachtige plek, genaamd Fort Cambleton, met overzicht op de hele baai, die een dikke 5 min buiten de grenzen ligt, maar ik kom er meermaals per week voor meditatie of yoga. Vandaag ben ik naar Flagstaff Hill geweest, dat is de heuvel 200+ meter boven het dorp, met uitzicht op alle duikstekken die we tot dusver gedaan hebben. De moeite waard, net zoals de klim van meer dan een uur door het oerwoud onderweg er naartoe. Prachtig. Aangezien ik al enige tijd niks meer gepost heb en ik jullie niet wilde bombarderen met ineens veel teveel tekst, wil ik hier even afsluiten. Ik heb nog heel wat te vertellen en ga dat ook doen. Bij dezen heb je een idee van de onderwerpen waarover ik je zeer binnenkort wil gaan vertellen: "

    var result: Vector[Char] = Vector()
    val s = 250
    var i = 0
    var random1 = 0
    var random2 = Hashing.jh(10000)
    var random3 = Hashing.jh(20000)

    while (i < s) {
      val r1: Int = (5 + (random1 % 5)) + 1
      val r2: Int = (5 + (random2 % 5)).toInt + 1

      random1 = Hashing.jh(random2) ^ random1
      random2 = Hashing.jh(random1) ^ random2

      var ii = 0
      var r: Vector[Char] = Vector()
      while (ii <= r1) {
        random3 = Hashing.jh(random3) ^ random1 - random2
        r = r :+ ((13 + (random3 % 13)) + 'a').toChar
        ii = ii + 1
      }
      random3 = Hashing.jh(random3) ^ random1 + random2

      ii = 0
      var rr: Vector[Char] = Vector()
      while (ii <= r2) {
        rr = rr ++ r
        r = r
        ii = ii + 1
      }
      result = result ++ rr
      i = i + 1
    }
    //result = a.toVector

    println("result: " + result.size)
    /*val aa = part(part(a.toArray))
    println("hh: " + hh.size)
    val bb = part(part(b.toArray))
    println("hh: " + hh.size)
    val cc = part(part(c.toArray))
    println("hh: " + hh.size)

    println("aa: " + aa.toList)
    println("bb: " + bb.toList)                    e
    println("cc: " + cc.toList)

    val k = List(1,2)
    val k2 = 3 +: k

    println(k2.tail) */

    val aa = part(part(part(result.toArray)))
    println("size: " + hhh.size)

    var oldsize = hhh.size

    var ii = 0
    var ss = result.size

    /*while (ii < ss) {
      val result2 = result.updated(ii,"%")
      ii = ii + 1
      val bb = (part(result2.toArray))
      if ((ii % 1) == 0) { println("ii: " + ii) ; println("dsize: " + (hhh.size-oldsize)) ; oldsize=hhh.size}
    } */

    val result2 = result.updated(5000,"%")
    monitor = true
    val bb = part(part(part(result2.toArray)))
    println("size: " + hhh.size)

  }

  def freq(h: scala.collection.mutable.HashMap[Any,Int]) = {
    val freq = new scala.collection.mutable.HashMap[Any,Int]()

    for (kv <- h) {
      val (_,v) = kv
      freq.get(v) match {
        case None => freq.put(v,1)
        case Some(x) => freq.put(v,x+1)
      }
    }
    freq
  }

  abstract class Foreach[@specialized(Int,Long,Double) X, @specialized(Int,Long,Double) Y] extends (Array[X] => Y) {
    def apply(a: Array[X]): Y
  }

  /*case class FMap[X,Y: ClassTag](f: X => Y) extends Foreach[X,Array[Y]] {
    def apply(a: Array[X]): Array[Y] = {

    }
  } */

  object mp extends (Unit => Int) {
    def apply(a: Unit) = a.hashCode
  }

  // Canonical B-Tree
  trait CBTree

  val multiplier = 1664525

  val wsize = 31

  val mm = {
    var i = 0
    var m = multiplier
    while (i < wsize) {
      m = m * multiplier
      i = i + 1
    }
    m
  }

  def part[X: ClassTag](a: Array[X]): Array[Chunk[_]] = part2(a,0,0,0,wsize)

  /*def concat[X: ClassTag](a1: Array[Chunk[X]], a2: Array[Chunk[X]]): Array[Chunk[X]] = {
    val s = a1.size

    val e0 = a1(s-3)
    val e1 = a1(s-2)
    val e2 = a1(s-1)
    val e3 = a2(0)
    val e4 = a2(1)

    val na = e0.a ++ e1.a ++ e2.a ++ e3.a ++ e4.a

    part2(na,e0.size+e1.size,e0.size+e1.size+e2.size,e2.rhash,wsize)
  } */

  // append one chunk at the tail
  /*def append[X: ClassTag](a: Array[Chunk[X]], e: Array[X]): Array[Chunk[X]] = {
    if (a.size == 1) {
      part(a(0).a ++ e)
    }
    else {
      val s = a.size
      val e0 = a(s-3)
      val e1 = a(s-2)
      val e2 = a(s-1)
      val na = e0.a ++ e1.a ++ e2.a ++ e
      println("e0: " + e0)
      println("e1: " + e1)
      println("e2: " + e2)
      println("na: " + na.toList)
      println("e0.size + e1.size: " + (e0.size + e1.size))
      part2(na,e0.size,e0.size + e1.size,e1.rhash,wsize)
    }
  }  */


  case class ArrayHolder[X](x: Array[X])

  def part2[X: ClassTag](a: Array[X], si: Int, ei: Int, sh: Int, ws: Int): Array[Chunk[_]] = {
    import Hashing.jh

    var r: List[Chunk[_]] = List()

    var i = ei
    var s = a.length
    var h: Int = sh

    var w = ws

    var ii = si

    while (i < s) {
      val wi = i-ii

      if (((h >> 9) & 15) == 7) {
            r = r :+ avector(copy(a,ii,wi),h,ws)
            ii = i
      }

      h = (h + jh(a(i))) * multiplier
      if (i >= w) {
        h = h - (jh(a(i - w)) * mm)
      }

      i = i + 1
    }

    val wi = i-ii

    if ((wi) > 0) { r = r :+ avector(copy(a,ii,wi),h,ws) }
    r.toArray
  }

  def copy[X : ClassTag](s: Array[X], i: Int, w: Int): Array[X] = {
    val na = new Array[X](w)
    Array.copy(s,i,na,0,w)
    na
  }

  trait FBVector[@specialized(Int,Long,Double) X] {
    def depth: Int
    def size: Int
  }

  def avector[@specialized(Int,Long,Double) X: ClassTag](a: Array[X], rhash: Int, ws: Int): Chunk[_] = {
    val av = Chunk(a,rhash)
    count2(av)
    av
    /*if (a.size < (ws * 4)) {
      val av = Chunk(a,rhash)
      count2(av)
      av
    }
    else {
      import Compress._
      val rs = compress3(a,16)
      val ks = part(rs)

      if ((ks.size) > ws * 4) sys.error("unlikely")
      else {
        val av = Chunk(ks,rhash)
        count2(av)
        av
      }
    }  */
  }

  case class Chunk[X : ClassTag](a: Seq[X], rhash: Int) extends FBVector[X] {
    def depth = 0
    def size = a.length

    override def hashCode : Int = hCode

    lazy val hCode : Int = {
      var i = 0
      val s = a.size
      var h = Hashing.jh(s)
      while (i < s) {
        h = Hashing.jh(h) ^ a(i).hashCode
        i = i + 1
      }
      h
    }

    override def toString = {
      var r = "<"+size+":"
      for (i <- a) {
        r = r + i.toString
      }
      r + ">"
    }
  }

  case class FBTreeVector[X](a: Array[FBVector[X]]) extends FBVector[X] {
    val depth = a(0).depth + 1
    def size = 0
  }

  def concatSameLevel[X](a1: Array[X], a2: Array[X]): Array[Array[X]] = {
    sys.error("not yet")
  }

  object Compress{

    trait CString[X]

    case class Literal[X](x: X) extends CString[X]{
      override def toString = "[" + x.toString + "]"
    }

    case class Reference[X](i: Int,s: Int) extends CString[X]{
      override def toString = "R(" + i + "," + s + ")"
    }

    def block[X](dist: Int,b: Seq[X]) ={
      if (b.size > (-dist)) Repetition(b.slice(0,-dist),b.size)
      else Block(b)
    }

    case class Repetition[X](b: Seq[X],size: Int) extends CString[X]{
      override def toString ={
        var s = "{" + size + ":"
        var i = 0
        while (i < b.length) {
          s = s + b(i).toString
          i = i + 1
        }
        s + "}"
      }
    }

    case class Block[X](b: Seq[X]) extends CString[X]{
      override def toString ={
        var s = "[" + b.size + ":"
        var i = 0
        while (i < b.length) {
          s = s + b(i).toString
          i = i + 1
        }
        s + "]"
      }
    }

    case class BestMatch(distance: Int,length: Int)

    val minStringLength = 2

    def concat2[X](a: List[CString[X]],i: CString[X]): List[CString[X]] ={
      if (!a.isEmpty) {
        val r: List[CString[X]] = {
          a.head match {
            case Block(aa) => i match {
              case Block(ii) => {
                Block(aa ++ ii) +: a.tail
              }
              case Repetition(seq,size) => {
                if (aa == seq) {
                  Repetition(seq,size + aa.size) +: a.tail
                }
                else i +: a
              }
            }
            case _ => i +: a
          }
        }
        r
      }
      else i +: a
    }

    var k: Long = 0

    def compress3[X](data: Array[X],windowLength: Int): Array[CString[X]] ={
      var pos = 0
      var compressed: List[CString[X]] = List()
      var lastPos = data.length

      while (pos < lastPos) {
        var searchPos = 0 max (pos - windowLength)
        var matchLength = 0

        var bestMatchLength = 0
        var bestDist = 0

        var newCompressed: CString[X] = null

        while (searchPos < pos) {
          k = k + 1

          while (((pos + matchLength) < lastPos) && (data(searchPos + matchLength) == data(pos + matchLength))) {
            matchLength = matchLength + 1
          }
          if (((searchPos + matchLength) > pos) && (matchLength > bestMatchLength)) {
            bestMatchLength = matchLength
            bestDist = pos - searchPos
          }
          searchPos = searchPos + 1
          matchLength = 0
        }

        if (((searchPos + matchLength) > pos) && (matchLength > bestMatchLength)) {
          bestMatchLength = matchLength
          bestDist = pos - searchPos
        }

        if (bestMatchLength > minStringLength) {
          newCompressed = Repetition(data.slice(pos - bestDist,pos),bestMatchLength)
          pos = pos + bestMatchLength
        }
        else {
          newCompressed = Block(Seq(data(pos)))
          pos = pos + 1
        }

        compressed = concat2(compressed,newCompressed)
      }

      data.slice(pos,data.length - 1).map(Literal(_)) ++ compressed

      compressed.reverse.toArray
    }
  }

  def comp[X](a: Array[X], b: Array[X]): Boolean = {
    if (a.length == b.length) {
      var i = 0
      var s = a.length
      var eq = true
      while ((i < s) && (eq)) {
        if (a(i) != b(i)) { eq = false}
        i = i + 1
      }
      eq
    }
    else false
  }
}
