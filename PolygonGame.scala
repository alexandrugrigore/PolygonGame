import scala.math._

case class Point(x: Double, y: Double){
  def +(p: Point) = Point(x + p.x, y + p.y)
  def -(p: Point) = Point(x - p.x, y - p.y)
  def /(d: Double) = Point(x / d, y / d)
  def *(d: Double) = Point(x * d, y * d)
  def length = Math.sqrt(x * x + y * y)
  def distance(p: Point) = Math.sqrt((p.x - x)*(p.x - x) + (p.y - y)*(p.y - y))
  def egal(p: Point) = if (p.x == x && p.y == y) true else false
  override def toString() = "("+x+","+y+")"
}

class RandomPolygon(ctrX:Double, ctrY:Double, aveRadius:Double, 
      var irregularity:Double, var spikeyness:Double, 
      numVerts:Integer) {
  /*Params:
    ctrX, ctrY - coordinat es of the "centre" of the polygon
    aveRadius - in px, the average radius of this polygon, this roughly 
                controls how large the polygon is, really only useful for order of magnitude.
    irregularity - [0,1] indicating how much variance there is in the 
                  angular spacing of vertices. [0,1] will map to [0, 2pi/numberOfVerts]
    spikeyness - [0,1] indicating how much variance there is in 
                  each vertex from the circle of radius aveRadius. [0,1] will map to [0, aveRadius]
    numVerts - self-explanatory

    Returns a list of vertices, in CCW order.*/
  import Page._
  import ScalaJSExample._
  
  val pointList: List[Point] = generatePolygon//(200,200,2)
  
  def generatePolygon(x: Double, y: Double, scale: Int): List[Point] = {
    val points = List(
      new Point(x,y),
      new Point(x +250 / scale, y +250 / scale),
      new Point(x, y -300/scale),
      new Point(x -300 / scale, y +250 / scale), 
      new Point(x,y))
    points
  }
    
  def generatePolygon: List[Point] = {
    def randomDouble(rangeMin: Double, rangeMax: Double): Double = {
      val r = new scala.util.Random
      val randomValue: Double = rangeMin + (rangeMax - rangeMin) * r.nextDouble()
      randomValue
    }
    
    def randomGauss(rangeMin: Double, rangeMax: Double): Double = {
      val r = new scala.util.Random
      val randomValue: Double = rangeMin + (rangeMax - rangeMin) * r.nextGaussian()
      randomValue
    }
    def clip(x: Double, min: Double, max: Double): Double = {
      if( min > max ) x 
        else if( x < min ) min 
          else if( x > max ) max 
            else x
      }   
    irregularity = clip( irregularity, 0,1 ) * 2*Pi / numVerts
    spikeyness = clip( spikeyness, 0,1 ) * aveRadius

    // generate n angle stepsp
    var angleSteps: List[Double] = Nil
    val lower = (2*Pi / numVerts) - irregularity
    val upper = (2*Pi / numVerts) + irregularity
    var sum: Double = 0
    for (i <- 0 until numVerts){
        val tmp: Double = randomDouble(lower, upper)
        angleSteps = angleSteps :+ tmp
        sum = sum + tmp
    }

    // normalize the steps so that point 0 and point n+1 are the same
    val k = sum / (2*Pi)
    for (i <- 0 until numVerts)
        angleSteps.updated(i, angleSteps(i) / k)

    // now generate the points
    var angle = randomDouble(0, 2*Pi)
    val points = (0 until numVerts).map{ i =>
        val r_i = clip( randomGauss(aveRadius, spikeyness), 0, 2*aveRadius )
        val x = ctrX + r_i*cos(angle)
        val y = ctrY + r_i*sin(angle)
        angle = angle + angleSteps(i)
        new Point(x,y)
      }.toList
    points :+ points(0)
  }
  
  def render = {
    renderer.beginPath
    renderer.fillStyle = "yellow"
    renderer.moveTo(pointList(0).x, pointList(0).y)
    renderer.fillRect(pointList(0).x, pointList(0).y, 1, 1)
    
    renderer.strokeStyle = "red"
    renderer.fillStyle = "red"
    for (vecter <- pointList.drop(1)){
      renderer.lineTo(vecter.x, vecter.y)
      renderer.stroke
      renderer.fillRect(vecter.x, vecter.y, 1, 1)
    }
    renderer.closePath
  }
  
}

class Player(var pozition: Point) {
  import Page._
  import ScalaJSExample._
  def get: Point = pozition
  
  private def moveDirection(offset: Point): Point = {
    pozition = pozition + offset
    pozition
  }
  
  def render = {
    renderer.fillStyle = "white"
    renderer.fillRect(pozition.x - 5, pozition.y - 5, 10, 10)
  }
  
  def renderPolygon = {
      polygon.render
      
      val dr = new Dreapta(polygon)
      renderer.fillStyle = "red"
        
      val ecPlayer = dr.ecPlayerPoints(player.get)
      val ecPoints = dr.ecTwoPoints
      val segmentsTwo = dr.segmentsTwoPoints
      
      val pointsOnPoly = dr.intersectingPoints(ecPlayer, ecPoints)
      .flatMap {point =>
        segmentsTwo.map {segment=>
          if (dr.isBetween(segment._1, segment._2, point)) point
          else new Point(0,0)
        }
      }.filterNot{point =>
        (point.x == 0 && point.y == 0)
      }
      
      val segmentsPP = dr.segmentsPlayerPoints(pointsOnPoly, player.get)
      
      val segmentsTuple: List[((Point, Point),List[Point])] = segmentsPP
      .map {segment =>
        val listaTemp: List[Point] = pointsOnPoly.filter{point => 
          dr.isBetween(segment._1, segment._2, point)
        }.toSet.toList
        val lista: List[Point] = listaTemp.sortBy{
          _.distance(segment._1)
        }
        ((segment._1, segment._2), lista)
      }
      
      val goodSegments: List[(Point, Point)] = segmentsTuple
      .filterNot{tuple =>
        tuple._2.size > 1
      }
      .map{segment =>
        (segment._1)
      }
      
      val badSegments: List[(Point, Point)] = segmentsTuple
      .filter{tuple =>
        tuple._2.size > 1
      }
      .map{segment =>
        (segment._1)
      }
      
      val goodFromBadSeg: List[(Point, Point)] = segmentsTuple
      .filter{tuple =>
        tuple._2.size > 1
      }
      .map{segment =>
        val newList = segment._2.dropWhile{point =>
          polygon.pointList.contains(point)
        }
        (segment._1, newList)
      }
      .filterNot{tuple =>
        tuple._2.size > 1
      }
      .map{segment =>
        (segment._1)
      }
      
      val finalSegments = goodSegments ++ goodFromBadSeg
      val polyPoints: List[Point] = finalSegments
      .map{segments =>
        segments._2
      }
      
      
      val pIndex2:List[(Point,Double,Int)] = polyPoints.flatMap{point=>
        segmentsTwo.zipWithIndex.find{segment =>
          dr.isBetween(segment._1._1, segment._1._2, point)
        }.map{x=>
          (point,x._2)
        }
      }.map{tuple =>
        val angle = dr.anglePoint(tuple._1, player.get)
        (tuple._1,Math.round(angle._1*10000000)/10000000.0,tuple._2)
      }//.sortBy(_._3).sortBy{_._2}
      .sortWith{(a,b) =>
        if (a._2 == b._2)
          a._3 < b._3
        else
          a._2 < b._2
      }
      
      val pIndex = pIndex2
      .map{tuple=>
        tuple._1
      }
      
      //println(pIndex2.mkString)
      
      val finalPolygonPoints: List[Point] = pIndex :+ pIndex(0)
      
      renderer.fillStyle = "blue"
      polyPoints.foreach {point =>
        renderer.fillRect(point.x, point.y, 5, 5)
      }
      
      renderer.fillStyle = "green"
      finalPolygonPoints.foreach {point =>
        renderer.fillRect(point.x, point.y, 5, 5)
      }
      
      
      
      renderer.beginPath
      renderer.strokeStyle = "green"
      finalSegments.foreach{segment =>
        renderer.moveTo(segment._1.x, segment._1.y)
        renderer.lineTo(segment._2.x, segment._2.y)
        renderer.stroke
      }
      renderer.closePath
      
      renderer.beginPath
      renderer.strokeStyle = "green"
      if (!finalPolygonPoints.isEmpty){
        renderer.moveTo(finalPolygonPoints(0).x,finalPolygonPoints(0).y)
        finalPolygonPoints.drop(1).foreach{segment =>
          renderer.lineTo(segment.x, segment.y)
          renderer.stroke
        }
        renderer.closePath
      }
  }
  
  def move(moveLittle: String) = {
    renderer.clearRect(0, 0, w, h)
    this.moveDirection(Point(0,-1))  
    renderPolygon
  }
  
  def move = {
    if (keysDown(38)||keysDown(37)||keysDown(39)||keysDown(40)){
      renderer.clearRect(0, 0, w, h)
      if (keysDown(38)) this.moveDirection(Point(0, -1))
      if (keysDown(37)) this.moveDirection(Point(-1, 0))
      if (keysDown(39)) this.moveDirection(Point(1, 0))
      if (keysDown(40)) this.moveDirection(Point(0, 1))
      clear()
      println("Player position:"+this.get.toString)
     renderPolygon 
    }
  }
}

class Dreapta(poly: RandomPolygon){
  
  def anglePoint(p: Point, player: Point): (Double,Double) = {
        val theta = Math.atan2(p.y - player.y, p.x - player.x)   // Compute the angle
        val r = Math.hypot(p.x, p.y)       // Compute the distance
        (theta, r)
  }
  
  def isBetween(a: Point, b: Point, c: Point): Boolean = {
    // c is in [a,b]
    val epsilon = 5
    
    val crossproduct = (c.y - a.y) * (b.x - a.x) - (c.x - a.x) * (b.y - a.y)
    val dotproduct = (c.x - a.x) * (b.x - a.x) + (c.y - a.y)*(b.y - a.y)
    val squaredlengthba = (b.x - a.x)*(b.x - a.x) + (b.y - a.y)*(b.y - a.y)
    if (abs(crossproduct) > epsilon) false
      else if (dotproduct < 0) false
        else if (dotproduct > squaredlengthba) false
          else true
  }
  
  def ecDreptei(p1: Point, p2: Point): (Double, Double, Double)  = {
    val a = p2.y - p1.y
    val b = p1.x - p2.x
    val c = p1.x * (p1.y - p2.y) - p1.y * (p1.x - p2.x)
    (a,b,c)
  }
  
  def intersectie(a1: Double, b1: Double, c1: Double, a2: Double, b2: Double, c2: Double): Point ={
    val x = (c2 * b1 - c1 * b2) / (a1 * b2 - a2 * b1)
    val y = (-c1 - a1 * x) / b1
    new Point(x, y)
  }
  
  def segmentsTwoPoints: List[(Point, Point)] = {
    var first = poly.pointList(0)
    val segments = poly.pointList.drop(1).map{ point =>
      val second = point
      val (f,s) = (first, second)
      first = second
      (f,s)
    }
    segments
  }.toSet.toList
  
  def segmentsPlayerPoints(list: List[Point], player: Point): List[(Point, Point)] = {
    val segments = list.map{ point =>
        (player, point)
      }
    segments
  }.toSet.toList
  
  def ecPlayerPoints(player: Point): List[(Double, Double, Double)] = {
    val ecuations = poly.pointList.map{ point =>
        ecDreptei(player, point)
      }.toList
    ecuations
  }.toSet.toList
  
  def ecTwoPoints: List[(Double, Double, Double)] = {
    val ecuations = segmentsTwoPoints.map {segment =>
      ecDreptei(segment._1, segment._2)
    }
    ecuations
  }.toSet.toList
  
  def intersectingPoints(l1:List[(Double, Double, Double)], 
    l2:List[(Double, Double, Double)]): List[Point]={
      val points = l1.flatMap{ l1p =>
        l2.map { l2p =>
          intersectie(l1p._1,l1p._2,l1p._3,l2p._1,l2p._2,l2p._3)
        }
      }
      points
    }.toSet.toList
}

class Enemies(var enemies: Seq[Point]) {
  import Page._
  import ScalaJSExample._
  var wave = 1
  var count = 0
  var x = Point(0,0)
  
  def get: Seq[Point] = enemies
  
  def move: Seq[Point] = {
    count += 1  
    if (enemies.isEmpty){
      enemies = for{
        x <- (20 until canvas.width.toInt by 50)
        y <- 0 until wave
      } yield {
        Point(x, 50 + y * 50)
      }
      wave += 1
    }
    
    enemies = enemies.map{ e =>
      val i = count % 200
      if (inCorners(e))
      if (i < 50) e.copy(x = e.x - 0.2)
      else if (i < 100) e.copy(y = e.y + 0.2)
      else if (i < 150) e.copy(x = e.x + 0.2)
      else e.copy(y = e.y + 0.2)
      else e.copy(x=e.x, y=e.y)
    }
    enemies
  }
  
  def render = {
    renderer.fillStyle = "yellow"
    for (enemy <- enemies){
      renderer.fillRect(enemy.x - 5, enemy.y - 5, 10, 10)
    }
  }
  
    
  def isDead: Seq[Point] = {
    enemies = enemies.filter( e =>
      !bullets.get.exists(b =>
        (e - b).length < 5
      )
    )
    enemies = enemies.filter( e =>
        !((e - player.get).length < 10)
    )
    enemies
  }
}

class Bullets(var bullets: Seq[Point]) {
  import Page._
  import ScalaJSExample._
  
  def get: Seq[Point] = bullets
  
  def move: Seq[Point] = {
    bullets = bullets.map(
      p => 
      if (inCorners(Point(p.x, p.y - 5)))
        Point(p.x, p.y - 5)
      else
        Point(p.x, p.y)
      )
    bullets
  }  
  
  def render = {
    renderer.fillStyle = "red"
    for (bullet <- bullets){
      renderer.fillRect(bullet.x - 2, bullet.y - 2, 4, 4)
    }
  }
}

object Graph {
  import ScalaJSExample._
  val graphs = Seq[(String, Double => Double)](
      //("red", x => x * (player.get.x/player.get.y) + (player.get.x/player.get.y) )
      ("red", sin)
      /*("green", x => 2 - abs(x % 8 - 4)),
      ("blue", x => 3 * pow(sin(x / 12), 2) * sin(x))*/
    ).zipWithIndex
  var x = 0.0
}

object ScalaJSExample extends js.JSApp{
  import Page._
  import Graph._
  
  val (h, w) = (Page.canvas.height, Page.canvas.width)
  val corners = Seq(Point(0, 0), Point(w,h))
  
  val playerInitPoint = Point(239,202)
  var polygon = new RandomPolygon( w / 2, h / 2, 180, 1.0, 0.7, 10 )
  polygon.render
  
  var player = new Player(playerInitPoint)
  var bullets = new Bullets(Seq.empty[Point])
  var enemies = new Enemies(Seq.empty[Point])
  
  player.move("aLittle")
  
  def inCorners(p: Point) = {
    if (p.y > corners(0).y && p.y < corners(1).y) true else false
  }
  
  def run = {
   //bullets.move
   //enemies.move
   player.move
  }
 
  def draw = {
    //renderer.clearRect(0, 0, w, h)
    player.render
    enemies.render
    enemies.isDead
    bullets.render

  }
  
  def graphDraw = {
    x = (x + 1) % w
    /*x = (x + 1) % w
    if (x == 0) renderer.clearRect(0, 0, w, h)
    else*/ for (((color, func), i) <- graphs) {
        val y = func(x/w * 75) * h/40 + h/4 * (i+0.5)
        renderer.fillStyle = color
        renderer.fillRect(x, y, 3, 3)
      }
  }
 
  val keysDown = collection.mutable.Set.empty[Int]
  
  def main() = {
 
    dom.onkeypress = {(e: dom.KeyboardEvent) =>
      if (e.keyCode == 32){ //bullets = new Bullets(player.get +: bullets.get)
        renderer.clearRect(0, 0, w, h)
        polygon = new RandomPolygon( w / 2, h / 2, 180, 1.0, 0.7, 10 )
        polygon.render
      }
    }
    dom.onkeydown = {(e: dom.KeyboardEvent) =>
      keysDown.add(e.keyCode.toInt)
    }
    dom.onkeyup = {(e: dom.KeyboardEvent) =>
      keysDown.remove(e.keyCode.toInt)
    }
    
    dom.setInterval(() => {run; draw/*; graphDraw*/}, 10)
  }
}