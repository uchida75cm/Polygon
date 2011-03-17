package net.knserve.scala.polygon {
  import java.awt.{ List => _, _ }
  import java.awt.event._

  object Main {
    def main( args: Array[ String ] ): Unit = {
      val p0 = Point3( -100,  100, -100, 1 )
      val p1 = Point3(  100,  100, -100, 1 )
      val p2 = Point3(  100, -100, -100, 1 )
      val p3 = Point3( -100, -100, -100, 1 )
      val p4 = Point3( -100,  100,  100, 1 )
      val p5 = Point3(  100,  100,  100, 1 )
      val p6 = Point3(  100, -100,  100, 1 )
      val p7 = Point3( -100, -100,  100, 1 )
      val cubic = Cubic(
        Face( p0, p1, p2, p3 ),
        Face( p4, p5, p6, p7 ),
        Face( p0, p4, p5, p1 ),
        Face( p1, p5, p6, p2 ),
        Face( p2, p6, p7, p3 ),
        Face( p3, p7, p4, p0 )
      )
      val drw = new Draw( cubic )
      drw.setBackground( Color.black )
      drw.setSize( 400, 400 )
      drw.setVisible( true )
      val thr = new Thread( drw )
      thr.start
    }
  }

  case class Point3( x: Double, y: Double, z: Double, s: Double ) {

    type Matrix[T] = List[ List[T] ]
    type Rotation[T] = List[ T ]

    def determinantZ( r: Rotation[ Double ] ): Matrix[ Double ] = List(
      List( cos(r(2)), sin(r(2)), 0, 0 ),
      List( -sin(r(2)), cos(r(2)), 0, 0 ),
      List( 0, 0, 1, 0 ),
      List( 0, 0, 0, 1 )
    ) 

    def determinantY( r: Rotation[ Double ] ): Matrix[ Double ] = List(
      List( cos(r(1)), 0, -sin(r(1)), 0 ),
      List( 0, 1, 0, 0 ),
      List( sin(r(1)), 0, cos(r(1)), 0 ),
      List( 0, 0, 0, 1 )
    )

    def determinantX( r: Rotation[ Double ] ): Matrix[ Double ] = List(
      List( 1, 0, 0, 0 ),
      List( 0, cos(r(0)), sin(r(0)), 0 ),
      List( 0, -sin(r(0)), cos(r(0)), 0 ),
      List( 0, 0, 0, 1 )
    )

    def sin( d: Double ) = math.sin( math.toRadians( d ))
    def cos( d: Double ) = math.cos( math.toRadians( d ))

    def affine( m: Matrix[Double] ): Point3 = {
      def fusion( a: List[Double], b: List[Double], f: (Double, Double) => Double ) =
        a.zip( b ).map( org => f( org._1, org._2 ) )
      Point3( fusion( toList, List.transpose( m )(0), _*_ ).foldLeft(0.0)(_+_),
              fusion( toList, List.transpose( m )(1), _*_ ).foldLeft(0.0)(_+_),
              fusion( toList, List.transpose( m )(2), _*_ ).foldLeft(0.0)(_+_),
              fusion( toList, List.transpose( m )(3), _*_ ).foldLeft(0.0)(_+_)
            )
    }

    def rotate( r: Rotation[ Double ] ) =
      affine( determinantX( r ) ).
        affine( determinantY( r ) ).
          affine( determinantZ( r ) ) 

    def toList = List( x, y, z, s )
  }
  
  case class Face( points: Point3* ) {
    def toList: List[ Point3 ] = points.toList
  }
  
  case class Cubic( faces: Face* ) {
    def toList: List[ Face ] = faces.toList
  }

  object World {
    val axis3Depth = 400
    val screenDepth = 320
  }

  object Draw {
    val axis2X = 200
    val axis2Y = 200
    val convert2d: ( Double, Double, Double ) => Int =
      ( x1, L1, L2 ) => (( x1 * L2 ) / L1).toInt
    def P3toP2( p: Point3 ): Tuple2[Int, Int] = (
      convert2d( p.x, World.axis3Depth+p.z, World.screenDepth ),
      convert2d( p.y, World.axis3Depth+p.z, World.screenDepth )
    )

    def polygonFactory( fc: List[Point3] ): Polygon = {
      val plist = fc.map( p => P3toP2( p ) )
      new Polygon(
        plist.map( p => p._1 + axis2X ).toArray,
        plist.map( p => p._2 + axis2Y ).toArray,
        plist.length
      )
    }
  }

  class Draw( cubic: Cubic ) extends Frame with Runnable with MouseMotionListener {
    
    addMouseMotionListener( this )
    addWindowListener( new WindowAdapter {
        override def windowClosing( e: WindowEvent ): Unit = {
          System.exit( 0 )
        }
    })

    val c: Cubic = cubic
    var r: List[Double] = List( 0.0, 0.0, 0.0 )

    override def paint( g: Graphics ): Unit = {
      g.setColor( Color.white )
      val rotated = c.toList.map( fc => fc.toList.map( p => p.rotate( r ) ) )
      rotated.foreach( fc => g.drawPolygon( Draw.polygonFactory( fc ) ) )
    }

    def mouseMoved( e: MouseEvent ): Unit = {
      r = List( e.getY-Draw.axis2Y, -( e.getX-Draw.axis2X ), 0.0 )
    }

    def mouseDragged( e: MouseEvent ): Unit = {
    }

    def run(): Unit = {
        try {
            while( true ) {
                repaint
                Thread.sleep( 30 )
            }
        } catch {
            case e: InterruptedException =>
                println( e.getMessage )
            }
        }
  }
}
