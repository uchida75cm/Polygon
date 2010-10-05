package net.knserve.scala.polygon

import java.awt._
import java.awt.event._
import scala.collection.immutable.List

class Draw extends Frame with Runnable with MouseListener with MouseMotionListener {
	
	//イニシャライズ
	
	setTitle( "Polygon" )
	setSize( 400, 400 )
	setBackground( Color.black )
	setVisible( true )
	addMouseListener( this )
	addMouseMotionListener( this )
	addWindowListener( new WindowAdapter {
		override def windowClosing( e: WindowEvent ): Unit = {
			System.exit( 0 )
		}
	});
	
	
	//リスト
	
	var angle: List[ Double ] = List( 0, 0, 0 )
	var position: List[ Double ] = List( 0, 0, 0 )
	var siz: List[ Double ] = List( 1, 1, 1 )
	var eye: List[ Double ] = List( 0, 0, 400 )
	val mes: List[ String ] = List( "0","1","2","3","4","5","6","7" )
	
	val org: List[List[ Double ]] = List(
			List( -100, 100, -100, 1 ),
			List( 100, 100, -100, 1 ),
			List( 100, -100, -100, 1 ),
			List( -100, -100, -100, 1 ),
			List( -100, 100, 100, 1 ),
			List( 100, 100, 100, 1 ),
			List( 100, -100, 100, 1 ),
			List( -100, -100, 100, 1 )
	)
	
	val surfeces: List[List[ Int ]] = List(
			List( 0, 1, 2, 3 ),
			List( 4, 5, 6, 7 ),
			List( 0, 4, 5, 1 ),
			List( 1, 5, 6, 2 ),
			List( 2, 6, 7, 3 ),
			List( 3, 7, 4, 0 )
	)
	
	
	//関数
	
	def sin( d: Double ) = Math.sin( Math.toRadians( d ))
	def cos( d: Double ) = Math.cos( Math.toRadians( d ))
	
	def determinantX: List[List[ Double ]] = List(
			List( 1, 0, 0, 0 ),
			List( 0, cos(angle(0)), sin(angle(0)), 0 ),
			List( 0, -sin(angle(0)), cos(angle(0)), 0 ),
			List( 0, 0, 0, 1 )
	)
	
	
	def determinantY: List[List[ Double ]] = List(
			List( cos(angle(1)), 0, -sin(angle(1)), 0 ),
			List( 0, 1, 0, 0 ),
			List( sin(angle(1)), 0, cos(angle(1)), 0 ),
			List( 0, 0, 0, 1 )
	)
	
	def determinantZ: List[List[ Double ]] = List(
			List( cos(angle(2)), sin(angle(2)), 0, 0 ),
			List( -sin(angle(2)), cos(angle(2)), 0, 0 ),
			List( 0, 0, 1, 0 ),
			List( 0, 0, 0, 1 )
	)
	
	def determinantSize: List[List[ Double ]] = List(
			List( siz(0), 0, 0, 0 ),
			List( 0, siz(1), 0, 0 ),
			List( 0, 0, siz(2), 0 ),
			List( 0, 0, 0, 1 )
	)
	
	def determinantWorld: List[List[ Double ]] = List(
			List( 1, 0, 0, 0 ),
			List( 0, 1, 0, 0 ),
			List( 0, 0, 1, 0 ),
			List( position(0), position(1), position(2), 1 )
	)
	
	val changeDirection: List[List[ Double ]] => List[List[ Double ]] =
		p3d => ( 0 to 3 ).toList.map( i => ( 0 to 3 ).toList.map( j => p3d( j )( i ) ))
		
	val fusion: ( List[ Double ], List[ Double ] ) => Double =
		( a, b ) => ( 0 to 3 ).toList.map( idx => a( idx ) * b( idx ) ).foldLeft( 0.0 )( _ + _ )
		
	def affine( determinant: List[List[ Double ]], org: List[List[ Double ]] ): List[List[ Double ]] = {
		val idx = ( 0 to 3 ).toList
		org.map( i => idx.map( j => fusion( i, changeDirection( determinant )( j ) )))
	}
	
	val to2d: List[List[ Double ]] => List[List[ Int ]] =
		p3d => p3d.map( i => ( 0 to 1 ).toList.map( idx => ( 200. + 320.*i( idx ) / ( eye(2) + i(2) ) ).toInt ) )
	
	override def paint( g: Graphics ): Unit = {
		val p3d = affine( determinantX, affine( determinantY, affine( determinantZ, affine( determinantSize, affine( determinantWorld, org )))))
		g.setColor(Color.white)
		for { i <- 0 to 7 }
			g.drawString( String.valueOf( mes(i) ), to2d( p3d )( i )( 0 ), to2d( p3d )( i )( 1 ) )
	}
	
	//イベントハンドラ
	
	def mouseMoved( e: MouseEvent ): Unit = {
		angle = List( e.getY()-200, -( e.getX()-200 ), 0 )
	}
	def mouseDragged( e: MouseEvent ): Unit = {
		position = List( e.getX()-200, e.getY()-200, 0 )
	}
	def mouseEntered( e: MouseEvent ): Unit = {}
	def mouseExited( e: MouseEvent ): Unit = {}
	def mouseClicked( e: MouseEvent ): Unit = {}
	def mousePressed( e: MouseEvent ): Unit = {}
	def mouseReleased( e: MouseEvent ): Unit = {}
	
	
	//スレッド
	
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
