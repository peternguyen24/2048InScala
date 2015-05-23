import org.otfried.cs109.UI._

import java.awt.image.BufferedImage
import java.awt.{Graphics2D,Color,Font,BasicStroke}
import java.awt.geom._

// color for the background (shows in the gaps between tiles)
val backgroundColor = new Color(0xbbada0)

// colors of the tiles
val tileColors = Map(0 -> new Color(205, 192, 180),
		     2 -> new Color(0xeee4da),
		     4 -> new Color(0xede0c8),
		     8 -> new Color(0xf2b179),
		     16 -> new Color(0xf59563),
		     32 -> new Color(0xf67c5f),
		     64 -> new Color(0xf65e3b), 
		     128 -> new Color(0xedcf72),
		     256 -> new Color(0xedcc61),
		     512 -> new Color(0xedc850),
		     1024 -> new Color(0xedc53f),
		     2048 -> new Color(0xedc22e))
// color for other tiles (4096 etc.)
val otherTileColor = new Color(0x3c3a32)

val lightTextColor = new Color(119, 110, 101)
val darkTextColor = new Color(0xf9f6f2)

// returns color for the number in the tile, depending on tile value
def textColor(tileValue: Int) = 
  if (tileValue <= 4) lightTextColor else darkTextColor

// returns font size for the number in the tile, depending on tile value
def textSize(tileValue: Int) = 
  if (tileValue <= 64) 44
  else if (tileValue <= 512) 34
  else if (tileValue <= 2048) 27
  else 21

//number of digits of x
def nODigits(x: Int): Int ={
	var u=0
	var v=x
	while (v!=0) {
		u+=1;
		v=v/10
	}
	return u
}

//Table
class Field() {
	val table = Array(Array(0,0,0,0),Array(0,0,0,0),Array(0,0,0,0),Array(0,0,0,0))

	//Max number in table ~ score
	def max: Int = {
		var maxv=0;
		for (i<-0 until 4) 
			for (j<-0 until 4) {
				if (table(i)(j)>maxv) {
					maxv=table(i)(j)
				}
			}
		return maxv;
	}
	//Check if table is filled in all 16 cell
	def full():Boolean = {
		var x=0;
		for (i<-0 until 4) 
			for (j<-0 until 4) {
				if (table(i)(j)!=0) {
					x+=1;
				}
			}
		return (x==16);
	}

	//Insert 1 number to blank cell (2 or 4)
	def insert() {
		var x = -1;
		var y = 0;
		val r = scala.util.Random;
		while (x== -1 || table(x)(y)!=0) {
			x= r.nextInt(4)
			y= r.nextInt(4)
		}
		table(x)(y)=(r.nextInt(2)+1)*2
	}

	//Draw table ~ for fun
	override def toString()={
		var ndigit=0
		var res=""
		for (i<- 0 until 4 ){
			res=res+"o----o----o----o----o\n"
			res=res+"|    |    |    |    |\n"
			for (j<-0 until 4) {
				res=res+"|"
				if (table(i)(j)==0) {
					res=res+" "*4
				} else {
					ndigit = nODigits(table(i)(j))
					res=res+" "*(4-ndigit-(4-ndigit)/2);
					res=res+table(i)(j).toString
					res=res+" "*((4-ndigit)/2)
				}
			}
			res=res+"|\n";
			res=res+"|    |    |    |    |\n"
			res=res+"o----o----o----o----o\n"
		}
		res
	}

	//Press left button ~ all go to the left and merge
	def left() {
		var a=Array[Int](0,0,0,0)
		var canMerge=true;
		var id =0;
		for (line<- 0 until 4) {
			a=Array[Int](0,0,0,0)
			canMerge= false; 
			id =0;
			for (i<-0 until 4) {
				if (table(line)(i)==0) {

				} else if (canMerge && (a(id-1)==table(line)(i))) {
					a(id-1) *=2;
					canMerge=false;
				} else {
					a(id)=table(line)(i);
					id+=1;
					canMerge=true;
				}
			}
			for (i<-0 until 4) {
				table(line)(i)=a(i);
			}
		}
	}

	//all go to the right and merge
	def right() {
		var a=Array[Int](0,0,0,0)
		var canMerge=true;
		var id =0;
		for (line<- 0 until 4) {
			a=Array[Int](0,0,0,0)
			canMerge= false; 
			id =0;
			for (i<-(0 until 4).reverse) {
				if (table(line)(i)==0) {

				} else if (canMerge && (a(id-1)==table(line)(i))) {
					a(id-1) *=2;
					canMerge=false;
				} else {
					a(id)=table(line)(i);
					id+=1;
					canMerge=true;
				}
			}
			for (i<-0 until 4) {
				table(line)(i)=a(3-i);
			}
		}
	}

	//All go up and merge
	def up() {
		var a=Array[Int](0,0,0,0)
		var canMerge=true;
		var id =0;
		for (row<- 0 until 4) {
			a=Array[Int](0,0,0,0)
			canMerge= false; 
			id =0;
			for (i<-0 until 4) {
				if (table(i)(row)==0) {

				} else if (canMerge && (a(id-1)==table(i)(row))) {
					a(id-1) *=2;
					canMerge=false;
				} else {
					a(id)=table(i)(row);
					id+=1;
					canMerge=true;
				}
			}
			for (i<-0 until 4) {
				table(i)(row)=a(i);
			}
		}
	}

	//all go down and merge
	def down() {
		var a=Array[Int](0,0,0,0)
		var canMerge=true;
		var id =0;
		for (row<- 0 until 4) {
			a=Array[Int](0,0,0,0)
			canMerge= false; 
			id =0;
			for (i<-(0 until 4).reverse) {
				if (table(i)(row)==0) {

				} else if (canMerge && (a(id-1)==table(i)(row))) {
					a(id-1) *=2;
					canMerge=false;
				} else {
					a(id)=table(i)(row);
					id+=1;
					canMerge=true;
				}
			}
			for (i<-0 until 4) {
				table(i)(row)=a(3-i);
			}
		}
	}

	//Map user input with table's behaviours
	def push(a: Char) {
		if (a=='w') {
			up();
		} 
		if (a=='s') {
			down();
		}
		if (a=='a') {
			left();
		}
		if (a=='d') {
			right();
		}
	}


	//Update the canvas
	def draw(canvas: BufferedImage) {
		val g = canvas.createGraphics()
		var ww=0;
		g.setColor(backgroundColor)
  		g.fillRect(0, 0, canvas.getWidth, canvas.getHeight)
		for (i<-0 until 4) 
			for (j<-0 until 4) {
				g.setRenderingHint(java.awt.RenderingHints.KEY_ANTIALIASING, 
		     	java.awt.RenderingHints.VALUE_ANTIALIAS_ON)
				g.setColor(tileColors(table(i)(j)))
        		g.fill(new Rectangle2D.Double(10+j*90,10+i*90,80,80))
        		//
        		g.setColor(textColor(table(i)(j)) ) // a darker green
  				g.setFont(new Font("sans-serif",Font.PLAIN,textSize(table(i)(j)) )) 
  				ww = g.getFontMetrics().stringWidth(table(i)(j).toString)
  				g.drawString(table(i)(j).toString,10+j*90+40-ww/2 , 10+i*90+40+textSize(table(i)(j))/17*10)

			}
	}
	
}

//Create canvas and play
def playUI() {
  val canvas = new BufferedImage(370, 370, BufferedImage.TYPE_INT_RGB);
  show(canvas)

  val b = new Field
  b.insert()
  b.insert()

  while (true) {
    b.draw(canvas)
    show(canvas)
    val ch = waitKey()
    //println(ch)
    if ("awsd" contains ch) {
      if (b.full())  {
      	showMessage("You lose. Got "+b.max.toString+" !!!")
      	return;
      }
      b.push(ch)
      b.insert()
    }
  }
}


playUI()

