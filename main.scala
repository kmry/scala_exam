/*
nanoc向けslimを作成します。
========== 仕様 ==========
[1] 分野別企業一覧を作成し、csv化(BMCメンバー)
[2]csv毎の処理
(1) "会社概要"が来るまでループ
会社概要が来たら、二社を抽出

(2)以下について、2社ずつ抜いていく


===========================

*/

import java.io._
                
object Ex2 {

//分野別ページ用slim
def category(name:String) ={
	val str = name match {
		case "Mono" =>"ものづくり"
		case "Food" =>"食産業"
		case _ =>"その他"
	}
s""".hero-unit
  hr
  h2 ${str} 分野　企業一覧
  p
  ul"""
}
//企業別ページ用slim
def head(name:String) =s""".hero-unit
  h1 ${name}
  p
  p 概要
  table.table-bordered
"""

def slimTemp(a:String,b:String) = 	s"""
	tr
		td.item ${a}
		td.name ${b}"""

def tail(name:String) =s""".hero-unit
.row
  .span6
    p 
  .span6
    p 
"""

	def makePW (fn:String)= {
		val file = new File(fn)
		val fos = new FileOutputStream(file)
           val osw = new OutputStreamWriter(fos, "UTF-8")
		//ファイル出力用PrintWriter cf.http://j.mp/M9JFQW
		new PrintWriter(new BufferedWriter(osw))
	}
/*
	関数リテラルの記法
		(引数1:型, 引数2:型, ･･･) => { 処理 }
	 例
		def read( in: java.io.InputStream , f:(String) => Unit ) =
*/
def splitCOL(line:String, fn:(Array[String]) => Unit) :String= {// 処理した文字列の後半部分(arr(1))を返す 
	def go(spl:String) = {
		var rtn =""
		if (line.indexOf(spl) > 0){
			val arr =line.split(spl)
			if (arr.length >1) {
				fn(arr)
				rtn =arr(1)
			} 
			rtn
		} else "err."
	}
	val re = go("：")
	if (re != "err.") re else {
		print(":")
		go(":")
	}
}
/////////////////////////////////////////////////////////////////////////////////////

  def main(args: Array[String]) = {
		val pw0 = 	makePW("../data/index.slim")
		//対象分野事にindex.slimに追加していく
		pw0.println(
"""
h1 イチオシ商材一覧
p
""")
		pw0.println(
			make ("Mono","m", List(4,6,8,11,13,15) ))
		pw0.println(
			make ("Food","f", List(1,2,5) ))
		pw0.close
	
  }
	def make (target:String,t:String, ids:List[Int]) = { // 対象企業一覧を返す。
		def output(br:BufferedReader,pw: PrintWriter,str:String) :String={		//企業名を返す
			var line1=br.readLine()
			val companyName=
				splitCOL (line1,(arr:Array[String]) =>{
					pw.println(head(arr(1)))
					pw.println(slimTemp(arr(0),arr(1)))
				})

			do {
				line1 =br.readLine()
				splitCOL (line1,(arr:Array[String]) =>{
					pw.println(slimTemp(arr(0),arr(1)))
				})
			} while (line1 != "==JPG==")
			//画像集
			pw.println("\t.aw_showcase")
			try {
				while ({line1 =br.readLine(); line1.length>0}) 
					splitCOL (line1,(arr:Array[String]) =>{
							//企業ページへのhrefリンクを出力
						pw.println(s"\t\t== aw_image_slide('/assets/images/${target}/${str}/${arr(0)}.JPG', '${arr(1)}')")
					})
			} catch {
			case _ => print(_) 
			}
			companyName
		}

		//10/9 分野別トップベージの作成 
		var indexSnips =category(target)
		//処理対象ファイルの読み込み
		ids foreach {id =>
			val str =f"${t}${id}%02d"
			val base = s"../data/${target}/"
			val src = s"${base}${str}/${str}.txt"
			val filereader = new FileReader(src);
			val br = new BufferedReader(filereader);
			val out_dir= s"${base}/${str}"
			(new File(out_dir)).mkdirs()
				//"mkdirs"メソッド : 作成ディレクトリの親ディレクトリなき場合、親ディレクトリもまとめて作成
			//ファイル出力用PrintWriter cf.http://j.mp/M9JFQW
			val pw = 	makePW(out_dir+"/index.slim")
			val companyName = output (br,pw,str)
			indexSnips += s"\tli\n\t\ta href='${str}' ${companyName}\n"
			pw.close()
			
		}
		indexSnips
	}

}
