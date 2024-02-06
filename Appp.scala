import com.github.tototoshi.csv._
import java.io.File
import org.nspl.*
import org.nspl.awtrenderer.*
import org.nspl.data.HistogramData

implicit object MyFormat extends DefaultCSVFormat {
  override val delimiter = ';'
}

object Appp {
  @main
  def intergador2class(): Unit = {
    val pathDataFile = "C://Users//RomanEc//ArchivoPIntegrador//dsAlineacionesXTorneo.csv"
    val reader = CSVReader.open(new File(pathDataFile))
    //val contentFile: List[List[String]] = reader.all
    val contentFile: List[Map[String, String]] = reader.allWithHeaders()

    reader.close()
    println(contentFile.take(1))
    println(s"Filas: ${contentFile.length} y Columnas: ${contentFile(1).keys.size} ")

    charting(contentFile)

    def charting(data: List[Map[String, String]]): Unit = {
      val camisetas: List[Double] = data
        .filter(x => x("squads_position_name") == "forward" && x("squads_shirt_number") != "0")
        .map(x => x("squads_shirt_number").toDouble)
      //println(camisetas.)
      val histForwardShirtNum = xyplot(HistogramData(camisetas, 10) -> bar())(
        par
          .xlab("N° Shirt")
          .ylab("freq.")
          .main("Foward shirt number")
      )

      pngToFile(new File("C://Users//RomanEc//ArchivoPIntegrador//histograma.png"), histForwardShirtNum.build, 1000)
    }

    charting(contentFile)
  }
}
