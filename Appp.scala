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
    val pathDataFile = "C://Users//RomanEc//ArchivoPIntegrador//dsPartidosYGoles.csv"
    val reader = CSVReader.open(new File(pathDataFile))
    //val contentFile: List[List[String]] = reader.all
    val contentFile: List[Map[String, String]] = reader.allWithHeaders()

    reader.close()
    println(contentFile)

    

    def obtenerNombrePais(codigo: String): String = codigo match {
      case "ARG" => "Argentina"
      case "BRA" => "Brasil"
      // Agrega más casos según tus necesidades
      case _ => codigo // Devuelve el código si no hay coincidencia
    }

    def paisesMasParticipantes(datos: List[Map[String, String]]) = {
      val codigosPaisesHome = datos.flatMap(_("home_team_name")).distinct
      val codigosPaisesAway = datos.flatMap(_("away_team_name")).distinct

      val paisesHome = codigosPaisesHome.map(obtenerNombrePais)
      val paisesAway = codigosPaisesAway.map(obtenerNombrePais)

      (paisesHome ++ paisesAway).distinct
    }
    // Ejemplo de uso
    val paisesParticipantes = paisesMasParticipantes(contentFile)
    println(s"Países que han participado en más mundiales: $paisesParticipantes")


    /*def mejorPromedioGolesConCantidad(datos: List[Map[String, String]]): (String, Double, Int) = {
      val golesPorMundial = datos.groupBy(_("tournaments_tournament_name")).mapValues { partidos =>
        val totalGoles = partidos.map(row => row("matches_home_team_score").toInt + row("matches_away_team_score").toInt).sum
        val totalPartidos = partidos.length
        if (totalPartidos > 0) (totalGoles.toDouble / totalPartidos, totalGoles)
        else (0.0, 0)
      }
    //println(contentFile.take(3))

      val (mejorPromedio, totalGolesMejorPromedio) = golesPorMundial.maxBy(_._2._1)._2
      val mejorPromedioMundial = golesPorMundial.maxBy(_._2._1)._1

      (mejorPromedioMundial, mejorPromedio, totalGolesMejorPromedio)
    }

    val (mejorPromedioMundial, mejorPromedio, totalGolesMejorPromedio) = mejorPromedioGolesConCantidad(contentFile)
    println(s"El mundial con el mejor promedio de goles es: $mejorPromedioMundial con $totalGolesMejorPromedio goles")
    println(s"Mejor promedio de goles por partido: $mejorPromedio")
/**/
def promedioPartidosPorMundial(datos: List[Map[String, String]]): Double = {
  val partidosPorMundial = datos.groupBy(_("matches_tournament_id")).mapValues(_.length)
  val totalMundiales = partidosPorMundial.size

  if (totalMundiales > 0) {
    val totalPartidos = partidosPorMundial.values.sum
    totalPartidos.toDouble / totalMundiales
  } else {
    0.0
  }
}

    val promedioPartidos = promedioPartidosPorMundial(contentFile)
    println(s"El promedio de partidos jugados por mundial es: $promedioPartidos")
    /*
    */


     */


    /*println(s"Filas: ${contentFile.length} y Columnas: ${contentFile(1).keys.size} ")*/

    /*charting(contentFile)

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

      pngToFile(new File("C://Users//RomanEc//ArchivoPIntegrador//histogra.png"), histForwardShirtNum.build, 1000)
    }

    charting(contentFile)*/
  }
}

