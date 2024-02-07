import com.github.tototoshi.csv._
import java.io.File
import org.nspl.*
import org.nspl.awtrenderer.*
import org.nspl.data.HistogramData

implicit object MyFormat extends DefaultCSVFormat {
  override val delimiter = ';'
}


object Datos {
  @main// Transforma la función manipulacionDatos en un método ejecutable.
  def manipulacionDatos()=
    val pathDataFile: String = "C://Users//RomanEc//ArchivoPIntegrador//dsPartidosYGoles.csv" // Indica la ruta del archivo CSV.
    val reader = CSVReader.open(new File(pathDataFile)) // Crea un objeto CSVReader que se puede usar para leer datos del archivo CSV especificado por pathDataFile.
    val contentFile: List[Map[String, String]/*Clave y Valor son String.*/] = reader.allWithHeaders() // Almacenar el contenido con encabezados del archivo en la variable.
    reader.close() // Cerrar el canal de lectura, ya no se necesita abierto.
    /* println(contentFile.take(3)) // Toma los tres primeros elementos de la lista.*/
    println(s"******* ESTADISTICAS DESCRIPTIVAS *******")

    def stadiums(data: List[Map[String, String]]) =
      val capacidadesEstadios = data
        .map(row => ( // Lista de tuplas de tres
          row("stadiums_stadium_name"), // Nombre de los estadios.
          row("stadiums_city_name"), //Nombre de la ciudad donde estan los estadios.
          row("stadiums_stadium_capacity").toInt, //Capacidad de cada estadio
        ))
        .distinct // Selecciona Valores únicos
        .groupBy(_._1) // Agrupa por nombres de los estadios
        .map((t2 => (t2._1, t2._2.map(_._3).sum))) //Transforma a una nueva coleccion. Extraer la tercera componente de cada tupla en t2._2 y sumará.
        .toList
      // Calcular capacidad mínima y máxima
      val capacidadMinima = capacidadesEstadios.minBy(_._2)._2/*Se obtiene  la tupla que tiene el mínimo valor en el segundo elemento, luego se accede al segundo elemento de esa tupla.*/
      val capacidadMaxima = capacidadesEstadios.maxBy(_._2)._2//Se obtiene  la tupla que tiene el máximo valor en el segundo elemento, luego se accede al segundo elemento de esa tupla
      val promedioCapacidades = capacidadesEstadios.map(_._2).sum.toDouble / capacidadesEstadios.size
      println(s"Capacidad mínima de estadios: $capacidadMinima")
      println(s"Capacidad máxima de estadios: $capacidadMaxima")
      println(s"Promedio de Capacidades de Estadios: ${"%.2f".format(promedioCapacidades)}") // Obtener resultado con dos decimales.
    stadiums(contentFile)

    println(s"******* FIN DE ESTADISTICAS *******")
    println(s"******* PREGUNTAS DE CONSULTA *******")
    /*PREGUNTAS*/
    println(s" 1. ¿Qué equipo ha marcado más goles?")

    def Equipos(data: List[Map[String, String]]): Unit = // Lista de Mapas (Clave/Valor)
      val dataGoles = data
        .map(row => ( // Lista de tuplas de cuatro
          row("home_team_name"), //nombre del E.local
          row("away_team_name"), //nombre del E. visitante
          row("matches_home_team_score").toInt, // Goles E.local
          row("matches_away_team_score").toInt // Goles E. visitante
        ))
        .distinct // Selecciona valores unicos.
        .flatMap //Transforma una colección de partidos en una nueva colección.
            {  case (equipoLocal, equipoVisitante, golesLocal, golesVisitante) => List((equipoLocal, golesLocal), (equipoVisitante, golesVisitante)) //Lista de dos tuplas (Equipos, Goles)
           } //Aplanar la lista de listas resultante en una única lista.
        .groupBy(_._1) // Agrupa por el nombre del equipo
        .map(t2 => (t2._1, t2._2.map(_._2).sum)) //  Transforma a una nueva coleccion. Extraer la segunda componente de cada tupla en t2._2 y sumará.
        .toList // Transforma en una lista
      val maximoGoles = dataGoles.maxBy(_._2) //  Encuentra el valor máximo en función de la segunda componente de cada tupla.
      println(s"El equipo ${maximoGoles._1} es el más goleador con ${maximoGoles._2} goles") // Imprime resultado  en pantalla

    Equipos(contentFile) //Invocación


    println(s" 2. ¿Cúal es el promedio de partidos jugados por mundial?")
    def promedioPartidosPorMundial(datos: List[Map[String, String]]):Double= { // Lista de Mapas: Devuelve un Double
      val partidosPorMundial = datos.groupBy(_("matches_tournament_id"))
      /*Agrupa los datos por el identificador del torneo: (identificadores de los torneos, listas de partidos asociados a cada torneo)*/
      val totalMundiales = partidosPorMundial.size // Obtiene el total de torneos distintos
      val totalPartidos = partidosPorMundial.values //devuelve una colección de todas las listas de partidos
        .flatten // Combina a una sola lista
        .size.toDouble //Obtener la cantidad total de partidos.

      if (totalMundiales > 0) { //Condicional donde pregunta si existe al menos un torneo en el mundial.
        totalPartidos.toDouble / totalMundiales // Calcula el promedio dividiendo el total de partidos entre el total de torneos
      } else {
        0.0 // devolvera esto como valor predeterminado
      }
    }
    val promedioPartidos = promedioPartidosPorMundial(contentFile) // Almacena el resultado
    println(s"El promedio de partidos jugados por mundial es: $promedioPartidos")


    println(s" 3. ¿Cúantos partidos tienen un resultado diferente de Empate?")
    // Función para obtener el total de partidos que tuvieron un resultado diferente de empate
    def getTotalPartidos(data: List[Map[String, String]]): Int = { //Parametro: Lista de Mapas
      // Aplica distinctBy para eliminar filas duplicadas basadas en el campo:
      val distinctData = data.distinctBy(row => row.getOrElse("matches_match_id", ""))
      // Retorna el número de filas donde el resultado es diferente de "empate"
      distinctData //Contar el número de filas que cumplen las condiciones especificas.
        .count(row => row.contains("matches_match_id") //Verifica si la fila row contiene la clave "matches_match_id".
          && row.contains("matches_result")  //Verifica si la fila row contiene la clave "matches_match_result".
          && row("matches_result") != "draw") //Verifica si el valor que contiene la clave es diferente.
    }

    // Llamada a la función para obtener el total de partidos con resultado diferente de empate
    val totalPartidos: Int = getTotalPartidos(contentFile)
    println(s"Total de partidos con resultado diferente de empate: $totalPartidos partidos")  // Imprime el resultado en la consola
    println(s"******* FIN DE PREGUNTAS *******")

    println(s"******* GRÁFICAS *******")
    println(s"Las grafias se encuentran en la ruta: C://Users//RomanEc//ArchivoPIntegrador")
    val pathDataFile2 = "C://Users//RomanEc//ArchivoPIntegrador//dsAlineacionesXTorneo.csv"
    val reader2 = CSVReader.open(new File(pathDataFile2))
    val contentFile2: List[Map[String, String]] = reader2.allWithHeaders()
    reader2.close()
    // Crear histogramas llamando a la función charting con diferentes columnas y etiquetas
    charting(contentFile, "squads_shirt_number", "Número de Camiseta")
    charting(contentFile, "squads_position_name", "Posición")
    charting(contentFile, "players_birth_date", "Fecha de Nacimiento")
    charting(contentFile, "squads_team_id", "ID del Equipo")

    charting2(contentFile2)

    def charting2(data: List[Map[String, String]]): Unit = {
      val jugadoresDelanteros: List[Double] = data
        .filter(x => x("squads_tournament_id") == "WC-2015")
        .map(x => x("players_forward").toDouble)
      //HISTOGRAMA QUE MUESTRA LA FRECUENCIA DE LOS JUGADORES DELANTEROS DEL MUNDIAL WC-2015
      val histForwardShirtNum = xyplot(HistogramData(jugadoresDelanteros, 10) -> bar())(
        par
          .xlab("N° Jugadores Delanteros")
          .ylab("freq.")
          .main("WC-2015 && Jugadores Delanteros")
      )
      val histFordwardShirtNumber = xyplot()
      pngToFile(new File("C://Users//RomanEc//ArchivoPIntegrador//jugadoresdelanteros.png"), histForwardShirtNum.build, 1000)

    }

      charting2(contentFile2)

      // Definir la función charting que crea un histograma y lo guarda como un archivo PNG
      def charting(data: List[Map[String, String]], columnName: String, label: String): Unit = {
        // Filtrar y mapear los valores de la columna especificada como Double
        val columnValues: List[Double] = data
          .filter(x => x.contains(columnName) && isNumeric(x(columnName)))
          .map(x => x(columnName).toDouble)

        // Crear un histograma utilizando la biblioteca NSPL
        val hist = xyplot(HistogramData(columnValues, 10) -> bar())(
          par
            .xlab(label)
            .ylab("freq.")
            .main(s"Histograma de $label")
        )

        // Guardar el histograma como un archivo PNG en una ubicación específica
        val fileName = s"histograma_${columnName.replaceAll("\\s", "_")}.png"
        pngToFile(new File(s"C:/Users/RomanEc/ArchivoPIntegrador/$fileName"), hist.build, 1000)
      }

       println(s"******* FIN DE GRÁFICAS *******")
      // Función para verificar si una cadena es numérica
      def isNumeric(s: String): Boolean = s.matches("[-+]?\\d*\\.?\\d+") // verifica si la cadena cumple con el formato de un número, ya sea entero, decimal, positivo o negativo
    }
