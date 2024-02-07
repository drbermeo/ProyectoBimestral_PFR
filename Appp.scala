/*Importacion de Paquetes*/

import com.github.tototoshi.csv.*
import java.io.File

/* Se utiliza para personalizar el formato de los archivos CSV que se leen o escriben.*/
implicit object MyFormat extends DefaultCSVFormat {
  override val delimiter: Char = ';' // Caractér que está separando cada uno de los valores del archivo.
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

  // Supongamos que tienes una lista de mapas llamada partidos que contiene información sobre los partidos.
  // Cada mapa representa la información de un partido.

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

   def Equipos(data: List[Map[String, String]]): Unit =
     val dataGoles = data
       .map(row => ( // Lista de tuplas de cuatro
         row("home_team_name"), //nombre del E.local
         row("away_team_name"), //nombre del E. visitante
         row("matches_home_team_score").toInt, // Goles E.local
         row("matches_away_team_score").toInt // Goles E. visitante
       ))
       .distinct // Selecciona valores unicos.
       .flatMap //Transforma una colección de partidos en una nueva colección.
           {
       case (equipoLocal, equipoVisitante, golesLocal, golesVisitante) =>
         List((equipoLocal, golesLocal), (equipoVisitante, golesVisitante)) //Lista de dos tuplas (Equipos, Goles)
        } //Aplanar la lista de listas resultante en una única lista.
       .groupBy(_._1) // Agrupa por el nombre del equipo
       .map(t2 => (t2._1, t2._2.map(_._2).sum)) //  Transforma a una nueva coleccion. Extraer la segunda componente de cada tupla en t2._2 y sumará.
       .toList // Transforma en una lista
     val maximoGoles = dataGoles.maxBy(_._2) //  Encuentra el valor máximo en función de la segunda componente de cada tupla.
     println(s"El equipo ${maximoGoles._1} es el más goleador con ${maximoGoles._2} goles") // Imprime resultado  en pantalla

   Equipos(contentFile) //Invocación
   println(s"******* FIN DE PREGUNTAS *******")
   println(s"******* GRÁFICAS *******")
   println(s"******* FIN DE GRÁFICAS *******")



}
