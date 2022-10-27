import Contribuyente.imprimeDatos

//Ejercicio 10 extender de trait Analizador en vez de App
object AnalisisExploratorio extends Analizador {

  // ejercicio-1:
  // Popula la variable dataset con el resultado de la función loadDataset de Utilidades.
  // Ten en cuenta que se carga el csv completo, incluyendo las cabeceras, asegúrate de omitirlas (la primera fila)
  // IYM: Se utiliza tail que devuelve todos los elementos excepto el primero (cabecera)
  val dataset = Utilidades.loadDataset().tail

  // Implementa la función
  // ejercicio-2:
  // Número total de registros en el dataset.
  // IYM Se comprueba que no existan registros nulos o vacios en el dataset y se utiliza size */
  def totalDeRegistros(c: Seq[Contribuyente]): Int = {
    if (c.nonEmpty && c != null) c.length else 0
  }

  // Implementa la función
  // ejercicio-3:
  // Calcular la media de edad de todos los contribuyentes
  // IYM Con map se recorre la columna age para realizar el calculo*/
  def calculaEdadMedia(c: Seq[Contribuyente]): Double = {
    c.map(cont => cont.age).sum / totalDeRegistros(c)
  }

  // Implementa la función
  // ejercicio-4:
  // Calcular la media de edad de todos los contribuyentes que nunca se han casado.
  // hint: marital-status = Never-Married
  // IYM En primer lugar se crea filtro para never married y despues se calcula la media
  // En el pdf indica calcular la media de edad sin considerar a los que no se han casado*/
  def calculaEdadMediaNeverMarried(c: Seq[Contribuyente]): Double = {
    val filtro = c.filter(ms => ms.maritalStatus == "Never-married")
    filtro.map(x => x.age).sum / totalDeRegistros(filtro)
  }

  // Implementa la función
  // ejercicio-5:
  // Descubrir de cuántos países distintos provienen los contribuyentes
  // IYM con distinct y map es posible retornar los valores para paises distintos */
  def paisesOrigenUnicos(c: Seq[Contribuyente]): Seq[String] = {
    val x = c.map(_.nativeCountry).distinct
    x.filter(_.nonEmpty)
  }

  // Implementa la función
  // ejercicio-6:
  // De todos los contribuyentes, ¿cómo se distribuye por género?. Devuelve el porcentaje de hombres
  // y el de mujeres, en ese orden, (porcentajeDeHombres, porcentajeDeMujeres)
  // IYM en primer lugar se crean variables aux para obtener el size para hombres y mujeres y despues calcular % */
  def distribucionPorGeneros(c: Seq[Contribuyente]): (Double, Double) = {
    val mujeres = c.count(x => x.sex == "Female")
    val hombres = c.count(x => x.sex == "Male")
    val porcentajeDeHombres = hombres * 100 / totalDeRegistros(c)
    val porcentajeDeMujeres = mujeres * 100 / totalDeRegistros(c)
    (porcentajeDeHombres, porcentajeDeMujeres)
  }

  // Implementa la función
  // ejercicio-7:
  // Encuentra el tipo de trabajo (workclass) mejor remunerado. El trabajo mejor remunerado es aquel trabajo donde el
  // porcentaje de los contribuyentes que perciben ingresos (income) superiores a ">50K" es mayor que los contribuyentes
  // cuyos ingresos son "<50K".
  def trabajoMejorRemunerado(c: Seq[Contribuyente]): String = {
    val temp1 = c.groupMap(_.workclass)(_.income)
    val temp2 = temp1.groupMap(_._1)(x =>
      (x._2.count(_ == ">50K") > x._2.count(_ == "<=50K"))
    )
    val temp3 = temp2
      .map(x => (x._1, x._2.mkString))
      .filter(_._2 == "true")
      .keys
      .mkString
    temp3
  }
  // Implementa la función
  // ejercicio-8:
  // Cuál es la media de años de educación (education-num) de aquellos contribuyentes cuyo país de origen no es
  // United-States
  /*IYM se crea el filtro que quita a USA de los registros y se realiza el calculo */
  def aniosEstudiosMedio(c: Seq[Contribuyente]): Double = {
    val fil = c.filter(ms => ms.nativeCountry != "United-States")
    fil.map(x => x.educationNum).sum / totalDeRegistros(fil)
  }

  println(
    s" -> Dataset tiene un total de registros: ${totalDeRegistros(c = dataset)}"
  )
  println(
    s" -> En el dataset, los contribuyentes tienen una edad media: ${calculaEdadMedia(c = dataset)}"
  )
  println(
    s" -> En el dataset, los contribuyentes que no se han casado tienen una edad media de: ${calculaEdadMediaNeverMarried(c = dataset)}"
  )
  println(
    s" -> Los contribuyentes provienen de distintos países como: ${paisesOrigenUnicos(c = dataset)
      .mkString(",")} \n " +
      s"siendo un total de ${paisesOrigenUnicos(c = dataset).length} paises distintos"
  )
  println(
    s" -> Los contribuyentes se distribuyen en (hombres - mujeres): ${distribucionPorGeneros(c = dataset)}"
  )
  println(
    s" -> El tipo de trabajo mejor remunerado en el dataset es: ${trabajoMejorRemunerado(c = dataset)}"
  )
  println(
    s" -> La media de años de estudio de los contribuyenes de origen distinto a United States es: ${aniosEstudiosMedio(c = dataset)}"
  )

  // ejercicio-12
  // llama a la función imprimeContribuyentes pasándole los primeros 5 contribuyentes del dataset.
  imprimeContribuyente(dataset.take(5))
}
