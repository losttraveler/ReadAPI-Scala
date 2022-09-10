import org.scalatest.PrivateMethodTester
import org.scalatest.funsuite.AnyFunSuite


class APIUtilsTest extends AnyFunSuite with PrivateMethodTester {
  test("PruebaGET"){
    APIUtils.get("https://expired.badssl.com")

    assert(APIUtils.get("a")==null) //Error de String != url
    assert(APIUtils.get("https://res.in/api/users")==null) //La url no es un dominio existente
    //println(APIUtils.get("https://api.agify.io/?name=rafa"))

    //println(APIUtils.post("https://res.in/api/users",body="{\"name\": \"morpheus\",\"job\": \"leader\"\n}"))
  }
  test("PruebaGetPaginacion"){
    val getPaginacion:PrivateMethod[String] = PrivateMethod[String]('getPaginacion)
    val json:String = APIUtils invokePrivate getPaginacion()
  }
  test("PruebaPaginacion"){
    println(APIUtils.getPaginacion("https://testpaginacion.free.beeceptor.com/",paginacion = APIUtils.Paginacion("nextLink",Seq("data","table"))))
  }
}
