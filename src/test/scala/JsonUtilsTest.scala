import APIUtils.gson
import com.google.gson.{Gson, JsonArray, JsonObject, JsonPrimitive}
import org.scalatest.PrivateMethodTester
import org.scalatest.funsuite.AnyFunSuite

class JsonUtilsTest extends AnyFunSuite with PrivateMethodTester{
  val gson:Gson = new Gson()
  test("aplanarJsonObject"){
    val endJson = JsonUtils.aplanarJsonObject(gson.fromJson(APIUtils.getPaginacion("https://testpaginacion.free.beeceptor.com/",paginacion=APIUtils.Paginacion("nextLink",Seq("data"))),classOf[JsonObject]))
    println(endJson)
  }
  test("acumularJson"){
    val json:JsonObject = gson.fromJson(APIUtils.get("https://testpaginacion.free.beeceptor.com/aplanar"),classOf[JsonObject])
    val acumularJson:PrivateMethod[Seq[JsonPrimitive]] = PrivateMethod[Seq[JsonPrimitive]]('acumularJson)
    val acumulador:Seq[JsonPrimitive] = JsonUtils invokePrivate acumularJson(json,null)
    println(acumulador.size)
    acumulador.foreach(println)
  }
  test("aplanarJsonArray"){
    val json:JsonArray = gson.fromJson(APIUtils.get("https://testpaginacion.free.beeceptor.com/aplanar"),classOf[JsonArray])
    val endJsonArray = JsonUtils.aplanarJsonArray(json)
    println(endJsonArray)
  }
}
