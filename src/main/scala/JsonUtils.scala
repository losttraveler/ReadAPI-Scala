import com.google.gson.{JsonArray, JsonElement, JsonObject}

import scala.collection.JavaConversions._

/**
 * JsonUtils 1.1
 * Una serie de metodos para trabjar con Jsons utilizando Gson
 */
object JsonUtils {

  /**
   * Recibe un Json como JsonObject y coloca todos sus elementos en el mismo nivel
   * @param json El archivo Json el cual va a ser igualado
   * @return Un JsonObject con todos sus elementos en el mismo nivel
   */
  def aplanarJsonObject(json:JsonObject):JsonObject = {
    val endJson:JsonObject = new JsonObject()
    val acumulacion:Seq[(String,JsonElement)] = acumularJson(json)
    acumulacion.foreach(pair=>{
      endJson.add(pair._1,pair._2)
    })
    endJson
  }

  /**
   * Recibe un Json como JsonArray, y recorre todos sus elementos aplanandolos
   * @param jsonArray El JsonArray que va a recorrer y aplanar
   * @return Un JsonArray con los elementos del array aplanados
   */
  def aplanarJsonArray(jsonArray:JsonArray):JsonArray = {
    val endJsonArray:JsonArray = new JsonArray()
    jsonArray.foreach(elem=>{
      if(elem.isJsonArray){
        endJsonArray.add(aplanarJsonArray(elem.getAsJsonArray))
      }else if(elem.isJsonObject){
        endJsonArray.add(aplanarJsonObject(elem.getAsJsonObject))
      }else{
        endJsonArray.add(elem)
      }
    })
    endJsonArray
  }


  /**
   * Metodo PRIVADO que acumula todos los elementos de un Json en una Sequencia de JsonElements con sus nombres
   * @param json Json del que vamos a acumular los elementos
   * @param name Parametro para definir el nombre de la propiedad acumulada
   * @return Sequencia de pares nombre de propiedad y elemento de la propiedad
   */
  private def acumularJson(json:JsonObject,name:String=null):Seq[(String,JsonElement)]={
    json.entrySet().foldLeft(Seq[(String,JsonElement)]())((ac,entry)=>{
      if(entry.getValue.isJsonObject){
        val newSeq = acumularJson(entry.getValue.getAsJsonObject,if(name!=null)name+"_"+entry.getKey else entry.getKey)
        ac++newSeq
      }else if(entry.getValue.isJsonArray){
        val arr:JsonArray = entry.getValue.getAsJsonArray.foldLeft(new JsonArray())((ac,elem)=>{
          if(elem.isJsonPrimitive){
            ac.add(elem)
          }else if (elem.isJsonArray){
            ac.add(elem)
          }else{
            acumularJson(elem.getAsJsonObject,if(name!=null)name+"_"+entry.getKey else entry.getKey).foreach(p=>{
              ac.add(p._2)
            })
          }
          ac
        })
        ac:+(if(name!=null)name+"_"+entry.getKey else entry.getKey,arr)
      }
      else{
        ac:+(if(name!=null)name+"_"+entry.getKey else entry.getKey,entry.getValue.getAsJsonPrimitive)
      }
    })
  }
}
