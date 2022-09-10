package com.rafa.utils

import com.google.gson.{Gson, JsonArray, JsonObject}
import org.apache.http.client.ClientProtocolException
import org.apache.http.client.methods.{HttpDelete, HttpGet, HttpPost, HttpPut}
import org.apache.http.entity.StringEntity
import org.apache.http.impl.client.HttpClientBuilder
import org.apache.http.util.EntityUtils
import org.apache.log4j.{Level, Logger}

import java.net.UnknownHostException
import javax.net.ssl.SSLHandshakeException

/**
 * APIUtils 1.1
 * Una forma sencilla de consumir metodos APIRESt
 */
object APIUtils {

  /**
   *
   * @param siguienteUrl Nombre de la propiedad del JSON que contiene la url a la siguiente llamada
   * @param filtro Sequencia de nombres de propiedades del JSON a acumular
   */
  case class Paginacion(siguienteUrl:String,filtro:Seq[String])

  Logger.getRootLogger.setLevel(Level.INFO)
  val gson:Gson = new Gson()

  /**
   * Devuelve el resultado de API GET
   * @param url La url de la API REST en formato String
   * @param headers *Opcional* Sequencia key/value de los headers para el Get
   * @return Body de la respuesta como String
   */
  def get(url:String,headers:Seq[(String,String)]=null): String ={
    val httpClient = HttpClientBuilder.create().build()
    val getRequest:HttpGet = new HttpGet(url)
    if(headers!=null){
      headers.foreach(p=>{
        getRequest.addHeader(p._1,p._2)
      })
    }
    try{
      val entity = httpClient.execute(getRequest).getEntity
      EntityUtils.toString(entity)
    }catch{
      case e:ClientProtocolException => System.err.println("La url introducida no es una url",e); null;
      case e:UnknownHostException => System.err.println("La url introducida no a sido encontrada",e);null
      case e:SSLHandshakeException => System.err.println("La API objetivo no tiene un certificado SSL valido",e);null;
    }
    finally{
      getRequest.releaseConnection()
    }
  }

  /**
   * Devuelve el resultado de API POST
   * @param url La url de la API REST en formato String
   * @param headers *Opcional* Sequencia key/value de los headers para el Get
   * @param body *Opcional* Body de la consulta como String
   * @return Body de la respuesta como String
   */
  def post(url:String,headers:Seq[(String,String)]=null,body:String=null): String ={
    val httpClient = HttpClientBuilder.create().build()
    val postRequest:HttpPost = new HttpPost(url)
    if(headers!=null){
      headers.foreach(p=>{
        postRequest.addHeader(p._1,p._2)
      })
    }
    if(body!=null){
      postRequest.setEntity(new StringEntity(body))
    }
    try{
      val entity = httpClient.execute(postRequest).getEntity
      EntityUtils.toString(entity)
    }catch{
      case e:ClientProtocolException => System.err.print("La url introducida no es una url",e); null;
      case e:UnknownHostException => System.err.print("La url introducida no a sido encontrada",e);null
      case e:SSLHandshakeException => System.err.println("La API objetivo no tiene un certificado SSL valido",e);null;
    } finally{
      postRequest.releaseConnection()
    }
  }

  /**
   * Devuelve el resultado de API PUT
   * @param url La url de la API REST en formato String
   * @param headers *Opcional* Sequencia key/value de los headers para el Get
   * @param body *Opcional* Body de la consulta como String
   * @return Body de la respuesta como String
   */
  def put(url:String,headers:Seq[(String,String)]=null,body:String=null):String = {
    val httpClient = HttpClientBuilder.create().build()
    val putRequest:HttpPut = new HttpPut(url)
    if(headers!=null){
      headers.foreach(p=>{
        putRequest.addHeader(p._1,p._2)
      })
    }
    if(body!=null){
      putRequest.setEntity(new StringEntity(body))
    }
    try{
      val entity = httpClient.execute(putRequest).getEntity
      EntityUtils.toString(entity)
    }catch{
      case e:ClientProtocolException => System.err.print("La url introducida no es una url",e); null;
      case e:UnknownHostException => System.err.print("La url introducida no a sido encontrada",e);null
      case e:SSLHandshakeException => System.err.println("La API objetivo no tiene un certificado SSL valido",e);null;
    } finally{
      putRequest.releaseConnection()
    }
  }

  /**
   * Devuelve el resultado de API DELETE
   * @param url La url de la API REST en formato String
   * @param headers *Opcional* Sequencia key/value de los headers para el Get
   * @return Body de la respuesta como String
   */
  def delete(url:String,headers:Seq[(String,String)]=null):String = {
    val httpClient = HttpClientBuilder.create().build()
    val deleteRequest:HttpDelete = new HttpDelete(url)
    if(headers!=null){
      headers.foreach(p=>{
        deleteRequest.addHeader(p._1,p._2)
      })
    }
    try{
      val entity = httpClient.execute(deleteRequest).getEntity
      EntityUtils.toString(entity)
    }catch{
      case e:ClientProtocolException => System.err.print("La url introducida no es una url",e); null;
      case e:UnknownHostException => System.err.print("La url introducida no a sido encontrada",e);null
      case e:SSLHandshakeException => System.err.println("La API objetivo no tiene un certificado SSL valido",e);null;
    } finally{
      deleteRequest.releaseConnection()
    }
  }

  /**
   * Metodo para acumular Jsons utilizando una propiedad con la siguiente llamada a la ApiRest
   * @param url Direccion url de la API Rest
   * @param headers *Opcional* Sequencia key/value de los headers para el Get
   * @param paginacion Clase Paginacion
   * @return Devuleve el Json el formato String con la acumulacion aplicada
   */
  def getPaginacion(url:String,headers:Seq[(String,String)]=null,paginacion:Paginacion):String={
    try{
      val acumulado = acumularJson(url,headers,nextUrlValue = paginacion.siguienteUrl).map(gson.fromJson(_,classOf[JsonObject]))
      val baseJson = gson.fromJson(acumulado.head.deepCopy(),classOf[JsonObject])
      paginacion.filtro.foreach(fil=>{
        baseJson.remove(fil)
        baseJson.add(fil,new JsonArray())
        acumulado.foreach(json=>{
          val add = json.get(fil)
          baseJson.get(fil).getAsJsonArray.add(add)
        })
      })
      baseJson.remove(paginacion.siguienteUrl)
      gson.toJson(baseJson)
    }catch{
      case e:NullPointerException=>null;
    }
  }

  /**
   * Metodo PRIVADO para acumular Jsons en una Sequencia de Strings
   * @param url La url para la consulta Get de Api
   * @param headers *Opcional* Sequencia key/value de los headers para el Get
   * @param acumulador *IMPORTANTE* No introducir manualmente, es para la recursividad
   * @param nextUrlValue El nombre de la propiedad que contiene la url a la siguiente llamada
   * @return Devuelve un Seq[String] que contiene todos los Json acumulados
   */
  private def acumularJson(url:String, headers:Seq[(String,String)]=null, acumulador:Seq[String]=null, nextUrlValue:String):Seq[String] = {
    try{
      val json = gson.fromJson(get(url, headers),classOf[JsonObject])
      val nextUrl = if(json.get(nextUrlValue)==null) "" else json.get(nextUrlValue).getAsString
      if(nextUrl.nonEmpty){
        if(acumulador==null){
          acumularJson(nextUrl,headers,Seq(gson.toJson(json)),nextUrlValue)
        }else{
          acumularJson(nextUrl,headers,acumulador:+gson.toJson(json),nextUrlValue)
        }
      }else{
        acumulador:+gson.toJson(json)
      }
    }catch{
      case e:NullPointerException => System.err.println(s"No se a encontrado ${nextUrlValue}",e);null;
    }
  }
}