package s1.telegrambots.examples

import okio.Utf8
import s1.telegrambots.BasicBot
import scalaj.http._

import scala.collection.mutable.Buffer
import scala.io.Codec
import scala.io.Source.fromURL
import scala.util.control.Exception
import scala.util.matching.Regex


object Muistava_YT_HakuBotti extends App {

  val bot = new BasicBot() {

    var edellinen : String = ""
    var edellinenKanava : String = ""
    val haut = Buffer[String]()
    val kanavaHaut = Buffer[String]()

    def viimeHaku(msg: Message) = {
      val osoite = "https://www.youtube.com/results?search_query=" + this.edellinen.replace(" ", ",")

      val response: HttpResponse[String] = Http(osoite).asString
      val sisalto = response.body

      val regex = new Regex("watch\\?v=(\\S{11})")

      val haettava = """watch\\?v=(\\S{11})""".r
      val osuma : Option[String] = regex.findFirstIn(sisalto)

      val apu = osuma.getOrElse("Ei löytynyt")

      if(apu.equals("Ei löytynyt")) {
        "Ei löytynyt"
      } else {
        "https://www.youtube.com/" + apu
      }


    }

    def kerroViimeHaku(msg: Message) = {

      if(this.edellinen.equals("")) {
        "Moi " + getUserFirstName(msg) + ". Mitään ei ole vielä haettu (onnistuneesti)" + "."
      } else {
        "Moi " + getUserFirstName(msg) + ". Edellinen (onnistunut) hakusihan oli " + this.edellinen
      }

    }

    def viimeKanava(msg: Message) = {
        val osoite = "https://www.youtube.com/c/" + this.edellinenKanava.replace(" ", "")
        var onnistui = true

        try {
          val url = scala.io.Source.fromURL(osoite).mkString
       } catch {
          case ex: java.io.FileNotFoundException => onnistui = false
        }

        val osoite2 = "https://www.youtube.com/user/" + this.edellinenKanava.replace(" ", "")
        var onnistui2 = true

        try {
          val url = scala.io.Source.fromURL(osoite2).mkString
        } catch {
          case ex: java.io.FileNotFoundException => onnistui2 = false
        }

        if(onnistui) {
          "https://www.youtube.com/c/" + this.edellinenKanava.replace(" ", "").toLowerCase()
        } else if(onnistui2) {
          "https://www.youtube.com/user/" + this.edellinenKanava.replace(" ", "").toLowerCase()
        } else {
          "Kanavaa ei löytynyt"
        }
    }

    def kerroViimeKanava(msg: Message)  = {

      if(this.edellinenKanava.equals("")) {
        "Moi " + getUserFirstName(msg) + ". Kanavia ei ole vielä haettu (onnistuneesti)" + "."
      } else {
        "Moi " + getUserFirstName(msg) + ". Edellinen (onnistuneesti) hakemasi kanavahan oli " + this.edellinenKanava + "."
      }

    }

/*
    hae-komennolla haetaan youtubesta haetulla hakusanalla sisältöä, minkä jälkeen sivuston html-koodi
    dekoodataan ja sieltä haetaan ensimmäinen hakusanalla vastaantullut youtube-video. Komento palauttaa
    kyseisen linkin
*/

    def hae(msg: Message) = {

      val haettu = getString(msg) : String

      val osoite = "https://www.youtube.com/results?search_query=" + haettu.replace(" ", ",")

      val response: HttpResponse[String] = Http(osoite).asString
      val sisalto = response.body

      val regex = new Regex("watch\\?v=(\\S{11})")

      val osuma : Option[String] = regex.findFirstIn(sisalto)

      val apu = osuma.getOrElse("Ei löytynyt")

      val hakuLinkki = "https://www.youtube.com/" + apu

      if(apu.equals("Ei löytynyt")) {
        "Ei löytynyt"
      } else {
        this.edellinen = haettu
        this.haut += haettu.replace(" ", ",").toLowerCase()
        hakuLinkki
      }

    }
    //haeKanava ei toimi yhtä sulavasti kuin hae-komento. Kanavan nimi on syötettävä melko täsmällisesti,
    //jotta komento toimii oikein. Komento ei myöskään toimi, mikäli YouTube-kanavan omistaja
    //ei ole korvannut kanavansa URL-osoitteen monimutkaista ID:tä kanavan nimellä.
    //Toimii kuitenkin lähes kaikilla vähänkin isommilla kanavilla.

    def haeKanava(msg: Message) = {

      val haettu = getString(msg)

      val osoite = "https://www.youtube.com/c/" + haettu.replace(" ", "")
      var onnistui = true

      try {
        val url = scala.io.Source.fromURL(osoite).mkString
      } catch {
        case ex: java.io.FileNotFoundException => onnistui = false
      }

      val osoite2 = "https://www.youtube.com/user/" + haettu.replace(" ", "")
      var onnistui2 = true

      try {
        val url = scala.io.Source.fromURL(osoite2).mkString
      } catch {
        case ex: java.io.FileNotFoundException => onnistui2 = false
      }

      if(onnistui) {
        this.edellinenKanava = haettu
        this.kanavaHaut += haettu.replace(" ", "").toLowerCase()
        "https://www.youtube.com/c/" + haettu.replace(" ", "").toLowerCase()
      } else if(onnistui2) {
        this.edellinenKanava = haettu
        this.kanavaHaut += haettu.replace(" ", "").toLowerCase()
        "https://www.youtube.com/user/" + haettu.replace(" ", "").toLowerCase()
      } else {
        "Kanavaa ei löytynyt"
      }

    }




    def suosikkiHaku(msg: Message) = {

      if(this.haut.isEmpty) {
        "Yhtään hakua ei ole vielä tehty (onnistuneesti)."
      } else {
        val enitenHaettu = haut.groupBy(identity).maxBy(_._2.size)._1

        val osoite = "https://www.youtube.com/results?search_query=" + enitenHaettu.replace(" ", ",")

        val response: HttpResponse[String] = Http(osoite).asString
        val sisalto = response.body

        val regex = new Regex("watch\\?v=(\\S{11})")

        val osuma : Option[String] = regex.findFirstIn(sisalto)

        val apu = osuma.getOrElse("Ei löytynyt")

        if(apu.equals("Ei löytynyt")) {
          "Ei löytynyt"
        } else {
          this.edellinen = enitenHaettu
          "https://www.youtube.com/" + apu
        }
      }


    }

    def kerroSuosikki(msg: Message) = {
      if (this.haut.isEmpty) {
        "Moi " + getUserFirstName(msg) + ". Et ole hakenut vielä mitään (onnistuneesti)."
      } else {
        val enitenHaettu = this.haut.groupBy(identity).maxBy(_._2.size)._1
        "Moi " + getUserFirstName(msg) + ". Suosikkihakusihan on " + enitenHaettu + "."
      }
    }


    def suosikkiKanavaHaku(msg: Message) = {

      if(this.kanavaHaut.isEmpty) {
        "Yhtään kanavaa ei ole vielä haettu (onnistuneesti)."
      } else {
        val enitenHaettu = this.kanavaHaut.groupBy(identity).maxBy(_._2.size)._1

      val osoite = "https://www.youtube.com/c/" + enitenHaettu.replace(" ", "")
      var onnistui = true

      try {
        val url = scala.io.Source.fromURL(osoite).mkString
      } catch {
        case ex: java.io.FileNotFoundException => onnistui = false
      }

      val osoite2 = "https://www.youtube.com/user/" + enitenHaettu.replace(" ", "")
      var onnistui2 = true

      try {
        val url = scala.io.Source.fromURL(osoite2).mkString
      } catch {
        case ex: java.io.FileNotFoundException => onnistui2 = false
      }

      if(onnistui) {
        "https://www.youtube.com/c/" + enitenHaettu.replace(" ", "")
      } else if(onnistui2) {
        "https://www.youtube.com/user/" + enitenHaettu.replace(" ", "")
      } else {
        "Kanavaa ei löytynyt"
      }
      }
    }

    def kerroSuosikkiKanava(msg: Message) = {

      if (this.kanavaHaut.isEmpty) {
        "Moi " + getUserFirstName(msg) + ". Et ole hakenut vielä mitään (onnistuneesti)."
      } else {
        val enitenHaettu = this.kanavaHaut.groupBy(identity).maxBy(_._2.size)._1
        "Moi " + getUserFirstName(msg) + ". Eniten hakemasi kanavahan on " + enitenHaettu + "."
      }

    }

    def help(msg: Message) = {

      """
      Voin auttaa sinua hakemaan videoita ja kanavia YouTube:sta. Minua voi hallita komennoilla:

      HAE VIDEOTA
      /hae - hakee videota YouTubesta hakusanalla
      /viimeHaku - tekee YouTube-haun käyttäen edellistä hakusanaa
      /kerroViimeHaku - kertoo edellisen hakusanan
      /suosikkiHaku - tekee haun eniten käyttämälläsi hakusanalla
      /kerroSuosikki - kertoo eniten käyttämäsi hakusanan

      HAE KANAVAA
      /haeKanava - hakee kanavan YouTubesta hakusanalla
      /viimeKanava - tekee edellisen onnistuneen kanavahaun
      /kerroViimeKanava - kertoo edellisen onnistuneen kanavahaun
      /suosikkiKanavaHaku - hakee eniten hakemasi kanavan
      /kerroSuosikkiKanava - kertoo eniten hakemasi kanavan
      """
    }

    this.command("kerroViimeHaku", kerroViimeHaku)
    this.command("viimeHaku", viimeHaku)
    this.command("hae", hae)
    this.command("haeKanava", haeKanava)
    this.command("viimeKanava", viimeKanava)
    this.command("kerroViimeKanava", kerroViimeKanava)
    this.command("help", help)
    this.command("suosikkiHaku", suosikkiHaku)
    this.command("kerroSuosikki", kerroSuosikki)
    this.command("suosikkiKanavaHaku", suosikkiKanavaHaku)
    this.command("kerroSuosikkiKanava", kerroSuosikkiKanava)

    this.run()

    println("Started")
  }

}
