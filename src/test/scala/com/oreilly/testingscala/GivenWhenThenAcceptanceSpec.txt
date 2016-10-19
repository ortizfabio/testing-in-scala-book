package com.oreilly.testingscala

import org.specs2.Specification
import org.specs2.specification.script.{StepParser, StandardDelimitedStepParsers}
import org.specs2.specification.{Then, When, Given}



import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
@RunWith(classOf[JUnitRunner])
class GivenWhenThenAcceptanceSpec extends Specification with StandardDelimitedStepParsers
{
  def is = s2"""  Given the first name {David} and the last {Bowie} create an Artist
                When we add the artist to an album called {Hunky Dory} with the year {1971}
                and And when an the album is added to a jukebox
                |Then the jukebox should have one album whose name is {Hunky Dory}
                first and last name $e1 then add an album and verify that the name is
        Hunky Dory
      """
  val fname:String = "" //StepParser((_: String))
  val lname:String = "" //StepParser((_: String))
  val artist =setUpBowie.extract("${David Bowie}")
  val album = setUpHunkyDory.extract(artist,"{Hunky Dory} {1971}")
  val jukeBox = addTheAlbumToAJukebox
  object setUpBowie extends Given[Artist] {
    def extract(text: String) = {


      new Artist(fname,lname)
    }
  }

  object setUpHunkyDory extends When[Artist, Album] {
    def extract(p: Artist, text: String) = {
      val tokens = extract2(text)
      new Album(tokens._1, tokens._2.toInt, p)
    }
  }

  object addTheAlbumToAJukebox extends When[Album, JukeBox] {
    def extract(p: Album, text: String) = new JukeBox(Some(List(p)))
  }

  object result extends Then[JukeBox] {
    def extract(t: JukeBox, text: String) = t.albums.get must have size (1)
  }
   def e1 = {
       artist.firstName must beEqualTo("David") and(
         artist.lastName must beEqualTo("Bowie")
         )
   }

  def e2 = {
    album.title must beEqualTo("Hunky Dory")

  }

}