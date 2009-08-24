package biz.jackman.brewmaster.brewutils


import org.scalatest.Suite

/**
 * Copyright Ben Jackman 2009
 * please contact ben@jackman.biz for licensing questions.
 * User: bjackman
 * Date: Aug 24, 2009
 * Time: 12:50:22 PM
 * 
 */

trait MavenSuite extends Suite {
  def testWithMaven() = {
    execute()
  }
}