package com.sc4nam.module

import java.nio.file.{Files, Paths, Path}
import io.github.memo33.metarules.meta.{RotFlip, Rule, EquivRule, IdTile}
import RotFlip._
import syntax.IdTile
import SanityChecker.{isRulFile, fileEndsWithNewline, Driveside, Rhd, Lhd, RhdAndLhd, parseRule, linePatternIncludingNewlines}

/** Run with `SBT_OPTS="-Xmx2G" sbt "runMain com.sc4nam.module.RedundantAdjacenciesChecker"`.
  * Note that this increases the heap size for more memory.
  * Takes about 4 minutes.
  */
object RedundantAdjacenciesChecker {

  /** Scans the Controller/RUL2/ folder for adjancy RUL2 code that is
    * redundant with the DLL RUL2 engine.
    *
    * The respective lines are commented out and tagged as "; redundant-adjacency".
    *
    * Make sure all your changes to files are committed to git beforehand,
    * as this modifies files in place.
    *
    * Afterwards, you need to manually look through the changes and remove the
    * redundant code.
    */
  def main(args: Array[String]): Unit = {
    checkRedundantAdjacencies()
  }

  def drivesideOfFile(path: Path): Driveside = {
    val name = path.getFileName().toString()
    if (name.contains("rhd.")) Rhd
    else if (name.contains("lhd.")) Lhd
    else RhdAndLhd
  }

  val lhdPrefix = ";###LHD###"
  val rhdPrefix = ";###RHD###"

  def parseRuleWithRestrictedDriveside(line: String, driveside: Driveside): Option[(Rule[IdTile], Driveside)] = {
    val line1 = line.trim()
    if (line1.startsWith(lhdPrefix)) {
      if (driveside == Rhd) None else parseRule(line1.substring(lhdPrefix.length)).map(_ -> Lhd)
    } else if (line1.startsWith(rhdPrefix)) {
      if (driveside == Lhd) None else parseRule(line1.substring(rhdPrefix.length)).map(_ -> Rhd)
    } else {
      parseRule(line1).map(_ -> driveside)
    }
  }

  // def foreachLineIncludingNewlines(in: java.io.BufferedReader)(f: String => Unit): Unit = {
  //   val buf = Array.empty[Char](1024 * 16)
  //   var count = 0
  //   while (count != -1) {
  //     count = in.read(buf, 0, buf.length)
  //     var idx = 0
  //     while (idx < buf.length) {
  //       if (
  //       idx += 1
  //     }
  //   }
  // }

  def checkRedundantAdjacencies(): Unit = {
    val rulesRhd = collection.mutable.Map.empty[EquivRule, Rule[IdTile]]
    val rulesLhd = collection.mutable.Map.empty[EquivRule, Rule[IdTile]]
    val rulesShared = collection.mutable.Map.empty[EquivRule, Rule[IdTile]]
    val lookupRuleRhd: PartialFunction[EquivRule, Rule[IdTile]] = rulesShared.orElse(rulesRhd)  // the two maps should be disjoint
    val lookupRuleLhd: PartialFunction[EquivRule, Rule[IdTile]] = rulesShared.orElse(rulesRhd)  // the two maps should be disjoint

    LOGGER.info("caching all RUL2 code for RHD and LHD")
    Files.walk(Paths.get("Controller/RUL2")).forEach { path =>
      if (isRulFile(path)) {
        val drivesideFile = drivesideOfFile(path)
        scala.util.Using.resource(new java.util.Scanner(path.toFile(), "UTF-8")) { scanner =>
          while(scanner.hasNextLine()) {
            parseRuleWithRestrictedDriveside(scanner.nextLine(), drivesideFile) match {
              case Some((rule, Rhd)) => rulesRhd.addOne(new EquivRule(rule), rule)
              case Some((rule, Lhd)) => rulesLhd.addOne(new EquivRule(rule), rule)
              case Some((rule, RhdAndLhd)) => rulesShared.addOne(new EquivRule(rule), rule)
              case None => // ignore
            }
          }
        }
      }
    }

    LOGGER.info("searching for redundant adjacencies in RUL2 code")
    Files.walk(Paths.get("Controller/RUL2")).forEach { path =>
      if (isRulFile(path)) {
        val drivesideFile = drivesideOfFile(path)
        val tmpPath = path.resolveSibling(path.getFileName().toString() + ".tmp")
        val endsWithNewline = fileEndsWithNewline(path)  // attempt to preserve missing newlines at end of files to avoid noise
        scala.util.Using.resources(
          new java.util.Scanner(path.toFile(), "UTF-8").useDelimiter(linePatternIncludingNewlines),
          new java.io.PrintWriter(tmpPath.toFile(), "UTF-8")
        ) { (lineScanner, printer) =>
          while (lineScanner.hasNext()) {
            val line = lineScanner.next()

            val redundant =
              parseRuleWithRestrictedDriveside(line, drivesideFile) match {
                case Some((rule, Rhd)) => isRedundantAdjacency(rule, lookupRuleRhd)
                case Some((rule, Lhd)) => isRedundantAdjacency(rule, lookupRuleLhd)
                case Some((rule, RhdAndLhd)) =>
                  val b = isRedundantAdjacency(rule, lookupRuleLhd)
                  if (b != isRedundantAdjacency(rule, lookupRuleLhd)) {
                    throw new AssertionError("Redundancies should be the same for RHD and LHD.")  // hopefully this is always be the case
                  }
                  b
                case None => false  // comments are not redundant
              }

            if (redundant) {
              printer.println(s";${line.stripLineEnd}; redundant-adjacency")  // comments out the line
            } else {
              printer.print(line)  // preserving original linebreaks
            }
          }
        }
        Files.move(tmpPath, path, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
      }
    }
  }

  val surrogateTiles = Seq(
    IdTile(0x00004B00, R1F0), IdTile(0x00004B00, R3F0),  // Road
    IdTile(0x57000000, R1F0), IdTile(0x57000000, R3F0),  // Dirtroad
    IdTile(0x05004B00, R1F0), IdTile(0x05004B00, R3F0),  // Street
    IdTile(0x5D540000, R1F0), IdTile(0x5D540000, R3F0),  // Rail
    IdTile(0x08031500, R1F0), IdTile(0x08031500, R3F0),  // Lightrail
    IdTile(0x09004B00, R1F0), IdTile(0x09004B00, R3F0),  // Onewayroad  (TODO or 0x5f940300?)
    IdTile(0x04006100, R3F0), IdTile(0x04006100, R1F0),  // Avenue
    IdTile(0x0D031500, R1F0), IdTile(0x0D031500, R3F0)  // Monorail
  )

  def evaluateRule(rule: Rule[IdTile], t0: IdTile, t1: IdTile): Option[(IdTile, IdTile)] = {
    if (t0 == rule(0) && t1 == rule(1)) Some((rule(2), rule(3)))
    else if (t0 == rule(0) * R2F1 && t1 == rule(1) * R2F1) Some((rule(2) * R2F1, rule(3) * R2F1))
    else if (t0 == rule(1) * R0F1 && t1 == rule(0) * R0F1) Some((rule(3) * R0F1, rule(2) * R0F1))
    else if (t0 == rule(1) * R2F0 && t1 == rule(0) * R2F0) Some((rule(3) * R2F0, rule(2) * R2F0))
    else None
  }

  def evaluateRulesOnce(lookupRule: PartialFunction[EquivRule, Rule[IdTile]], t0: IdTile, t1: IdTile): Option[(IdTile, IdTile)] = {
    val key = new EquivRule(Rule(t0, t1, t0, t1))
    lookupRule.unapply(key).flatMap(rule => evaluateRule(rule, t0, t1))
  }

  /** Checks if adjacency overrides x -> y and y -> z exist (in that order and
    * direction) (where y is surrogate tile) and matches expected result
    * xExpected, zExpected.
    */
  def connectingOverridesExist(lookupRule: PartialFunction[EquivRule, Rule[IdTile]], x: IdTile, y: IdTile, z: IdTile, xExpected: IdTile, zExpected: IdTile): Boolean = {
    evaluateRulesOnce(lookupRule, x, y) match {
      case Some((x1, y1)) if x1 == x && x1.id != y1.id =>
        evaluateRulesOnce(lookupRule, y1, z) match {
          case Some((y2, z2)) if y2 == y1 && z2 != z && y2.id != z2.id =>
            // found two overrides connecting x to z, so check if result of their application is as expected
            x1 == xExpected && z2 == zExpected
          case _ => false
        }
      case _ => false
    }
  }

  def isRedundantAdjacency(rule: Rule[IdTile], lookupRule: PartialFunction[EquivRule, Rule[IdTile]]): Boolean = {
    val x = rule(0)
    val z = rule(1)
    surrogateTiles.exists { y =>
      if (y.id == x.id || y.id == z.id) {
        false
      } else {
        (connectingOverridesExist(lookupRule, x, y, z, rule(2), rule(3))  // x -> y -> z
          || connectingOverridesExist(lookupRule, z * R2F0, y * R2F0, x * R2F0, rule(3) * R2F0, rule(2) * R2F0))  // z -> y -> x
      }
    }
  }

}
