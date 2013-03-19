package eg

//import scalaz._
//import Scalaz._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import eg._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Shrink
import org.scalacheck.Shrink.shrink

object JExprCheck extends Properties("JExpr") {

  implicit def arbJExpr: Arbitrary[JExpr] = Arbitrary {

    val arbJNull = Gen.value(JNull)
    val arbJBool = arbitrary[Boolean].map(JBool)
    val arbJNum = arbitrary[Double].map(JNum)
    val arbJStr = arbitrary[String].map(JStr)

    val arbMap = for {
      n <- Gen.choose(0, 1)
      l <- Gen.listOfN(n, for {
        k <- arbitrary[String].suchThat(_.length > 1)
        v <- arbitrary[JExpr]
      } yield (k, v))
    } yield JMap(l.toMap)

    val arbVec = for {
      n <- Gen.choose(0, 1)
      l <- Gen.listOfN(n, arbitrary[JExpr])
    } yield JVec(l)

    Gen.frequency(
      (1, arbJNull),
      (2, arbJBool),
      (3, arbJNum),
      (3, arbJStr),
      (5, arbMap),
      (5, arbVec))

  }

  implicit val shrinkJExpr: Shrink[JExpr] = Shrink {
    case JNull => Stream.Empty
    case JBool(_) => Stream.Empty
    case JNum(n) => shrink(n).map(JNum)
    case JStr(s) => shrink(s).map(JStr)
    case JVec(es) => shrink(es).map(JVec)
    case JMap(m) => shrink(m).map(JMap)
  }

  property("Parsing") = forAll { (e: JExpr) => JExpr.parse(e.toString) == Right(e) }

}