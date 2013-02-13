package eg

import eg.Quote._
import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object QuoteCheck extends Properties("Quote") {
  property("Unquote") = forAll((s: String) => unquote(quote(s)) == s)
}


