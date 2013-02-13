package eg

/** 
 * Functions for escaping and un-escaping strings. The format is fairly standard and should work
 * for Java, Javascript, and similar languages. Yes, there is a fuzz test for this.
 */
object Quote {

  def quote(s: String) = s.map {
    case '"' => "\\\""
    case '\\' => "\\\\"
    case '/' => "\\/"
    case '\b' => "\\b"
    case '\f' => "\\f"
    case '\n' => "\\n"
    case '\r' => "\\r"
    case '\t' => "\\t"
    case c if ((c >= '\u0000' && c <= '\u001f') || (c >= '\u007f' && c <= '\u009f')) => "\\u%04x".format(c: Int)
    case c => c
  }.mkString("\"", "", "\"")

  def unquote(s:String):String =
    if (s.isEmpty) s else s(0) match {
      case '"' => unquote(s.tail)
      case '\\' => s(1) match {
        case 'b' => '\b' + unquote(s.drop(2))
        case 'f' => '\f' + unquote(s.drop(2))
        case 'n' => '\n' + unquote(s.drop(2))
        case 'r' => '\r' + unquote(s.drop(2))
        case 't' => '\t' + unquote(s.drop(2))
        case '"' => '"'  + unquote(s.drop(2))
        case 'u' => Integer.parseInt(s.drop(2).take(4), 16).toChar + unquote(s.drop(6))
        case c => c + unquote(s.drop(2))
      }
      case c => c + unquote(s.tail)
    }
  
}