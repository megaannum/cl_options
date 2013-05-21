
package com.megaannum.cl_options

import scala.collection.mutable.ListBuffer
import scala.util.parsing.input._
import scala.util.parsing.combinator._
import scala.{ Option => SOption}
import scala.tools.cmd.FromString
import scala.util.control.Breaks.{break, breakable}

/**
 *
 Grammar 
   options := ( options )*
   option  := prefix nameOps
   prefix  := ( '-' or '--' )
   nameOps := ( (name (key '=' value)* ) or ( name '=' value) or (name value))
   name    := string
   value   := string

  // Unary -f or --flag
  // Binary -v b, -v=b, --value b or --value=b
  // Property -Dkey=value
  // KeyValues -p key0=value0 key1=value1 ...
 */
object CmdLine {
  class DefinitionParser extends RegexParsers {

    override def skipWhitespace = false

    def names: Parser[List[Option.Name]] = opt(opt_namesOp) ^^ {
      case optionsOp => optionsOp match {
        case Some(optionsList) => optionsList
        case None => Nil
      }
    }
    def opt_namesOp: Parser[List[Option.Name]] = opt_name~rep(spaces~>opt_name) ^^ {
      case name~namelist => 
        var names: List[Option.Name] = Nil
        names = name :: names
        for (name <- namelist) names = name :: names
        names.reverse
    }

    def opt_name: Parser[Option.Name] = prefix~name ^^ { 
      case prefix~name => Option.Name(prefix, name)
    }

    def prefix: Parser[String] = ( "--" | "-" ) ^^ { s => s }
    def name: Parser[String] = """\w+""".r ^^ { s => s }
    def spaces: Parser[String] = whiteSpace ^^ { s => s }
  }

  class LineParser(options: Options) extends RegexParsers {
    val debug = true
    def doDebug(msg: String): Unit = {
      if (debug) {
        print("Parser: ")
        println(msg)
      }
    }

    override def skipWhitespace = false

    def SPACE = ' '
    def EQUALS = '='

    def load: Parser[Boolean] = opt(optionsOp) ^^ {
      case optionsOp => optionsOp match {
        case Some(value) => value
        case None => false
      }
  /*
      case optionsOp => optionsOp match {
        case Some(optionsList) => optionsList
        case None => Nil
      }
  */
    }
    def optionsOp: Parser[Boolean] = option~rep(spaces~>option) ^^ {
      case op~oplist =>
        oplist.foldLeft(op){(v,b) => v&&b}
  /*
        if (! op) return false
        else for (op <- oplist) if (! op) return false
        true
  */
  /*
      case op~oplist => 
        var optionList: List[Option.Value] = Nil
        optionList = op :: optionList
        for (op <- oplist) optionList = op :: optionList
        optionList.reverse
  */
    }
    def option: Parser[Boolean] = (binaryOrProperty | unaryOrKeyValue ) ^^ {
      case opval => opval
    }

    // Unary -f or --flag
    // KeyValues -p key0=value0 key1=value1 ...
    def unaryOrKeyValue: Parser[Boolean] = prefix~name~opt(keyvaluelistOp) ^^ { 
      case prefix~name~keyvaluelistOp => keyvaluelistOp match {
        case Some(keyvaluelist) => 
          options.bindKeyValue(Option.Name(prefix, name), keyvaluelist)
        case None => Option.Value(prefix, name)
          options.bindUnary(Option.Name(prefix, name))
      }
    }

    // Binary -v b, -v=b, --value b or --value=b
    // Property -Dkey=value
    def binaryOrProperty: Parser[Boolean] = prefix~name~(EQUALS~>value | spaces~>value) ^^ { 
      // case prefix~name~value => Option.Value(prefix, name, value)
      case prefix~name~value => 
        options.bindBinaryOrProperty(Option.Name(prefix, name), value)
    }

    def keyvaluelistOp: Parser[List[Tuple2[String,String]]] = keyvalue~rep(spaces~>keyvalue) ^^ {
      case kv~kvlist => 
        var keyvalueList: List[Tuple2[String,String]] = Nil
        keyvalueList = kv :: keyvalueList
        for (kv <- kvlist) keyvalueList = kv :: keyvalueList
        keyvalueList.reverse
    }
    def keyvalue: Parser[Tuple2[String,String]] = name~(EQUALS~>value) ^^ { 
      case name~value => Tuple2(name, value)
    }
    

  /*
    def options: Parser[List[Option.Value]] = option~rep(SPACE~>option) ^^ {
      case op~oplist => 
        var optionList: List[Option.Value] = Nil
        optionList = op :: optionList
        for (op <- oplist) optionList = op :: optionList
        optionList.reverse
    }

    def option: Parser[Option.Value] = ( "--" | "-" )~name~opt(valueOp) ^^ {
        case prefix~name~valueOp => valueOp match {
          case Some(value) => Option.Value(prefix, name, value)
          case None => Option.Value(prefix, name)
        }
         
      }

    def valueOp: Parser[String] = ( EQUALS~>value | SPACE~>value ) ^^ {
        case value => value
      }
  */

    def prefix: Parser[String] = ( "--" | "-" ) ^^ { s => s }

    def name: Parser[String] = """\w+""".r ^^ { 
      s => 
      doDebug("name="+s)
      s 
    }

    def value: Parser[String] = """\w+""".r ^^ { 
      s => 
      doDebug("value="+s)
      s 
    }

    // def spacesOp: Parser[String] = opt(spaces) ^^ { s => }

    def spaces: Parser[String] = whiteSpace ^^ { s => s }
  }

  object Option {

    object Value {
      def apply(prefix: String, name: String) = 
        new Value(new Name(prefix, name))
      def apply(prefix: String, name: String, value: String) = 
        new Value(new Name(prefix, name), value)
    }
    class Value(val name: Name, val valueOp: SOption[String]) {
      def this(name: Name, value: String) = this(name, Some(value))
      def this(name: Name) = this(name, None)

      override def toString: String = name+valueOp.getOrElse("")
    }

    object Name {
      def apply(s: String): Name = {
        if (s.startsWith("--")) new Name("--", s.substring(2))
        else if (s.startsWith("-")) new Name("-", s.substring(1))
        else throw new Exception("Bad Option String: " + s)
      }
      def apply(prefix: String, tag: String): Name = new Name(prefix, tag)
    }
    class Name(val prefix: String, val tag: String) {
      override def equals(obj: Any): Boolean = obj match {
        case other: Name => other.prefix == this.prefix && other.tag == this.tag
        case _ => false
      }
      override def toString: String = prefix+tag
    }

    object Pair {
      def apply(key: String, value: String) = new Pair(key, Some(value))
      def apply(key: String) = new Pair(key, None)
    }
    class Pair(key: String, valueOp: SOption[String]) {
    }

    // name(:type )?(=value)?
    case class ValueDef(val name: String, 
                        val tnameOp: SOption[String],
                        val valueOp: SOption[String]) {
    }

    // Option.Def
    abstract class Def {
      var parentOptionsOp: SOption[Options] = None
      var childOptionDefs: List[Option.Def] = Nil

      def provider: Options.Provider
      def name: Name
      def aliases: List[Name]
      def allNames: List[Name] = name :: aliases
      def help: String
      def detailHelp: List[String]

      def doMatch(other: Name): Boolean = 
        other == name || aliases.exists{ a => a == other }

      def ++=(optDefs: List[Option.Def]): Unit = 
        childOptionDefs = optDefs ::: childOptionDefs 

      def +=(optDef: Option.Def): Unit = 
        childOptionDefs = optDef :: childOptionDefs 


      // def apply(value: Value): Unit 
      def error(msg: String): Unit = parentOptionsOp match {
        case Some(options) => options.error(this, msg)
        case None => 
          val e = "No Options object for Option.Def " + 
                  allNames.mkString("") +
                  " with error message: " +msg
          throw new Exception(e)
      }
    }

    // Option.Unary
    // -f or --flag
    object Unary {
      def apply(provider: Options.Provider,
                name: Name, 
                aliases: List[Name], 
                help: String,
                detailHelp: List[String],
                flag: Boolean) = new Unary(provider,  
                                           name, 
                                            aliases, 
                                            help,
                                            detailHelp,
                                            flag)

      // helper
      def apply(provider: Options.Provider,
                name: Name, 
                help: String,
                flag: Boolean) = new Unary(provider,  
                                           name, 
                                           Nil, 
                                           help,
                                           Nil,
                                           flag)
      def apply(provider: Options.Provider,
                name: Name, 
                help: String,
                detailHelp: List[String],
                flag: Boolean) = new Unary(provider,  
                                           name, 
                                           Nil, 
                                           help,
                                           detailHelp,
                                           flag)

      def apply(provider: Options.Provider,
                str: String, 
                help: String,
                flag: Boolean): Unary = Unary(provider,  
                                              str, 
                                              help,
                                              Nil,
                                              flag)

      def apply(provider: Options.Provider,
                str: String, 
                help: String,
                detailHelp: List[String],
                flag: Boolean): Unary = {
        val pp = new Unary.Parser
        val r = pp.parseAll(pp.names, str)
        val names: List[Name] = r.get
        // This ignores if the opt has a value
        new Unary(provider,  
                  names.head, 
                  names.tail, 
                  help,
                  detailHelp,
                  flag)
      }
      // Unary -f or --flag
      class Parser extends RegexParsers {

        override def skipWhitespace = false

        def names: Parser[List[Option.Name]] = opt(opt_namesOp) ^^ {
          case optionsOp => optionsOp match {
            case Some(optionsList) => optionsList
            case None => Nil
          }
        }
        def opt_namesOp: Parser[List[Option.Name]] = opt_name~rep(spaces~>opt_name) ^^ {
          case name~namelist => 
            var names: List[Option.Name] = Nil
            names = name :: names
            for (name <- namelist) names = name :: names
            names.reverse
        }

        def opt_name: Parser[Option.Name] = prefix~name ^^ { 
          case prefix~name => Option.Name(prefix, name)
        }

        def prefix: Parser[String] = ( "--" | "-" ) ^^ { s => s }
        def name: Parser[String] = """\w+""".r ^^ { s => s }
        def spaces: Parser[String] = whiteSpace ^^ { s => s }
      }
    }
    class Unary(val provider: Options.Provider,
                val name: Name, 
                val aliases: List[Name], 
                val help: String,
                val detailHelp: List[String],
                var flag: Boolean) extends Def {
    }

    // Option.Binary
    // -v b, -v=b, --value b or --value=b
    object Binary {
      def apply(provider: Options.Provider,
                name: Name, 
                aliases: List[Name], 
                valDefOp: SOption[ValueDef],
                help: String,
                detailHelp: List[String]): Binary = 
        new Binary(provider,  
                   name, 
                   aliases, 
                   valDefOp,
                   help,
                   detailHelp)

      // helper
      def apply(provider: Options.Provider,
                name: Name, 
                aliases: List[Name], 
                valDef: ValueDef,
                help: String): Binary = 
        Binary(provider,  
               name, 
               aliases, 
               Some(valDef),
               help,
               Nil)

      def apply(provider: Options.Provider,
                name: Name, 
                aliases: List[Name], 
                valDef: ValueDef,
                help: String,
                detailHelp: List[String]): Binary = 
        Binary(provider,  
               name, 
               aliases, 
               Some(valDef),
               help,
               detailHelp)

      // helper
      def apply(provider: Options.Provider,
                name: Name, 
                aliases: List[Name], 
                help: String): Binary = 
        new Binary(provider,  
                   name, 
                   aliases, 
                   None,
                   help,
                   Nil)

      def apply(provider: Options.Provider,
                name: Name, 
                aliases: List[Name], 
                help: String,
                detailHelp: List[String]): Binary = 
        new Binary(provider,  
                   name, 
                   aliases, 
                   None,
                   help,
                   detailHelp)

      def apply(provider: Options.Provider,
                name: Name, 
                valDefOp: SOption[ValueDef],
                help: String): Binary = 
        new Binary(provider,  
                   name, 
                   Nil, 
                   valDefOp,
                   help,
                   Nil)

      def apply(provider: Options.Provider,
                name: Name, 
                valDefOp: SOption[ValueDef],
                help: String,
                detailHelp: List[String]): Binary = 
        new Binary(provider,  
                   name, 
                   Nil, 
                   valDefOp,
                   help,
                   detailHelp)

      // helper
      def apply(provider: Options.Provider,
                name: Name, 
                help: String): Binary = 
        Binary(provider,  
               name, 
               Nil, 
               None,
               help,
               Nil)

      def apply(provider: Options.Provider,
                name: Name, 
                help: String,
                detailHelp: List[String]): Binary = 
        Binary(provider,  
               name, 
               Nil, 
               None,
               help,
               detailHelp)

      // helper
      def apply(provider: Options.Provider,
                name: Name, 
                valDef: ValueDef,
                help: String): Binary = 
        new Binary(provider,  
                   name, 
                   Nil, 
                   Some(valDef),
                   help,
                   Nil)

      def apply(provider: Options.Provider,
                name: Name, 
                valDef: ValueDef,
                help: String,
                detailHelp: List[String]): Binary = 
        new Binary(provider,  
                   name, 
                   Nil, 
                   Some(valDef),
                   help,
                   detailHelp)

      def apply(provider: Options.Provider,
                str: String, 
                help: String): Binary =  Binary(provider,  
                                                str, 
                                                help,
                                                Nil)

      def apply(provider: Options.Provider,
                str: String, 
                help: String,
                detailHelp: List[String]): Binary =  {
        val pp = new Binary.Parser
        val r = pp.parseAll(pp.namesVDefOp, str)
        val (names: List[Name], valDefOp) = r.get
valDefOp match {
case Some(ValueDef(name, tnameOp, valueOp)) => 
println("valdef="+name+":"+tnameOp+"="+valueOp)
case None => println("valdef=NONE")
}
        // This ignores if the opt has a value
        Binary(provider,  
               names.head, 
               names.tail, 
               valDefOp,
               help,
               detailHelp)
      }

      // Binary -v b, -v=b, --value b or --value=b
      //  -v --value <value_name(:Type)?(=DefaultValue)>
      class Parser extends RegexParsers {

        override def skipWhitespace = false
        def COLON = ':'
        def EQUALS = '='
        def LEFT_CHEVRON = '<'
        def RIGHT_CHEVRON = '>'


        def namesVDefOp: Parser[Tuple2[List[Option.Name],SOption[ValueDef]]] = opt(opt_namesOp~opt(spaces~>valueDef)) ^^ {
          case tmpOp => tmpOp match {
            case Some(optionsList~valDefOp) => valDefOp match {
              case Some(valueDef) => Tuple2(optionsList, Some(valueDef))
              case None =>           Tuple2(optionsList, None)
            }
            case None => Tuple2(Nil, None)
          }
        }
        def opt_namesOp: Parser[List[Option.Name]] = opt_name~rep(spaces~>opt_name) ^^ {
          case name~namelist => 
            var names: List[Option.Name] = Nil
            names = name :: names
            for (name <- namelist) names = name :: names
            names.reverse
        }

        def opt_name: Parser[Option.Name] = prefix~name ^^ { 
          case prefix~name => Option.Name(prefix, name)
        }
        def valueDef: Parser[ValueDef] = LEFT_CHEVRON~>(words~opt(COLON~>words)~opt(EQUALS~>words))<~RIGHT_CHEVRON ^^ { 
          case name~tnameOp~valueOp => tnameOp match {
            case Some(tname) => valueOp match {
              case Some(value) => ValueDef(name, Some(tname), Some(value))
              case None => ValueDef(name, Some(tname), None)
            }
            case None => valueOp match {
              case Some(value) => ValueDef(name, None, Some(value))
              case None => ValueDef(name, None, None)
            }
          }
        }

        def words: Parser[String] = name~rep(whiteSpace~>name) ^^ {
          case word~wordlist => 
            var words = word
            for (w <- wordlist) words += " " +w
            words
        }

        def prefix: Parser[String] = ( "--" | "-" ) ^^ { s => s }
        def name: Parser[String] = """\w+""".r ^^ { s => s }
        def spaces: Parser[String] = whiteSpace ^^ { s => s }
      }
    }
    class Binary(val provider: Options.Provider,
                 val name: Name, 
                 val aliases: List[Name], 
                 val valDefOp: SOption[ValueDef],
                 val help: String,
                 val detailHelp: List[String]) extends Def {

      def valueName: String = valDefOp match {
        case Some(valDef) => 
          valDef.name + 
            ":" + (valDef.tnameOp getOrElse "String") +
            "=" + (valDef.valueOp getOrElse "None")
        case None => name.tag
      }
    }


    // Option.Property
    // arguments (-Dkey=value)
    object Property {
      def apply(provider: Options.Provider,
                name: Name, 
                valDefOp: SOption[ValueDef],
                help: String,
                detailHelp: List[String]): Property =
        new Property(provider,  
                     name, 
                     valDefOp,
                     help,
                     detailHelp)

      def apply(provider: Options.Provider,
                name: Name, 
                valDefOp: SOption[ValueDef],
                help: String) : Property = Property(provider,
                                                    name,
                                                    valDefOp,
                                                    help,
                                                    Nil)

  /*
      // helper
      def apply(provider: Options.Provider,
                name: Name, 
                help: String): Property = 
        Property(provider,  
                 name, 
                 help,
                 None)

      // helper
      def apply(provider: Options.Provider,
                name: Name, 
                help: String,
                keyValuePair: T): Property = 
        new Property(provider,  
                     name, 
                     help,
                     Some(keyValuePair))
  */


      def apply(provider: Options.Provider,
                str: String, 
                help: String): Property = Property(provider,
                                                   str,
                                                   help,
                                                   Nil)

      def apply(provider: Options.Provider,
                str: String, 
                help: String,
                detailHelp: List[String]): Property = {
        val pp = new Property.Parser
        val r = pp.parseAll(pp.nameVDefOp, str)
        val (name: Name, valDefOp) = r.get
        // This ignores if the opt has a value
        Property(provider,  
                 name, 
                 valDefOp,
                 help)
      }
      // Property -Dkey=value
      // Property -F<value_name(:Type)?(=DefaultValue)>
      class Parser extends RegexParsers {

        override def skipWhitespace = false
        def COLON = ':'
        def EQUALS = '='
        def LEFT_CHEVRON = '<'
        def RIGHT_CHEVRON = '>'


        def nameVDefOp: Parser[Tuple2[Option.Name,SOption[ValueDef]]] = opt(opt_name~opt(valueDef)) ^^ {
          case tmpOp => tmpOp match {
            case Some(optname~valDefOp) => valDefOp match {
              case Some(valueDef) => Tuple2(optname, Some(valueDef))
              case None =>           Tuple2(optname, None)
            }
            case None => Tuple2(Option.Name("NONE", "NONE"), None)
          }
        }
  /*
        def opt_namesOp: Parser[List[Option.Name]] = opt_name~rep(spaces~>opt_name) ^^ {
          case name~namelist => 
            var names: List[Option.Name] = Nil
            names = name :: names
            for (name <- namelist) names = name :: names
            names.reverse
        }
  */

  // XXXXXXXXXXXXXXXXXXXX
        def opt_name: Parser[Option.Name] = prefix~name ^^ { 
          case prefix~name => Option.Name(prefix, name)
        }
        def valueDef: Parser[ValueDef] = LEFT_CHEVRON~>(words~opt(COLON~>words)~opt(EQUALS~>words))<~RIGHT_CHEVRON ^^ { 
          case name~tnameOp~valueOp => tnameOp match {
            case Some(tname) => valueOp match {
              case Some(value) => ValueDef(name, Some(tname), Some(value))
              case None => ValueDef(name, Some(tname), None)
            }
            case None => valueOp match {
              case Some(value) => ValueDef(name, None, Some(value))
              case None => ValueDef(name, None, None)
            }
          }
        }

        def words: Parser[String] = name~rep(whiteSpace~>name) ^^ {
          case word~wordlist => 
            var words = word
            for (w <- wordlist) words += " " +w
            words
        }

        def prefix: Parser[String] = ( "--" | "-" ) ^^ { s => s }
        def name: Parser[String] = """\w+""".r ^^ { s => s }
        def spaces: Parser[String] = whiteSpace ^^ { s => s }
      }
    }
    class Property(val provider: Options.Provider,
                   val name: Name, 
                   val valDefOp: SOption[ValueDef],
                   val help: String,
                   val detailHelp: List[String]) extends Def {
      // def this(ref: Ref) = this(ref, None)
      // def this(ref: Ref, value: T) = this(ref, Some(value))
      
      def aliases: List[Name] = Nil
      def valueName: String = name.tag

      override def doMatch(other: Name): Boolean = other == name 
    }

    // Option.KeyValues
    // -p key0=value0 key1=value1 ...
    object KeyValues {
    }
    class KeyValues(val provider: Options.Provider,
              val name: Name, 
              val aliases: List[Name], 
              val help: String,
              val detailHelp: List[String],
              var keyValuePairs: List[Pair]) extends Def {
      // def this(ref: Ref) = this(ref, Nil)
    }

  }

  import Option._




  object Help {
    def apply(): Help = new Help

    abstract class Err {
      def execute(): String
    }
    class StrErr(msg: String) extends Err {
      def execute(): String = msg
    }
    class OptDefErr(optDef: Option.Def, msg: String) extends Err {
      def execute(): String = optDef.name +": " +msg
    }

    abstract class Gen(fmt: String) {
      def execute(): String
    }
    class StrGen(str: String, fmt: String) extends Gen(fmt) {
      def execute(): String = fmt.format(str)
    }
    class HeaderGen(text: String, fmt: String) extends Gen(fmt) {
      def execute(): String = fmt.format(text)
    }
    // Called for unknown Option.Def type
    class DefGen(optDef: Option.Def, fmt: String) extends Gen(fmt) {
      def execute(): String = {
        val help = optDef.help
        val name = optDef.name
        fmt.format(name, help)
      }
    }
    class UnaryGen(unary: Unary, fmt: String) extends Gen(fmt) {
      def execute(): String = {
        val names = unary.allNames.foldLeft(""){(v,n) => 
                      val s = if (v.length == 0) v else v + " | "
                      s + n.prefix + n.tag
                   }
        fmt.format(names, unary.help) + 
          unary.detailHelp.foldLeft("") { (v, dh) =>
            if (v.length == 0) "TAB" + dh else v + "\n  " + "TAB" + dh
          }
      }
    }
    class BinaryGen(binary: Binary, fmt: String) extends Gen(fmt) {
      def execute(): String = {
        val opts = binary.allNames.foldLeft(""){(v,n) => 
                      val s = if (v.length == 0) v else v + ", "
                      s + n.prefix + n.tag
                   }
        val name = binary.name
        val vname = binary.valueName
        val help = binary.help
        fmt.format(opts, vname, help) + 
          binary.detailHelp.foldLeft("") { (v, dh) =>
            if (v.length == 0) "TAB" + dh else v + "\n  " + "TAB" + dh
          } +
          (binary.childOptionDefs.map{childOD =>
            (childOD match {
              case u: Unary => new UnaryGen(u, defaultUniaryFormat)
              case b: Binary => new BinaryGen(b, defaultBinaryFormat)
              case p: Property => new PropertyGen(p, defaultBinaryFormat)
              case _ => new DefGen(childOD, defaultDefFormat)
          }) execute() }).foldLeft("  ") { (v,s) =>
            if (v.length == 0) s else v + "\n  " + s
          } 
      }
    }
    class PropertyGen(prop: Property, fmt: String) extends Gen(fmt) {
      def execute(): String = {
        val opts = prop.allNames.foldLeft(""){(v,n) => 
                      val s = if (v.length == 0) v else v + ", "
                      s + n.prefix + n.tag
                   }
        val name = prop.name
        val vname = prop.valueName
        val help = prop.help
        fmt.format(opts, vname, help) + 
          prop.detailHelp.foldLeft("") { (v, dh) =>
            if (v.length == 0) "TAB" + dh else v + "\n  " + "TAB" + dh
          } +
          (prop.childOptionDefs.map{childOD =>
            (childOD match {
              case u: Unary => new UnaryGen(u, defaultUniaryFormat)
              case b: Binary => new BinaryGen(b, defaultBinaryFormat)
              case p: Property => new PropertyGen(p, defaultBinaryFormat)
              case _ => new DefGen(childOD, defaultDefFormat)
          }) execute() }).foldLeft("  ") { (v,s) =>
            if (v.length == 0) s else v + "\n  " + s
          } 
      }
    }
    class KeyValuesGen(kvs: KeyValues, fmt: String) extends Gen(fmt) {
      def execute(): String = {
        val opts = kvs.allNames.foldLeft(""){(v,n) => 
                      val s = if (v.length == 0) v else v + ", "
                      s + n.prefix + n.tag
                   }
        val name = kvs.name
        val help = kvs.help
        fmt.format(opts, name, help) + 
          kvs.detailHelp.foldLeft("") { (v, dh) =>
            if (v.length == 0) "TAB" + dh else v + "\n  " + "TAB" + dh
          } +
          (kvs.childOptionDefs.map{childOD =>
            (childOD match {
              case u: Unary => new UnaryGen(u, defaultUniaryFormat)
              case b: Binary => new BinaryGen(b, defaultBinaryFormat)
              case p: Property => new PropertyGen(p, defaultBinaryFormat)
              case _ => new DefGen(childOD, defaultDefFormat)
          }) execute() }).foldLeft("  ") { (v,s) =>
            if (v.length == 0) s else v + "\n  " + s
          } 
      }
    }

    val defaultStrFormat = "%s"
    val defaultHeaderFormat = "\n%s"
    val defaultDefFormat = "  %s :%s"
    val defaultUniaryFormat = "  %s :%s"
    val defaultBinaryFormat = "  %s <%s> :%s"
    val defaultPropertyFormat = "  %s<%s> :%s"
    val defaultKeyValuesFormat = "  %s <%s> :%s"
  }
  class Help extends Options.Provider {
    import Help._

    protected var _help = new ListBuffer[Help.Gen]
    protected var _errors = new ListBuffer[Help.Err]
    protected var dohelp = false

    def error(optDef: Option.Def, msg: String): Unit = 
      _errors += new Help.OptDefErr(optDef, msg)
    def error(msg: String): Unit = 
      _errors += new Help.StrErr(msg)

    override def toString: String = 
      _help.map { gen => gen.execute() } mkString("\n")

    def handleErrors: Unit = if (! _errors.isEmpty) {
      println("Errors: ")
      for (err <- _errors) println(err.execute())
      println
      println(this)
      exit
    }

    def addHelp(gen: Help.Gen): Unit = _help += gen
    def help(str: String): Unit = 
      addHelp(new Help.StrGen(str, defaultStrFormat))
    def +(str: String): Unit = help(str)
    def heading(str: String): Unit = 
      addHelp(new HeaderGen(str, defaultHeaderFormat))
    def +(optDef: Option.Def): Unit = optDef match {
      case u: Unary => addHelp(new UnaryGen(u, defaultUniaryFormat))
      case b: Binary => addHelp(new BinaryGen(b, defaultBinaryFormat))
      case p: Property => addHelp(new PropertyGen(p, defaultPropertyFormat))
      case kv: KeyValues => addHelp(new KeyValuesGen(kv, defaultKeyValuesFormat))
      case _ => addDef(optDef)
    }
    protected def addDef(optDef: Option.Def): Unit = 
      addHelp(new DefGen(optDef, defaultDefFormat))

    def optDefs: List[Option.Def] = {
      List(Unary(this,
                 "--help -h", 
                 "Show help message", 
                 false))
    }
    def bindOption(optDef: Option.Def, valueOp: SOption[String]): Boolean = {
  println("Help.bindOption: TOP")
      // should not have a value
      valueOp match {
        case Some(str) => optDef.error("Help does not take parameter"); false
        case None => dohelp = true; true
      }
    }
    def resolveOption(optDef: Option.Def): Unit = {
  println("Help.resolveOption: TOP")
      handleErrors
      if (dohelp) println(this); exit
    }
  }


  object Options {
    trait Provider {
      /** The Provider adds its Option's to the Options object.
       */
      def optDefs: List[Option.Def]

      /** The Provider sets its local state based upon the option and
       * value. All Provider's bind methods are called for each of its
       * options that appeared on the command line prior to calling
       * and resolve methods.
       */
      def bindOption(optDef: Option.Def, valueOp: SOption[String]): Boolean

      /** The Provider determines if its state has been set correctly.
       */
      def resolveOption(optDef: Option.Def): Unit 
    }

    def apply(helplineOp: SOption[String],
              detailHelp: List[String]): Options = new Options(helplineOp, 
                                                               detailHelp)

    def apply(helpline: String,
              detailHelp: List[String]): Options = Options(Some(helpline), 
                                                           detailHelp)

    def apply(helpline: String): Options = Options(Some(helpline), Nil)

    def apply(): Options = Options(None, Nil)

  }

  class Options(helplineOp: SOption[String], detailHelp: List[String]) {

    val help = Help()

    this + help
    helplineOp foreach { helpline => help + helpline }
    // TODO detail help text
    detailHelp foreach { helpline => help + helpline }


    // XXXXXXX

    protected var _unary = List[Unary]()
    protected var _binary = List[Binary]()
    protected var _property = List[Property]()

    lazy val unary = _unary.distinct
    lazy val binary = _binary.distinct
    lazy val all = unary ++ binary

    def error(optDef: Option.Def, msg: String): Unit = help.error(optDef, msg)

    def +(provider: Options.Provider): Unit = this + provider.optDefs
    def +(optDefs: List[Option.Def]): Unit = 
      for (optDef <- optDefs) optDef match {
        case u: Unary => addUnary(u)
        case b: Binary => addBinary(b)
        case p: Property => addProperty(p)
        case _ => 
          val e = "Unknown Option.Def type: " + optDef.getClass.getName
          throw new Exception(e)
      }

    def addUnary(unary: Unary): Unit = {
      unary.parentOptionsOp = Some(this)
      _unary +:= unary
      help + unary
    }
    def addBinary(binary: Binary): Unit = {
      binary.parentOptionsOp = Some(this)
      _binary +:= binary
      help + binary
    }
    def addProperty(property: Property): Unit = {
      property.parentOptionsOp = Some(this)
      _property +:= property
      help + property
    }


    def parse(args: Array[String]): Boolean = {
  println("Options.parser: TOP")
      val pp = new LineParser(this)
      val r = pp.parseAll(pp.load, args.mkString(" "))
  println("Options.parser: BOTTOM")
      r.get
    }

    def bindUnary(name: Name): Boolean = {
  println("Options.bindUnary: name=" + name)
      // try unary options
      for (unaryOptDef <- unary;
           optname <- unaryOptDef.allNames;
           if optname == name) {
  println("Options.bindUnary: name="+name)
          if (unaryOptDef.provider.bindOption(unaryOptDef, None))
            return true
      }
      // try unary option child options (if any)
      for (unaryOptDef <- unary;
           childOptDef <- unaryOptDef.childOptionDefs;
           optname <- childOptDef.allNames;
           if optname == name) {
  println("Options.bindUnary: name="+name)
          if (childOptDef.provider.bindOption(childOptDef, None))
            return true
      }
      // TODO report bind error
      help.error("Failed to find binding for Unary Option: " +name) 
      return false
    }

    def bindBinaryOrProperty(name: Name, value: String): Boolean = {
  println("Options.bindBinaryOrProperty: name=" + name)
      // try binary options
      for (binaryOptDef <- binary;
           optname <- binaryOptDef.allNames;
           if optname == name) {
  println("Options.bindBinaryOrProperty: name="+name)
          if (binaryOptDef.provider.bindOption(binaryOptDef, Some(value)))
            return true
      }
      // try binary option child options (if any)
      for (binaryOptDef <- binary;
           childOptDef <- binaryOptDef.childOptionDefs;
           optname <- childOptDef.allNames;
           if optname == name) {
  println("Options.bindBinaryOrProperty: name="+name)
          if (childOptDef.provider.bindOption(childOptDef, Some(value)))
            return true
      }
      // TODO
      // try property options
      // try property option child options (if any)

      // TODO report bind error
      help.error("Failed to find binding for Binary/Property Option: " +name) 
      return false
    }

    // TODO
    def bindKeyValue(name: Name, keyvaluelist: List[Tuple2[String,String]]): Boolean = {
      // TODO report bind error
      help.error("Failed to find binding for KeyValue Option: " +name) 
      return false
    }

    def resolve: Unit = {
      for (optDef <- all) optDef.provider.resolveOption(optDef)
      for (optDef <- all; childOptDef <- optDef.childOptionDefs)
        childOptDef.provider.resolveOption(childOptDef)
    }
    def errors: Unit = help.handleErrors
  }








    def main(args: Array[String]): Unit = {
println("main: TOP")
      import Options._

      class Debug extends Provider {
        private var level = 0
        private var filename = "OUT.debug"

        def isEnabled(level: Int): Boolean = level >= this.level
        def print(msg: String): Unit = Console.print(msg)
        def print(level: Int, msg: String): Unit = 
          if (isEnabled(level)) print(msg)

        def optDefs: List[Option.Def] = {
          List(Binary(this, 
                      "--debug_level --debug -d <level:Int=0>", 
                      "Set debugging level"),
               Property(this, 
                        "-F", 
                        "Set file name")
              )
        }
        def bindOption(optDef: Option.Def, valueOp: SOption[String]): Boolean = {
  println("Debug.bindOption: TOP")
          optDef match {
            case b: Binary => valueOp match {
              case Some(str) => 
  println("Debug.bindOption: binary str="+str)
                // TODO non-negative values only
                level = FromString.IntFromString(str); true
              case None => optDef.error("Debug requires level value"); false
            }
            case p: Property => p.name.tag match {
              case "D" => true
              case _ => optDef.error("Debug unknown Property Option"); false
            }
          }
        }
        def resolveOption(optDef: Option.Def): Unit = {
  println("Debug.resolveOption: TOP")
        }
      }

      object Lang {
        abstract class Kind(val name: String) extends Provider

        class C extends Kind("C") {
          var functionNameOp: SOption[String] = None

          def optDefs: List[Option.Def] = {
            List(Binary(this, 
                        "--function_name --func_name -fn <Function Name>", 
                        "Name of function"))
          }
          def bindOption(optDef: Option.Def, 
                         valueOp: SOption[String]): Boolean = {
  println("Lang.C.bindOption: TOP")
            valueOp match {
              case Some(str) => 
  println("Lang.C.bindOption: str="+str)
                functionNameOp = Some(str)
                true
              case None => 
                optDef.error("Lang C function_name requires value")
                false
            }
          }
          def resolveOption(optDef: Option.Def): Unit = {
  println("Lang.C.resolveOption: TOP")
            if (functionNameOp isEmpty) optDef.error("Language C function name not set.")
          }
        }
        class Scala extends Kind("Scala") {
          var functionNameOp: SOption[String] = None
          var objectNameOp: SOption[String] = None

          def optDefs: List[Option.Def] = {
            List(Binary(this, 
                     "--function_name --func_name -fn <Function Name>",
                     "Name of function"),
                 Binary(this, 
                     "--object_name --obj_name -on <Object Name>", 
                     "Name of object"))
          }
          def bindOption(optDef: Option.Def, 
                         valueOp: SOption[String]): Boolean = {
  println("Lang.Scala.bindOption: TOP")
            optDef.name.tag match {
              case "function_name" => valueOp match {
                case Some(str) => 
  println("Lang.Scala.bindOption: str="+str)
                  functionNameOp = Some(str)
                  true
                case None => 
                  optDef.error("Lang Scala function_name requires value")
                  false
              }
              case "object_name" => valueOp match {
                case Some(str) => 
  println("Lang.Scala.bindOption: str="+str)
                  objectNameOp = Some(str)
                  true
                case None => 
                  optDef.error("Lang Scala object_name requires value")
                  false
              }
              case _ =>
                optDef.error("Lang Scala unknown option " + optDef.name)
                false
            }
          }
          def resolveOption(optDef: Option.Def): Unit = {
  println("Lang.Scala.resolveOption: TOP")
            if (functionNameOp isEmpty) optDef.error("Language C function name not set.")
            if (objectNameOp isEmpty) optDef.error("Language C object name not set.")
          }
        }
      }
      class Lang extends Provider {
        private var kindOp: Option[Lang.Kind] = None

        def optDefs: List[Option.Def] = {
          List(Binary(this, 
                      "--language --lang -l <Programming Language=Scala>", 
                      "Which langage to support: C or Scala")
          )
        }
        def bindOption(optDef: Option.Def, valueOp: SOption[String]): Boolean = {
  println("Lang.bindOption: TOP")
            valueOp match {
              case Some(str) => 
  println("Lang.bindOption: str="+str)
                str match  {
                  case "C" => 
                    val l = new Lang.C
                    optDef ++= l.optDefs
                    this.kindOp = Some(new Lang.C)
                    true
                  case "Scala" => 
                    val l = new Lang.Scala
                    optDef ++= l.optDefs
                    this.kindOp = Some(new Lang.C)
                    true
                  case _ =>
                    optDef.error("Unknown language name: " + str)
                    false
                }
              case None => optDef.error("Lang requires name"); false
          }
        }
        def resolveOption(optDef: Option.Def): Unit = {
  println("Lang.resolveOption: TOP")
          this.kindOp match {
            case Some(kind) => // Ok
            case None => // Error
              optDef.error("Language kind not set.")
          }
        }
      }

      val debug = new Debug
      val lang = new Lang

/*
      val opts = Options()
      opts.help + "Usage: Options option*"
      opts + opts.help
*/
      val opts = Options("Usage: CmdLine options*")
      opts + debug
      opts + lang

  /*
      val optionValues = opts.parse(args)
  println("main: optionValues=" + optionValues.mkString(" "))
      opts.bind(optionValues)
      opts.errors
      opts.resolve(optionValues)
      opts.errors
  */
      if (opts.parse(args)) {
        println("Success")
        opts.resolve
        opts.errors
      } else opts.errors

  println("main: BOTTOM")
    }
}
