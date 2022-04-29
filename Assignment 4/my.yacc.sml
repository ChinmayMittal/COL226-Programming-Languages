functor MyLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : My_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
open DataTypes ; 
val typeTable : (string, Type) HashTable.hash_table =
    HashTable.mkTable (HashString.hashString, op=) (42, Fail "identifier not found") ; 

exception VariableRedeclarationException of string ; 
exception TypeMisMatchException ; 
exception UnDeclaredVariableException ; 

fun insertInTable( idList : string list , idType : Type) = 
        if ( null idList ) then () else ( if isSome (HashTable.find typeTable (hd idList)) then raise VariableRedeclarationException( hd idList) else 
                HashTable.insert typeTable ( hd idList , idType ) ; insertInTable( tl idList , idType) 
                ) ; 

fun checkIntOfID( id: string ) = 
        if( isSome (HashTable.find typeTable  id)) 
        then
            case ( HashTable.lookup typeTable id ) of Int => () | Bool => ( raise TypeMisMatchException) 
                                                      
        else
                raise UnDeclaredVariableException ; 

fun checkBoolOfID( id: string ) = 
        if( isSome (HashTable.find typeTable  id)) 
        then
            case ( HashTable.lookup typeTable id ) of Int => ( raise TypeMisMatchException) | Bool => () 
                                                      
        else
                raise UnDeclaredVariableException ; 

fun getType ( id : string ) = 
         if( isSome (HashTable.find typeTable  id)) 
         then 
                HashTable.lookup typeTable id 
         else 
                raise UnDeclaredVariableException ; 


fun checkBool ( typeToBeChecked : Type ) = 
        if ( typeToBeChecked = Bool ) then true else false ; 

fun checkInt ( typeToBeChecked : Type ) = 
        if ( typeToBeChecked = Int ) then true else false ; 

fun checkSameType ( A : Type , B : Type ) = (A = B ) ; 


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\012\000\000\000\
\\001\000\002\000\025\000\000\000\
\\001\000\003\000\034\000\029\000\033\000\032\000\032\000\033\000\031\000\
\\036\000\030\000\041\000\029\000\042\000\028\000\000\000\
\\001\000\004\000\076\000\018\000\056\000\019\000\055\000\020\000\054\000\
\\021\000\053\000\022\000\052\000\023\000\051\000\024\000\050\000\
\\025\000\049\000\026\000\048\000\027\000\047\000\028\000\046\000\
\\030\000\045\000\031\000\044\000\000\000\
\\001\000\005\000\000\000\000\000\
\\001\000\007\000\080\000\000\000\
\\001\000\008\000\061\000\018\000\056\000\019\000\055\000\020\000\054\000\
\\021\000\053\000\022\000\052\000\023\000\051\000\024\000\050\000\
\\025\000\049\000\026\000\048\000\027\000\047\000\028\000\046\000\
\\030\000\045\000\031\000\044\000\000\000\
\\001\000\009\000\082\000\000\000\
\\001\000\011\000\060\000\018\000\056\000\019\000\055\000\020\000\054\000\
\\021\000\053\000\022\000\052\000\023\000\051\000\024\000\050\000\
\\025\000\049\000\026\000\048\000\027\000\047\000\028\000\046\000\
\\030\000\045\000\031\000\044\000\000\000\
\\001\000\012\000\079\000\000\000\
\\001\000\013\000\022\000\000\000\
\\001\000\014\000\024\000\000\000\
\\001\000\014\000\062\000\000\000\
\\001\000\016\000\040\000\017\000\039\000\000\000\
\\001\000\034\000\003\000\000\000\
\\001\000\037\000\026\000\000\000\
\\001\000\040\000\005\000\000\000\
\\001\000\042\000\004\000\000\000\
\\001\000\042\000\014\000\000\000\
\\001\000\042\000\035\000\000\000\
\\084\000\000\000\
\\085\000\000\000\
\\086\000\035\000\009\000\000\000\
\\087\000\000\000\
\\088\000\000\000\
\\089\000\006\000\021\000\010\000\020\000\038\000\019\000\039\000\018\000\
\\042\000\017\000\000\000\
\\090\000\000\000\
\\091\000\000\000\
\\092\000\015\000\023\000\000\000\
\\093\000\000\000\
\\094\000\000\000\
\\095\000\000\000\
\\096\000\000\000\
\\097\000\018\000\056\000\019\000\055\000\020\000\054\000\021\000\053\000\
\\022\000\052\000\023\000\051\000\024\000\050\000\025\000\049\000\
\\026\000\048\000\027\000\047\000\028\000\046\000\030\000\045\000\
\\031\000\044\000\000\000\
\\098\000\018\000\056\000\019\000\055\000\020\000\054\000\021\000\053\000\
\\022\000\052\000\023\000\051\000\024\000\050\000\025\000\049\000\
\\026\000\048\000\027\000\047\000\028\000\046\000\030\000\045\000\
\\031\000\044\000\000\000\
\\099\000\000\000\
\\100\000\000\000\
\\101\000\024\000\050\000\025\000\049\000\026\000\048\000\027\000\047\000\
\\028\000\046\000\000\000\
\\102\000\024\000\050\000\025\000\049\000\026\000\048\000\027\000\047\000\
\\028\000\046\000\000\000\
\\103\000\018\000\056\000\019\000\055\000\022\000\052\000\023\000\051\000\
\\024\000\050\000\025\000\049\000\026\000\048\000\027\000\047\000\
\\028\000\046\000\000\000\
\\104\000\018\000\056\000\019\000\055\000\022\000\052\000\023\000\051\000\
\\024\000\050\000\025\000\049\000\026\000\048\000\027\000\047\000\
\\028\000\046\000\000\000\
\\105\000\024\000\050\000\025\000\049\000\026\000\048\000\027\000\047\000\
\\028\000\046\000\000\000\
\\106\000\024\000\050\000\025\000\049\000\026\000\048\000\027\000\047\000\
\\028\000\046\000\000\000\
\\107\000\026\000\048\000\027\000\047\000\028\000\046\000\000\000\
\\108\000\026\000\048\000\027\000\047\000\028\000\046\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\000\000\
\\112\000\018\000\056\000\019\000\055\000\020\000\054\000\021\000\053\000\
\\022\000\052\000\023\000\051\000\024\000\050\000\025\000\049\000\
\\026\000\048\000\027\000\047\000\028\000\046\000\000\000\
\\113\000\018\000\056\000\019\000\055\000\020\000\054\000\021\000\053\000\
\\022\000\052\000\023\000\051\000\024\000\050\000\025\000\049\000\
\\026\000\048\000\027\000\047\000\028\000\046\000\030\000\045\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\000\000\
\\118\000\000\000\
\\119\000\000\000\
\\120\000\000\000\
\"
val actionRowNumbers =
"\014\000\017\000\016\000\022\000\
\\022\000\000\000\020\000\018\000\
\\023\000\021\000\025\000\010\000\
\\028\000\011\000\001\000\015\000\
\\002\000\019\000\002\000\002\000\
\\013\000\018\000\025\000\024\000\
\\002\000\033\000\055\000\054\000\
\\002\000\053\000\052\000\002\000\
\\002\000\032\000\008\000\006\000\
\\012\000\031\000\030\000\029\000\
\\026\000\034\000\002\000\002\000\
\\002\000\002\000\002\000\002\000\
\\002\000\002\000\002\000\002\000\
\\002\000\002\000\002\000\051\000\
\\056\000\003\000\000\000\000\000\
\\027\000\049\000\048\000\047\000\
\\046\000\045\000\044\000\043\000\
\\042\000\041\000\039\000\040\000\
\\037\000\038\000\050\000\009\000\
\\005\000\036\000\000\000\007\000\
\\035\000\004\000"
val gotoT =
"\
\\001\000\081\000\000\000\
\\000\000\
\\000\000\
\\002\000\006\000\003\000\005\000\006\000\004\000\000\000\
\\003\000\008\000\006\000\004\000\000\000\
\\004\000\009\000\000\000\
\\000\000\
\\009\000\011\000\000\000\
\\000\000\
\\000\000\
\\005\000\014\000\007\000\013\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\025\000\000\000\
\\000\000\
\\010\000\034\000\000\000\
\\010\000\035\000\000\000\
\\008\000\036\000\000\000\
\\009\000\039\000\000\000\
\\005\000\040\000\007\000\013\000\000\000\
\\000\000\
\\010\000\041\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\055\000\000\000\
\\000\000\
\\000\000\
\\010\000\056\000\000\000\
\\010\000\057\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\061\000\000\000\
\\010\000\062\000\000\000\
\\010\000\063\000\000\000\
\\010\000\064\000\000\000\
\\010\000\065\000\000\000\
\\010\000\066\000\000\000\
\\010\000\067\000\000\000\
\\010\000\068\000\000\000\
\\010\000\069\000\000\000\
\\010\000\070\000\000\000\
\\010\000\071\000\000\000\
\\010\000\072\000\000\000\
\\010\000\073\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\075\000\000\000\
\\004\000\076\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\079\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 82
val numrules = 37
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | IDENTIFIER of unit ->  (string) | POSNUMERAL of unit ->  (string)
 | expression of unit ->  (Type*Exp)
 | varlist of unit ->  (string list) | typedec of unit ->  (Type)
 | command of unit ->  (CMD) | dec of unit ->  (DEC)
 | commands of unit ->  (CMD list) | comseq of unit ->  (CMD list)
 | decseq of unit ->  (DEC list) | blk of unit ->  (BLK)
 | start of unit ->  (AST)
end
type svalue = MlyValue.svalue
type result = AST
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 4) => true | _ => false
val showTerminal =
fn (T 0) => "LBRACE"
  | (T 1) => "RBRACE"
  | (T 2) => "LPAREN"
  | (T 3) => "RPAREN"
  | (T 4) => "EOF"
  | (T 5) => "IF"
  | (T 6) => "ELSE"
  | (T 7) => "THEN"
  | (T 8) => "ENDIF"
  | (T 9) => "WHILE"
  | (T 10) => "DO"
  | (T 11) => "ENDWH"
  | (T 12) => "COLON"
  | (T 13) => "SEMICOLON"
  | (T 14) => "COMMA"
  | (T 15) => "INT"
  | (T 16) => "BOOL"
  | (T 17) => "LT"
  | (T 18) => "LEQ"
  | (T 19) => "NEQ"
  | (T 20) => "EQ"
  | (T 21) => "GT"
  | (T 22) => "GEQ"
  | (T 23) => "PLUS"
  | (T 24) => "MINUS"
  | (T 25) => "TIMES"
  | (T 26) => "DIV"
  | (T 27) => "MOD"
  | (T 28) => "NOT"
  | (T 29) => "AND"
  | (T 30) => "OR"
  | (T 31) => "TRUE"
  | (T 32) => "FALSE"
  | (T 33) => "PROGRAM"
  | (T 34) => "VAR"
  | (T 35) => "NEG"
  | (T 36) => "ASSIGN"
  | (T 37) => "READ"
  | (T 38) => "WRITE"
  | (T 39) => "PROGSTART"
  | (T 40) => "POSNUMERAL"
  | (T 41) => "IDENTIFIER"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33)
 $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26)
 $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19)
 $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12)
 $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ 
(T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.blk blk1, _, blk1right)) :: _ :: ( _, ( 
MlyValue.IDENTIFIER IDENTIFIER1, _, _)) :: ( _, ( _, PROGRAM1left, _))
 :: rest671)) => let val  result = MlyValue.start (fn _ => let val  (
IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (blk as blk1) = blk1 ()
 in ( HashTable.clear typeTable ; PROG( IDENTIFIER , blk ))
end)
 in ( LrTable.NT 0, ( result, PROGRAM1left, blk1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.comseq comseq1, _, comseq1right)) :: ( _, ( 
MlyValue.decseq decseq1, decseq1left, _)) :: rest671)) => let val  
result = MlyValue.blk (fn _ => let val  (decseq as decseq1) = decseq1
 ()
 val  (comseq as comseq1) = comseq1 ()
 in (BLK(decseq , comseq ))
end)
 in ( LrTable.NT 1, ( result, decseq1left, comseq1right), rest671)
end
|  ( 2, ( rest671)) => let val  result = MlyValue.decseq (fn _ => (
([])))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( MlyValue.decseq decseq1, _, decseq1right)) :: ( _, ( 
MlyValue.dec dec1, dec1left, _)) :: rest671)) => let val  result = 
MlyValue.decseq (fn _ => let val  (dec as dec1) = dec1 ()
 val  (decseq as decseq1) = decseq1 ()
 in ( (dec::decseq))
end)
 in ( LrTable.NT 2, ( result, dec1left, decseq1right), rest671)
end
|  ( 4, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.commands 
commands1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.comseq (fn _ => let val  (commands as 
commands1) = commands1 ()
 in ((commands))
end)
 in ( LrTable.NT 3, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 5, ( rest671)) => let val  result = MlyValue.commands (fn _ => (
([])))
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 6, ( ( _, ( MlyValue.commands commands1, _, commands1right)) :: _
 :: ( _, ( MlyValue.command command1, command1left, _)) :: rest671))
 => let val  result = MlyValue.commands (fn _ => let val  (command as 
command1) = command1 ()
 val  (commands as commands1) = commands1 ()
 in ((command::commands))
end)
 in ( LrTable.NT 4, ( result, command1left, commands1right), rest671)

end
|  ( 7, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.typedec 
typedec1, _, _)) :: _ :: ( _, ( MlyValue.varlist varlist1, _, _)) :: (
 _, ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.dec
 (fn _ => let val  (varlist as varlist1) = varlist1 ()
 val  (typedec as typedec1) = typedec1 ()
 in (( insertInTable( varlist , typedec) ; DEC(varlist , typedec)))

end)
 in ( LrTable.NT 5, ( result, VAR1left, SEMICOLON1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, 
IDENTIFIER1right)) :: rest671)) => let val  result = MlyValue.varlist
 (fn _ => let val  (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 in (([IDENTIFIER]))
end)
 in ( LrTable.NT 8, ( result, IDENTIFIER1left, IDENTIFIER1right), 
rest671)
end
|  ( 9, ( ( _, ( MlyValue.varlist varlist1, _, varlist1right)) :: _ ::
 ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, _)) :: 
rest671)) => let val  result = MlyValue.varlist (fn _ => let val  (
IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (varlist as varlist1) = varlist1 ()
 in ((IDENTIFIER::varlist))
end)
 in ( LrTable.NT 8, ( result, IDENTIFIER1left, varlist1right), rest671
)
end
|  ( 10, ( ( _, ( _, INT1left, INT1right)) :: rest671)) => let val  
result = MlyValue.typedec (fn _ => ((Int)))
 in ( LrTable.NT 7, ( result, INT1left, INT1right), rest671)
end
|  ( 11, ( ( _, ( _, BOOL1left, BOOL1right)) :: rest671)) => let val  
result = MlyValue.typedec (fn _ => ((Bool)))
 in ( LrTable.NT 7, ( result, BOOL1left, BOOL1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, _, IDENTIFIER1right
)) :: ( _, ( _, READ1left, _)) :: rest671)) => let val  result = 
MlyValue.command (fn _ => let val  (IDENTIFIER as IDENTIFIER1) = 
IDENTIFIER1 ()
 in ((Read(IDENTIFIER)))
end)
 in ( LrTable.NT 6, ( result, READ1left, IDENTIFIER1right), rest671)

end
|  ( 13, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: ( _, ( _, WRITE1left, _)) :: rest671)) => let val  result = 
MlyValue.command (fn _ => let val  (expression as expression1) = 
expression1 ()
 in ((Write(#2 expression )))
end)
 in ( LrTable.NT 6, ( result, WRITE1left, expression1right), rest671)

end
|  ( 14, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: _ :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, _)
) :: rest671)) => let val  result = MlyValue.command (fn _ => let val 
 (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (expression as expression1) = expression1 ()
 in (
( if (checkSameType( getType IDENTIFIER,  #1 expression)) then () else raise TypeMisMatchException ;  SET(IDENTIFIER ,#2 expression ))
)
end)
 in ( LrTable.NT 6, ( result, IDENTIFIER1left, expression1right), 
rest671)
end
|  ( 15, ( ( _, ( _, _, ENDIF1right)) :: ( _, ( MlyValue.comseq 
comseq2, _, _)) :: _ :: ( _, ( MlyValue.comseq comseq1, _, _)) :: _ ::
 ( _, ( MlyValue.expression expression1, _, _)) :: ( _, ( _, IF1left,
 _)) :: rest671)) => let val  result = MlyValue.command (fn _ => let
 val  (expression as expression1) = expression1 ()
 val  comseq1 = comseq1 ()
 val  comseq2 = comseq2 ()
 in (
 (if( checkBool(#1 expression)) then () else raise TypeMisMatchException ;ITE( #2 expression , comseq1  , comseq2))
)
end)
 in ( LrTable.NT 6, ( result, IF1left, ENDIF1right), rest671)
end
|  ( 16, ( ( _, ( _, _, ENDWH1right)) :: ( _, ( MlyValue.comseq 
comseq1, _, _)) :: _ :: ( _, ( MlyValue.expression expression1, _, _))
 :: ( _, ( _, WHILE1left, _)) :: rest671)) => let val  result = 
MlyValue.command (fn _ => let val  (expression as expression1) = 
expression1 ()
 val  (comseq as comseq1) = comseq1 ()
 in (
 ( if( checkBool( #1 expression)) then () else raise TypeMisMatchException ; WH( #2 expression , comseq ))
)
end)
 in ( LrTable.NT 6, ( result, WHILE1left, ENDWH1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (
 (  if checkSameType( #1 expression1 , #1 expression2) then () else raise TypeMisMatchException ;(Bool , LEQ(#2 expression1 , #2 expression2)))
)
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 18, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (
( if checkSameType( #1 expression1 ,#1 expression2) then () else raise TypeMisMatchException ; (Bool , LT(#2 expression1 , #2 expression2)))
)
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 19, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (
( if checkSameType( #1 expression1 , #1 expression2) then () else raise TypeMisMatchException ; (Bool , EQ(#2 expression1 , #2 expression2)))
)
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 20, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (
( if checkSameType( #1 expression1 ,#1 expression2) then () else raise TypeMisMatchException ; (Bool , NEQ(#2 expression1 , #2 expression2)) )
)
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 21, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (
( if checkSameType( #1 expression1 , #1 expression2) then () else raise TypeMisMatchException ; (Bool , GT(#2 expression1 , #2 expression2)))
)
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 22, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (
( if checkSameType( #1 expression1 ,  #1 expression2) then () else raise TypeMisMatchException ; (Bool , GEQ(#2 expression1 , #2 expression2)))
)
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 23, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (
( if checkInt( #1  expression1) andalso checkInt( #1 expression2) then () else raise TypeMisMatchException ;(Int , PLUS(#2 expression1 , #2 expression2)))
)
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 24, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (
( if checkInt( #1  expression1) andalso checkInt( #1 expression2) then () else raise TypeMisMatchException ;(Int , MINUS(#2 expression1 , #2 expression2)))
)
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 25, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (
(if checkInt( #1  expression1) andalso checkInt( #1 expression2) then () else raise TypeMisMatchException ; (Int , TIMES(#2 expression1 , #2 expression2)))
)
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 26, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (
( if checkInt( #1  expression1) andalso checkInt( #1 expression2) then () else raise TypeMisMatchException ;(Int , DIV(#2 expression1 , #2 expression2)))
)
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 27, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (
( if checkInt( #1  expression1) andalso checkInt( #1 expression2) then () else raise TypeMisMatchException ;(Int , MOD(#2 expression1 , #2 expression2)))
)
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 28, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (
( if checkBool(#1 expression1) andalso checkBool(#1 expression2) then () else raise TypeMisMatchException ;(Bool , AND(#2 expression1 , #2 expression2))  )
)
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 29, ( ( _, ( MlyValue.expression expression2, _, expression2right
)) :: _ :: ( _, ( MlyValue.expression expression1, expression1left, _)
) :: rest671)) => let val  result = MlyValue.expression (fn _ => let
 val  expression1 = expression1 ()
 val  expression2 = expression2 ()
 in (
(if checkBool(#1 expression1) andalso checkBool(#1 expression2) then () else raise TypeMisMatchException ; (Bool , OR(#2 expression1 , #2 expression2)) )
)
end)
 in ( LrTable.NT 9, ( result, expression1left, expression2right), 
rest671)
end
|  ( 30, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expression 
expression1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.expression (fn _ => let val  (expression as 
expression1) = expression1 ()
 in (( expression))
end)
 in ( LrTable.NT 9, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: ( _, ( _, NEG1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (expression as expression1) = 
expression1 ()
 in (
(if checkInt(#1 expression) then () else raise TypeMisMatchException ;(Int , NEG( #2 expression)))
)
end)
 in ( LrTable.NT 9, ( result, NEG1left, expression1right), rest671)

end
|  ( 32, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.expression (fn _ => ((Bool , TT )))
 in ( LrTable.NT 9, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 33, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.expression (fn _ => ((Bool , FF )))
 in ( LrTable.NT 9, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.POSNUMERAL POSNUMERAL1, POSNUMERAL1left, 
POSNUMERAL1right)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (POSNUMERAL as POSNUMERAL1) = 
POSNUMERAL1 ()
 in (( Int , Posnumeral(POSNUMERAL) ))
end)
 in ( LrTable.NT 9, ( result, POSNUMERAL1left, POSNUMERAL1right), 
rest671)
end
|  ( 35, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, 
IDENTIFIER1right)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (IDENTIFIER as IDENTIFIER1) = 
IDENTIFIER1 ()
 in (( getType IDENTIFIER , Identifier(IDENTIFIER) ))
end)
 in ( LrTable.NT 9, ( result, IDENTIFIER1left, IDENTIFIER1right), 
rest671)
end
|  ( 36, ( ( _, ( MlyValue.expression expression1, _, expression1right
)) :: ( _, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.expression (fn _ => let val  (expression as expression1) = 
expression1 ()
 in (
 if checkBool(#1 expression) then () else raise TypeMisMatchException ; (Bool , NOT(#2 expression)) 
)
end)
 in ( LrTable.NT 9, ( result, NOT1left, expression1right), rest671)

end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : My_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDIF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun ENDWH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun PROGRAM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun WRITE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun PROGSTART (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun POSNUMERAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.POSNUMERAL (fn () => i),p1,p2))
fun IDENTIFIER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.IDENTIFIER (fn () => i),p1,p2))
end
end
