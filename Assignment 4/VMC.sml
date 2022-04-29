(* prints string to the out stream i.e text file *)
fun printToOutStream outstream str = let val os = outstream
                                     in
                                       TextIO.output(os,str)

                                     end;

(* outstream created from text file for printing output of the program *)
val os = TextIO.openOut "output.txt";

(* get one line of input from terminal *)
fun getInput( message : string ) =
   ( print ( message ^ "\n") ; 
    let
	val str= valOf(TextIO.inputLine TextIO.stdIn)
    in
	String.substring ( str , 0 ,  (String.size str) -1) 
    end)

fun initializeArray( size : int ) =
    Array.array( size, 0 )

fun getVariables( declaration ) = 
    let 
        val DataTypes.DEC( variables, typeOfVariables ) = declaration
    in 
        (variables,typeOfVariables)
    end 
    
fun addTypes( variables, typeOfVariables , tt ) = 
    if ( null variables ) then () 
    else
      (  HashTable.insert tt ( hd variables , typeOfVariables ) ; addTypes( tl variables , typeOfVariables , tt) ) 

fun initializeIndices( variables, mt, idx ) = 
    if ( null variables ) then () 
    else
      (  HashTable.insert mt ( hd variables , idx ) ; initializeIndices( tl variables , mt , idx + 1 ) )

fun initializeTables( declarations , tt , mt , idx ) = 
    if null declarations then 
        idx
    else 
        let 
            val temp = getVariables ( hd declarations )
            val numberOfVariables = length ( #1 temp )  
        in 
            addTypes( #1 temp, #2 temp , tt ) ; 
            initializeIndices( #1 temp, mt, idx ) ; 
            initializeTables( tl declarations , tt , mt , idx + numberOfVariables )
        end  
(* ------------------------------------------------------------------*)

(* converts AST datatype to stack datatype *)
fun processCommands( commands : DataTypes.CMD list ) = 
if null commands then 
    [] 
else
    let 
        val command = hd commands 
        val tempAns = processCommands( tl commands )
    in 
        case command of DataTypes.SET( x , y ) => StackDataTypes.SETCMD( x , y ) :: tempAns 
                        | DataTypes.Read(x) => StackDataTypes.READCMD(x) ::  tempAns
                        | DataTypes.Write(x) => StackDataTypes.WRITECMD(x) :: tempAns
                        | DataTypes.ITE(x,y,z) => StackDataTypes.ITECMD(x,y,z) :: tempAns 
                        | DataTypes.WH(x,z) => StackDataTypes.WHILECMD(x,z) :: tempAns 
    end 

fun createControlStack( commands , stack ) = 
if null commands then 
    stack 
else 
    createControlStack( tl commands , FunStack.push( ( hd commands), stack ))

fun expressionToStackNodes ( expression ) = 
case expression of DataTypes.LEQ(x,y) => List.concat [  expressionToStackNodes(x), expressionToStackNodes(y), [ StackDataTypes.OPERATOR("leq")] ]
                  | DataTypes.LT(x,y) => List.concat [  expressionToStackNodes(x), expressionToStackNodes(y), [ StackDataTypes.OPERATOR("lt")] ]
                  | DataTypes.EQ(x,y) => List.concat [  expressionToStackNodes(x), expressionToStackNodes(y), [ StackDataTypes.OPERATOR("eq")] ]
                  | DataTypes.NEQ(x,y) => List.concat [  expressionToStackNodes(x), expressionToStackNodes(y), [ StackDataTypes.OPERATOR("neq")] ]
                  | DataTypes.GT(x,y) => List.concat [  expressionToStackNodes(x), expressionToStackNodes(y), [ StackDataTypes.OPERATOR("gt")] ]
                  | DataTypes.GEQ(x,y) => List.concat [  expressionToStackNodes(x), expressionToStackNodes(y), [ StackDataTypes.OPERATOR("geq")] ]
                  
                  | DataTypes.PLUS(x,y) => List.concat [  expressionToStackNodes(x), expressionToStackNodes(y), [ StackDataTypes.OPERATOR("plus")] ]
                  | DataTypes.MINUS(x,y) => List.concat [  expressionToStackNodes(x), expressionToStackNodes(y), [ StackDataTypes.OPERATOR("minus")] ]
                  | DataTypes.TIMES(x,y) => List.concat [  expressionToStackNodes(x), expressionToStackNodes(y), [ StackDataTypes.OPERATOR("times")] ]
                  | DataTypes.DIV(x,y) => List.concat [  expressionToStackNodes(x), expressionToStackNodes(y), [ StackDataTypes.OPERATOR("div")] ]
                  | DataTypes.MOD(x,y) => List.concat [  expressionToStackNodes(x), expressionToStackNodes(y), [ StackDataTypes.OPERATOR("mod")] ]

                  | DataTypes.AND(x,y) => List.concat [  expressionToStackNodes(x), expressionToStackNodes(y), [ StackDataTypes.OPERATOR("and")] ]
                  | DataTypes.OR(x,y) => List.concat [  expressionToStackNodes(x), expressionToStackNodes(y), [ StackDataTypes.OPERATOR("or")] ]

                  | DataTypes.NEG(x) => List.concat [  expressionToStackNodes(x) ,  [ StackDataTypes.OPERATOR("neg")] ]
                  | DataTypes.NOT(x) => List.concat [  expressionToStackNodes(x)  , [ StackDataTypes.OPERATOR("not")] ]
                  | DataTypes.Posnumeral(x) => [ StackDataTypes.NUMBER(x) ]
                  | DataTypes.Identifier(x) => [ StackDataTypes.VARIABLE(x) ]
                  | DataTypes.TT =>  [ StackDataTypes.BOOLEANLITERAL("true") ] 
                  | DataTypes.FF => [ StackDataTypes.BOOLEANLITERAL("false")]

fun getVariableIdx( variableName, mt  ) = 
    HashTable.lookup mt variableName 

fun getVariableValue( variableName, mt, memory ) = 
    let 
        val idxOfVariable = getVariableIdx( variableName, mt)
    in 
        Array.sub( memory, idxOfVariable )
    end 

fun setVariableValue( variableName, mt, memory, value) = 
    let 
        val idxOfVariable = getVariableIdx( variableName, mt ) 
    in 
        Array.update( memory, idxOfVariable, value)
    end 

fun stringToInteger( str ) = 
    let 
        val firstCharacter = String.substring( str, 0, 1 )
    in 
        if( firstCharacter = "+") then 
        let 
            val SOME x = Int.fromString (String.extract( str, 1, NONE ))
        in 
            x
        end 
        else 
        let 
            val SOME x = Int.fromString (str) 
        in 
            x
        end 
        
    end 

fun BooleanToInt(x) = if( x = "true") then 1 else 0

fun evalOperator( x , y , operator : string ) = 
    if( operator = "plus" ) then x + y else 
    if( operator = "minus") then x - y else 
    if( operator = "times") then x * y else 
    if( operator = "div") then x div y else 
    if( operator = "mod") then x mod y else 
    if( operator = "and") then x*y else 
    if( operator = "or") then ( if(x = 0 andalso y = 0 ) then 0 else 1 ) else 
    if( operator = "leq") then ( if(x <= y  ) then 1 else 0 ) else 
    if( operator = "lt") then ( if(x < y  ) then 1 else 0 ) else 
    if( operator = "eq") then ( if(x = y  ) then 1 else 0 ) else 
    if( operator = "neq") then ( if(x = y  ) then 0 else 1 ) else 
    if( operator = "gt") then ( if(x > y  ) then 1 else 0 ) else 
    if( operator = "geq") then ( if(x >= y  ) then 1 else 0 ) else 
    if( operator = "not") then 1 -x else 
    if( operator = "neg") then ~x else
0 

fun addListToStack( l , s ) = 
if( null l ) then 
    s 
else 
    addListToStack( tl l , FunStack.push( hd l, s))

fun addExpToStack( expression , stack ) = 
    let 
        val nodes = List.rev( expressionToStackNodes( expression ) ) 
    in 
        addListToStack( nodes , stack )
    end 

fun printVariable( variableName, mt, memory ) = 
    let 
        val  valueOfVariable = Int.toString ( getVariableValue( variableName , mt , memory ) ) 
    in 
        printToOutStream os ( "Value of variable " ^ variableName ^ ": " ^ valueOfVariable ^ "\n" )
    end 

fun getVariableInput( variableName ) = 
   let
        val SOME x = Int.fromString ( getInput( "Enter input for variable "^ variableName   ) ) 
   in
        x
   end  

fun printValueStack( vs ) = printToOutStream os (( FunStack.toString Int.toString vs) ^ "\n")
fun ValueStackToString( vs ) = ( FunStack.toString Int.toString vs)
fun expressionElementtoString(ele) = case ele of StackDataTypes.VARIABLE(x) => x
                                             | StackDataTypes.NUMBER(x) => x 
                                             | StackDataTypes.BOOLEANLITERAL(x) => x 
                                             | StackDataTypes.OPERATOR(x) => x 
                                             | StackDataTypes.READCMD(x) => ""
                                             | StackDataTypes.WRITECMD(x) => ""
                                             | StackDataTypes.WRITECMD_ => ""
                                             | StackDataTypes.SETCMD(x,y) => ""
                                             | StackDataTypes.ITECMD(x,y,z) => ""
                                             | StackDataTypes.WHILECMD(x,y) => ""
                                             | StackDataTypes.ITECMD_(x,y) => ""
                                             | StackDataTypes.SETCMD_(x) => "" 
                                             | StackDataTypes.WHILECMD_(x,y) => "" 


fun ExpListToString( list ) = if null list then ""
                              else 
                              let 
                                    val joinWith = if ( null (tl list )) then "" else "."
                              in
                                    ( expressionElementtoString( hd list))^joinWith^(ExpListToString( tl list))
                              end 

fun ExpToString ( exp ) = let 
                            val nodeList = expressionToStackNodes( exp )
                         in 
                            ExpListToString( nodeList )
                         end 


fun CommandtoString ( cmd ) = case cmd of DataTypes.SET(x , y ) =>  x^"."^ExpToString( y)^ ".SET"
                                          | DataTypes.Read(x) => x ^"."^"READ"
                                          | DataTypes.Write(x) => ExpToString(x) ^  ".WRITE"
                                          | DataTypes.ITE(x,y,z) => ExpToString(x)^"."^commandSeqtoString(y )^"."^commandSeqtoString(z )^".ITE"
                                          | DataTypes.WH(x , y ) => ExpToString(x) ^ "." ^ commandSeqtoString( y  ) ^ ".WHILE"                                                           

and commandSeqtoString( cmds ) = 
if ( null cmds ) then 
   ""
else
    let 
        val joinWith = if( null ( tl cmds )) then "" else "."
    in
    CommandtoString( hd cmds ) ^ joinWith ^ commandSeqtoString( tl cmds  )
    end 

fun StackNodetoString(  node ) = case node of StackDataTypes.VARIABLE(x) => x
                                             | StackDataTypes.NUMBER(x) => x 
                                             | StackDataTypes.BOOLEANLITERAL(x) => x 
                                             | StackDataTypes.OPERATOR(x) => x 
                                             | StackDataTypes.READCMD(x) => x^".READ"
                                             | StackDataTypes.WRITECMD(x) => ExpToString(x)^".WRITE"
                                             | StackDataTypes.WRITECMD_ => "WRITE"
                                             | StackDataTypes.SETCMD(x,y) => x^"."^ExpToString(y)^".SET"
                                             | StackDataTypes.ITECMD(x,y,z) => ExpToString(x)^"."^commandSeqtoString(y )^"."^commandSeqtoString(z)^".ITE"
                                             | StackDataTypes.WHILECMD(x,y) => ExpToString(x)^"."^commandSeqtoString(y )^"."^"WHILE"
                                             | StackDataTypes.ITECMD_(x,y) => "cmdseq" ^ "." ^ "cmdseq" ^ "." ^ "ITE"
                                             | StackDataTypes.SETCMD_(x) => x^"."^"SET" 
                                             | StackDataTypes.WHILECMD_(x,y) => "partial-while"    

(* prints the string representation of the control stack *)
fun printControlStack( cs) = printToOutStream os ( (( FunStack.toString StackNodetoString cs) ^ "\n") )                                    

(* Converts the control stack to string *)
fun ControlStackToString( cs ) = ( FunStack.toString StackNodetoString cs ) 

(* prints the string representation of the memory *)
fun MemoryToString( memory , answer , idx , numberOfVariables ) = 
if( idx = numberOfVariables )then 
    answer
else 
    let 
        val temp = Int.toString ( Array.sub( memory, idx) )
        val joinWith = if( answer = "") then "" else "-"
    in 
        MemoryToString( memory , answer ^ joinWith ^ temp , idx + 1 , numberOfVariables )
    end 

fun addCommandsToStack( commands, stack ) = 
if( null commands ) then 
    stack 
else
    addCommandsToStack( tl commands, FunStack.push( hd commands, stack))

fun printValue( valueToBePrinted ) = printToOutStream os ( Int.toString valueToBePrinted)

signature VMC = 
sig 
  val inputFile : string
  val ASTTree : DataTypes.AST
  val filename : string
  val block : DataTypes.BLK
  val declarations : DataTypes.DEC list
  val commands : DataTypes.CMD list
  val postfix : DataTypes.AST -> string
  val postFixString : string
  val createMemory : DataTypes.DEC list
                     -> int array * (string,int) HashTable.hash_table * 
                        (string,DataTypes.Type) HashTable.hash_table * int
  val rules : int FunStack.Stack * int array * 
              (string,int) HashTable.hash_table * 
              StackDataTypes.STACKNODE FunStack.Stack
              -> int FunStack.Stack * int array * 
                 (string,int) HashTable.hash_table * 
                 StackDataTypes.STACKNODE FunStack.Stack
  val execute : int FunStack.Stack * int array * 
                (string,int) HashTable.hash_table * 
                StackDataTypes.STACKNODE FunStack.Stack
                -> int FunStack.Stack * int array * 
                   (string,int) HashTable.hash_table * 
                   StackDataTypes.STACKNODE FunStack.Stack
  val memory : int array
  val MemoryTable : (string,int) HashTable.hash_table
  val typeTable : (string,DataTypes.Type) HashTable.hash_table
  val numberOfVariables : int
  val ControlStack : StackDataTypes.STACKNODE FunStack.Stack
  val ValueStack : 'a FunStack.Stack
  val toString : int FunStack.Stack * int array * 
                 StackDataTypes.STACKNODE FunStack.Stack
                 -> string list
  val stringrep : string list
  val answer : int FunStack.Stack * int array * 
               (string,int) HashTable.hash_table * 
               StackDataTypes.STACKNODE FunStack.Stack
  val MemoryStr : string

end 


structure Vmc :> VMC = 

struct 

val inputFile = getInput("Enter file name ")  
val ASTTree = My.compile inputFile
val DataTypes.PROG(filename , block ) = ASTTree
val DataTypes.BLK( declarations , commands ) = block 

(* Function takes AST and converts it into a post fix tree  *)
fun postfix( tree ) = 
let
    val DataTypes.PROG(file  , blk  ) = tree
    val DataTypes.BLK( decs , cmds ) = block 
    val cs = createControlStack( List.rev (processCommands cmds ) ,  FunStack.create )  
in 
    ControlStackToString(cs)    
end 

val postFixString = postfix( ASTTree )
(* This function creates the memory and corresponding symbol tables *)
fun createMemory( declarations ) = 
    let
        val memory = initializeArray( 100 )  
        val MemoryTable : (string, int) HashTable.hash_table =  HashTable.mkTable (HashString.hashString, op=) (42, Fail "not found")
        val typeTable : (string, DataTypes.Type) HashTable.hash_table = HashTable.mkTable (HashString.hashString, op=) (42, Fail "identifier not found")
        val numberOfVariables  = initializeTables( declarations , typeTable , MemoryTable , 0 ) 
    in 
        ( memory, MemoryTable, typeTable, numberOfVariables )
    end 

(* implements the semantic rules of the VMC *)
fun rules(ValueStack, memory , mt , ControlStack ) = 
if FunStack.empty ControlStack then 
    ( ValueStack, memory,mt ,  ControlStack )
else 
let 
    val topElement = FunStack.top ControlStack 
    (* val nothing = printValueStack ValueStack *)
in 
    case topElement of StackDataTypes.NUMBER(x) => (FunStack.push( stringToInteger(x) ,  ValueStack) , memory , mt , FunStack.pop ControlStack)
                      | StackDataTypes.VARIABLE(x) => (FunStack.push( getVariableValue( x , mt , memory) ,  ValueStack) , memory  , mt , FunStack.pop ControlStack)
                      | StackDataTypes.BOOLEANLITERAL(x) => (FunStack.push( BooleanToInt(x) ,  ValueStack) , memory  , mt , FunStack.pop ControlStack)
                      | StackDataTypes.OPERATOR(x) =>  if( x = "neg" orelse x = "not") then 
                                                        let
                                                            val SOME (a,b) = FunStack.poptop( ValueStack )
                                                            val answer = evalOperator(a , 0 , x ) 
                                                        in 
                                                        ( FunStack.push( answer , b), memory , mt , FunStack.pop ControlStack)
                                                        end 
                                                        else 
                                                        let
                                                            val SOME (a,b) = FunStack.poptop( ValueStack )
                                                            val SOME (c,d) = FunStack.poptop( b )
                                                            val answer = evalOperator(c, a , x ) 
                                                        in 
                                                        ( FunStack.push( answer , d), memory , mt , FunStack.pop ControlStack)
                                                        end     
                    | StackDataTypes.SETCMD(x , y )  => let 
                                                            val a = FunStack.pop ControlStack (* remove SETCMD*)
                                                            val b = FunStack.push( StackDataTypes.SETCMD_(x) , a )
                                                            val c = addExpToStack( y , b )
                                                        in 
                                                            ( ValueStack, memory , mt , c )
                                                        end 
                    | StackDataTypes.SETCMD_(x)     => let 
                                                            val SOME(a,b) = FunStack.poptop( ValueStack )
                                                            val temp = setVariableValue( x , mt , memory , a )
                                                        in 
                                                            ( b , memory , mt , FunStack.pop( ControlStack ))
                                                        end 
                    | StackDataTypes.WRITECMD(x)    =>  let 
                                                            val a = FunStack.pop ControlStack (* remove WRITECMD *)
                                                            val b = FunStack.push( StackDataTypes.WRITECMD_ , a )
                                                            val c = addExpToStack( x , b )
                                                        in 
                                                            ( ValueStack, memory , mt , c )
                                                        end 
                    | StackDataTypes.WRITECMD_      =>  let 
                                                            val SOME(topValue , remainingValueStack) = FunStack.poptop( ValueStack )
                                                        in 
                                                           ( printValue( topValue) ; ( remainingValueStack, memory , mt , FunStack.pop( ControlStack )))
                                                        end 
                    | StackDataTypes.READCMD(x)     => let 
                                                            val y = getVariableInput( x )
                                                            val temp =  setVariableValue(x , mt , memory , y ) 
                                                        in
                                                            ((ValueStack , memory , mt , FunStack.pop ControlStack))
                                                        end  
                    | StackDataTypes.ITECMD(x, y, z) =>let 
                                                            val a = FunStack.pop ControlStack (* remove ITECMD *)
                                                            val b = FunStack.push( StackDataTypes.ITECMD_(y,z) , a )
                                                            val c = addExpToStack( x , b )
                                                            
                                                        in 
                                                            ( ValueStack, memory , mt , c )
                                                        end       
                    | StackDataTypes.ITECMD_(y,z)   => let  
                                                            
                                                            val SOME( condition ,remainingValueStack) = FunStack.poptop( ValueStack )
                                                            val remainingControlStack = ( FunStack.pop ControlStack ) 
                                                            val nothing = print ( Int.toString condition)
                                                        in 
                                                            if( condition = 1 ) then 
                                                               ( (remainingValueStack , memory , mt , addCommandsToStack(  List.rev (processCommands y ) , remainingControlStack  ) )  ) 
                                                            else 
                                                              (  (remainingValueStack , memory , mt , addCommandsToStack(  List.rev (processCommands z ) , remainingControlStack  ) )  ) 

                                                        end   
                    | StackDataTypes.WHILECMD( x , y )=>  let 
                                                            val a = FunStack.pop ControlStack (* remove WHILECMD *)
                                                            val b = FunStack.push( StackDataTypes.WHILECMD_(x,y) , a )
                                                            val c = addExpToStack( x , b )
                                                          in 
                                                            ( ValueStack, memory , mt , c )
                                                          end  
                    | StackDataTypes.WHILECMD_( x , y ) => let  
                                                            val SOME( condition ,remainingValueStack) = FunStack.poptop( ValueStack )
                                                            val remainingControlStack = ( FunStack.pop ControlStack ) 
                                                            val temp = FunStack.push( StackDataTypes.WHILECMD(x,y) , remainingControlStack )
                                                            val updatedControlStack =  addCommandsToStack(  List.rev (processCommands y ) , temp  ) 
                                                         in 
                                                            if( condition = 1 ) then 
                                                                (remainingValueStack , memory , mt ,updatedControlStack  ) 
                                                            else 
                                                               (remainingValueStack , memory , mt , remainingControlStack ) 

                                                         end              
 
end 
(* This function takes inital VMC machine and outputs the final VMC machine *)           
fun execute( ValueStack, memory, mt , ControlStack ) = 
    if(FunStack.empty ControlStack) then 
        (ValueStack, memory, mt , ControlStack) 
    else
        let
            val temp = rules( ValueStack, memory, mt, ControlStack )
        in 
            execute( #1 temp, #2 temp , #3 temp , #4 temp )
        end 

(* create memory and initialize type tables *)
val ( memory , MemoryTable , typeTable, numberOfVariables) = createMemory( declarations )

val ControlStack = createControlStack( List.rev (processCommands commands ) ,  FunStack.create )  
val ValueStack = FunStack.create 


(* converts the VMC machine to string *)
fun toString ( ValueStack, Memory, ControlStack ) =
let
    val MemoryStr = MemoryToString( Memory , "" , 0 , numberOfVariables )
    val ControlStr = ControlStackToString( ControlStack)
    val ValueStr = ValueStackToString( ValueStack )
in 
    [ ValueStr, ControlStr, MemoryStr ]
end  

val stringrep = toString( ValueStack, memory , ControlStack)

val nothing = printControlStack( ControlStack ) 
val answer = execute( ValueStack, memory , MemoryTable , ControlStack)
(* val nothing = printValueStack( #1 answer) ;  *)
val MemoryStr = MemoryToString( memory, "" , 0 , numberOfVariables)
(* close output stream to text file *)
val closeStream = TextIO.closeOut os
end ; 