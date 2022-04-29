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

%%
%name My

%term LBRACE | RBRACE | LPAREN | RPAREN | EOF | 
      IF | ELSE | THEN | ENDIF | WHILE | DO | ENDWH | 
      COLON | SEMICOLON | COMMA | INT | BOOL | 
      LT | LEQ | NEQ | EQ | GT | GEQ | 
      PLUS | MINUS | TIMES | DIV | MOD | 
      NOT | AND | OR | TRUE | FALSE | PROGRAM | VAR | NEG | 
      ASSIGN | READ | WRITE | PROGSTART | POSNUMERAL of string | IDENTIFIER of string

%nonterm start of AST | blk of BLK | 
         decseq of DEC list | comseq of CMD list  |
         commands of CMD list | 
         dec of DEC |
         command of CMD  |
         typedec of Type   |
         varlist of string list |
         expression of Type * Exp 


%eop EOF
%noshift EOF
%pos int 
%verbose 

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left GT GEQ LT LEQ
%left PLUS MINUS 
%left TIMES MOD DIV 
%right NOT NEG 
%left LPAREN RPAREN 

%%

start: PROGRAM IDENTIFIER PROGSTART blk ( HashTable.clear typeTable ; PROG( IDENTIFIER , blk ))

blk: decseq comseq (BLK(decseq , comseq ))
decseq:  (([]))
        |dec decseq ( (dec::decseq))

comseq: LBRACE commands RBRACE ((commands))

commands: (([]))
        | command SEMICOLON commands ((command::commands))

dec: VAR varlist COLON typedec SEMICOLON (( insertInTable( varlist , typedec) ; DEC(varlist , typedec)))

varlist:  IDENTIFIER (([IDENTIFIER]))
         | IDENTIFIER COMMA varlist  ((IDENTIFIER::varlist))

typedec:  INT ((Int)) 
      | BOOL ((Bool))

command: READ IDENTIFIER ((Read(IDENTIFIER)))
        | WRITE expression ((Write(#2 expression )))
        | IDENTIFIER ASSIGN expression (( if (checkSameType( getType IDENTIFIER,  #1 expression)) then () else raise TypeMisMatchException ;  SET(IDENTIFIER ,#2 expression )))
        | IF expression THEN comseq ELSE comseq ENDIF ( (if( checkBool(#1 expression)) then () else raise TypeMisMatchException ;ITE( #2 expression , comseq1  , comseq2)))
        | WHILE expression DO comseq ENDWH ( ( if( checkBool( #1 expression)) then () else raise TypeMisMatchException ; WH( #2 expression , comseq )))

expression: expression LEQ expression ( (  if checkSameType( #1 expression1 , #1 expression2) then () else raise TypeMisMatchException ;(Bool , LEQ(#2 expression1 , #2 expression2))))
            | expression LT expression (( if checkSameType( #1 expression1 ,#1 expression2) then () else raise TypeMisMatchException ; (Bool , LT(#2 expression1 , #2 expression2))))
            | expression EQ expression (( if checkSameType( #1 expression1 , #1 expression2) then () else raise TypeMisMatchException ; (Bool , EQ(#2 expression1 , #2 expression2))))
            | expression NEQ expression (( if checkSameType( #1 expression1 ,#1 expression2) then () else raise TypeMisMatchException ; (Bool , NEQ(#2 expression1 , #2 expression2)) ))
            | expression GT expression (( if checkSameType( #1 expression1 , #1 expression2) then () else raise TypeMisMatchException ; (Bool , GT(#2 expression1 , #2 expression2))))
            | expression GEQ expression (( if checkSameType( #1 expression1 ,  #1 expression2) then () else raise TypeMisMatchException ; (Bool , GEQ(#2 expression1 , #2 expression2))))
            
            | expression PLUS expression (( if checkInt( #1  expression1) andalso checkInt( #1 expression2) then () else raise TypeMisMatchException ;(Int , PLUS(#2 expression1 , #2 expression2))))
            | expression MINUS expression (( if checkInt( #1  expression1) andalso checkInt( #1 expression2) then () else raise TypeMisMatchException ;(Int , MINUS(#2 expression1 , #2 expression2))))
            | expression TIMES expression ((if checkInt( #1  expression1) andalso checkInt( #1 expression2) then () else raise TypeMisMatchException ; (Int , TIMES(#2 expression1 , #2 expression2))))
            | expression DIV expression (( if checkInt( #1  expression1) andalso checkInt( #1 expression2) then () else raise TypeMisMatchException ;(Int , DIV(#2 expression1 , #2 expression2))))
            | expression MOD expression (( if checkInt( #1  expression1) andalso checkInt( #1 expression2) then () else raise TypeMisMatchException ;(Int , MOD(#2 expression1 , #2 expression2))))
           
            | expression AND expression (( if checkBool(#1 expression1) andalso checkBool(#1 expression2) then () else raise TypeMisMatchException ;(Bool , AND(#2 expression1 , #2 expression2))  ))
            | expression OR expression ((if checkBool(#1 expression1) andalso checkBool(#1 expression2) then () else raise TypeMisMatchException ; (Bool , OR(#2 expression1 , #2 expression2)) ))
            
            | LPAREN expression RPAREN (( expression))

            | NEG expression ((if checkInt(#1 expression) then () else raise TypeMisMatchException ;(Int , NEG( #2 expression)))) 
            
            | TRUE ((Bool , TT ))
            | FALSE ((Bool , FF ))
            
            | POSNUMERAL (( Int , Posnumeral(POSNUMERAL) ))

            | IDENTIFIER (( getType IDENTIFIER , Identifier(IDENTIFIER) ))

            | NOT expression ( if checkBool(#1 expression) then () else raise TypeMisMatchException ; (Bool , NOT(#2 expression)) )

