## Context-free grammar
---

start => PROGRAM IDENTIFIER PROGSTART blk 

blk =>  decseq comseq

decseq => $ \epsilon $ | dec decseq 

comseq => LBRACE commands RBRACE

commands => $ \epsilon $ | command SEMICOLON commands

dec => VAR varlist COLON typedec SEMICOLON 

varlist => IDENTIFIER | IDENTIFIER COMMA varlist

typedec => INT | BOOL 

command => READ expression | WRITE expression 
            |  IDENTIFIER ASSIGN expression 
            | IF expression THEN comseq ELSE comseq ENDIF 
            | WHILE expression DO comseq ENDWH

expression => 
            
            expression LEQ expression 
            | expression LT expression 
            | expression EQ expression 
            | expression NEQ expression 
            | expression GT expression 
            | expression GEQ expression 

            | expression PLUS expression 
            | expression MINUS expression
            | expression TIMES expression 
            | expression DIV expression 
            | expression MOD expression 
            | expression AND expression
            | expression OR expression 

            | LPAREN expression RPAREN 
            | NEG expression  
            | TRUE 
            | FALSE 
            
            | POSNUMERAL 

            | IDENTIFIER 

            | NOT expression 


## Syntax-directed translation ( will be used later for evaluating expressions )
---

expression => 
            
            expression LEQ expression  ( expression0.val := LEQ(expression1.val , expression2.val) , expression0.type = Bool )
            | expression LT expression ( expression0.val := LT(expression1.val , expression2.val) , expression.type = Bool )
            | expression EQ expression ( expression0.val := EQ(expression1.val , expression2.val) , expression0.type = Bool )
            | expression NEQ expression ( expression0.val := NEQ(expression1.val , expression2.val) , expression0.type = Bool )
            | expression GT expression ( expression0.val := GT(expression1.val , expression2.val) , expression0.type = Bool )
            | expression GEQ expression ( expression0.val := GEQ(expression1.val , expression2.val) , expression0.type = Bool )

            | expression PLUS expression ( expression0.val := PLUS(expression1.val , expression2.val) , expression0.type = Int )
            | expression MINUS expression ( expression0.val := MINUS(expression1.val , expression2.val) , expression0.type = Int )
            | expression TIMES expression  ( expression0.val := TIMES(expression1.val , expression2.val) , expression0.type = Int )
            | expression DIV expression  ( expression0.val := DIV(expression1.val , expression2.val) , expression0.type = Int )
            | expression MOD expression  ( expression0.val := MOD(expression1.val , expression2.val) , expression0.type = Int )
            | expression AND expression ( expression0.val := AND(expression1.val , expression2.val) , expression0.type = Bool )
            | expression OR expression ( expression0.val := OR(expression1.val , expression2.val) , expression0.type = Bool )

            | LPAREN expression RPAREN ( expression0.val := expression1.val , expression0.type = expression1.type )
            | NEG expression  ( expression0.val := NEG(expression1.val ) , expression0.type = Int )
            | TRUE ( expression0.val := true , expression0.type = Bool )
            | FALSE ( expression0.val := false , expression0.type = Bool )
            
            | POSNUMERAL ( expression0.val := VALUEOF(POSNUMERAL) , expression0.type = Int )

            | IDENTIFIER ( expression0.val := IDENTIFIER.type , expression0.type = IDENTIFIER.type  )

            | NOT expression ( expression0.val := NOT(expression1.val ) , expression0.type = Bool )

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
       
       
        | WRITE IDENTIFIER ((Write(IDENTIFIER)))
        
        
        | IDENTIFIER ASSIGN expression (( if (checkSameType( getType IDENTIFIER,  #1 expression)) then () else raise TypeMisMatchException ;  SET(IDENTIFIER ,#2 expression )))
        
        
        | IF expression THEN comseq ELSE comseq ENDIF ( (if( checkBool(#1 expression)) then () else raise 
        TypeMisMatchException ;ITE( #2 expression , comseq  , comseq)))
        
        
        | WHILE expression DO comseq ENDWH ( ( if( checkBool( #1 expression)) then () else raise TypeMisMatchException ; WH( #2 expression , comseq )))


These will be implemented later since val attribte is not available for certain expressions during parsing. 


I also do typechecking in each corresponding semantic rule for eg in expression => expression LT expression, i ensure that both expressions on the right side of the production have the same type, if not I raise an expression  

## AST datatype definition
---

```datatypes.sml``` defines datatypes for the non-terminals of the parser, these are also thus used to generate the AST ( output of the parser )

```
datatype AST  = PROG of string * BLK 
and BLK = BLK of (DEC list) * ( CMD list ) 
and DEC = DEC of ( (string list) * Type )
and Type = Int | Bool
and CMD = SET of string*Exp | WH of Exp* (CMD list )| ITE of Exp * ( CMD list ) * (CMD list ) | Read of string | Write of string
and Exp = NOT of Exp
          |  NEG of Exp
          | AND of Exp*Exp
          | OR of Exp*Exp
          | PLUS of Exp*Exp
          | MINUS of Exp*Exp
          | TIMES of Exp*Exp
          | MOD of Exp * Exp 
          | DIV of Exp * Exp 
          | LT of Exp*Exp
          | LEQ of Exp*Exp
          | NEQ of Exp*Exp
          | EQ of Exp*Exp
          | GT of Exp*Exp
          | GEQ of Exp * Exp 
          | TT
          | FF 
          | Posnumeral of string 
          | Identifier of string 

```


Definitions of terminals and non terminals 

```
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
```

Value attributes and computation of values will be handled later 

The first memeber of the tuple assosciated with expression is used for type checking 
 
## Other Design Decisions
---

The terminals are kept in all-caps and non-terminals are lower case for ease of distinction. 

I have combined all type of expressions ( bool | int ) into one and have used ML-yacc's declarartions to specify the associativity and precedence of operators, similar to those in most programming languages. The precdence and assosciativity of operators in the WHILE programming languages are as follows


The precdence increases as we move from down.

```
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left GT GEQ LT LEQ
%left PLUS MINUS 
%left TIMES MOD DIV 
%right NOT NEG 
%left LPAREN RPAREN 
```

I have implemented typecheking using hashtables, whenever a variable is defined its type is stored in a hashtable, this is used later during parsing to ensure that operands have proper type based on the operator.

For this purpose I have defined several helper functions and exceptions in  ```my.yacc```. The following cases are handled - 

1. Variable Redeclaration
2. Type Mismatch based on operators and operands
3. Using an undeclared variable 

Expressions such as  ``` 3 + tt ```  will raise type mismatch exceptions, fix the program file and repeat all steps to run the parser again.

## Other Implementation Decisions
---

I have combined integer and boolean expressions into one and each expression non terminal has a corresponding type assosciated with it.

I have used a hashtable to store the type of each identifier ( from the declraration statements ), I raise exceptions in case of variable redeclaration or type mismatch or undeclared variables.

The description of states and parsing table is generated in ```my.yacc.desc```


The expression non terminal has a tuple of Type and Exp as it's datatype which facilitates in type checking 


I have defined a terminal POSNUMERAL, which takes into account the optional + sign, the negation using ~ are taken care by the rule expression -> NEG expression. This removes any conficts


The comparison of boolean expressions is handled by the unified expression -> expression LET expression etc. I do typechecking to ensure that both expressions have same type , along with assosiativity rules and precedence this handles comparison without any conficts 


Value computation and corresponding attributes are not done currently, because this is done after the AST is created 

## Running the program

Open the SML REPL 
```
sml
```

I have used the compilation manager to handle all files

```
CM.make "my.cm" ; 
```

This creates a compile function in a structure named My , invoke this function with the name of the file with the WHILE program

```
My.compile "prog.txt" ; 
```

To prevent the REPL from hiding long information using #, use 

```
Control.Print.printDepth := 100 ; 
```

NOTE:  ***In case of exceptions, redo all the steps, by first exiting the REPL ( Ctrl + D )***

## SAMPLE CASES 
1. 
    ```txt
    program test::
        var A, B: int;
        var E: bool;
    {
        A:= 4 + B;  
    }
    ```

    Output

    ```txt
    val it =
    PROG
        ("test",
        BLK
        ([DEC (["A","B"],Int),DEC (["E"],Bool)],
            [SET ("A",PLUS (Posnumeral "4",Identifier "B"))])) : ?.DataTypes.AST
    ```
2. Demonstrating Exceptions 

    ```text
    program test::
        var A, B: int;
        var E: bool;
    {
        E := A + B ; 
    }
    ```

    Output 
    ```My.compile "prog.txt" ; 

    uncaught exception TypeMisMatchException
    raised at: my.yacc.sml:516.78-516.99
                while_ast.sml:23.52

    ```

## Acknowledgements
---

The following references have been used for this assignment

1. <a href="https://www.cs.princeton.edu/~appel/modern/ml/ml-yacc/manual.html" target="_blank">ML-Yacc Princeton Manual</a>
2. <a href="https://www.cs.princeton.edu/~appel/modern/ml/ml-lex/manual.html">ML-Lex Princeton Manual </a>
3. <a href = "http://rogerprice.org/ug/ug.pdf" > User's guide to ML-Lex and ML-Yacc </a>

The code in ```glue.sml```, ```my.cm```, ``` while_ast.sml```  was largely modified from resource 3.