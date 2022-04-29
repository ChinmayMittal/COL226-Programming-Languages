structure DataTypes = 
struct
datatype AST  = PROG of string * BLK 
and BLK = BLK of (DEC list) * ( CMD list ) 
and DEC = DEC of ( (string list) * Type )
and Type = Int | Bool
and CMD = SET of string*Exp | WH of Exp* (CMD list )| ITE of Exp * ( CMD list ) * (CMD list ) | Read of string | Write of Exp 
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

end ; 
