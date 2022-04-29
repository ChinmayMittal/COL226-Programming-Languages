structure StackDataTypes = 
struct
datatype STACKNODE =  VARIABLE of string 
                    | NUMBER of string
                    | BOOLEANLITERAL of string 
                    | OPERATOR of string  
                    | READCMD of string 
                    | WRITECMD of DataTypes.Exp 
                    | WRITECMD_  (* partially completed write command with value to be written on top of value stack*)
                    | SETCMD of string * DataTypes.Exp 
                    | ITECMD of DataTypes.Exp * ( DataTypes.CMD list ) * ( DataTypes.CMD list )
                    | WHILECMD of DataTypes.Exp * ( DataTypes.CMD list ) 
                    | SETCMD_ of string (* partial set command string => variable name *)
                    | ITECMD_ of ( DataTypes.CMD list ) * ( DataTypes.CMD list ) (* partially completed if then else command with value of boolean on top of value stack *)
                    | WHILECMD_ of ( DataTypes.Exp )  * ( DataTypes.CMD list ) (* partially completed while command with value of expression on top of value stack   *)
end ; 
