structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
val eof = fn () => Tokens.EOF(!pos,!pos)

%%
%header (functor MyLexFun(structure Tokens: My_TOKENS));
alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
%%
\n       => ( pos:= (!pos) + 1; lex()); 
{ws}+    => (lex());
"program" => (Tokens.PROGRAM(!pos , !pos )) ; 
"var" => (Tokens.VAR(!pos , !pos )) ; 
"tt" => ( Tokens.TRUE(!pos , !pos ) ) ;
"ff" => (Tokens.FALSE(!pos , !pos )) ;  
"("      => (Tokens.LPAREN(!pos , !pos ));
")"      => (Tokens.RPAREN(!pos , !pos )) ; 
"}"      => (Tokens.RBRACE(!pos , !pos )) ; 
"{"      => (Tokens.LBRACE(!pos , !pos )) ; 
"!"      => (Tokens.NOT(!pos , !pos )) ; 
"&&"       => ( Tokens.AND(!pos , !pos )) ; 
"||"       => (Tokens.OR(!pos , !pos )) ;
";"       => (Tokens.SEMICOLON(!pos , !pos )) ;
":"       => (Tokens.COLON(!pos , !pos )) ;
","       => (Tokens.COMMA(!pos , !pos )) ;
":="       => (Tokens.ASSIGN(!pos , !pos )) ;
"+"       => (Tokens.PLUS(!pos , !pos )) ;
"-"       => (Tokens.MINUS(!pos , !pos )) ;
"*"       => (Tokens.TIMES(!pos , !pos )) ;
"/"       => (Tokens.DIV(!pos , !pos )) ;
"%"       => (Tokens.MOD(!pos , !pos )) ;
"read"       => (Tokens.READ(!pos , !pos )) ;
"write"       => (Tokens.WRITE(!pos , !pos )) ;
"if"       => (Tokens.IF(!pos , !pos )) ;
"else" => (Tokens.ELSE(!pos , !pos )) ; 
"endif"       => (Tokens.ENDIF(!pos , !pos )) ;
"while"       => (Tokens.WHILE(!pos , !pos )) ;
"endwh"       => (Tokens.ENDWH(!pos , !pos )) ;
"then"       => (Tokens.THEN(!pos , !pos )) ;
"do"       => (Tokens.DO(!pos , !pos )) ;
"int"       => (Tokens.INT(!pos , !pos )) ;
"bool"       => (Tokens.BOOL(!pos , !pos )) ;
"::"       => (Tokens.PROGSTART(!pos , !pos )) ;
"<="       => (Tokens.LEQ(!pos , !pos )) ;
">="       => (Tokens.GEQ(!pos , !pos )) ;
">"       => (Tokens.GT(!pos , !pos )) ;
"="       => (Tokens.EQ(!pos , !pos )) ;
"<>"       => (Tokens.NEQ(!pos , !pos )) ;
"<"       => (Tokens.LT(!pos , !pos )) ;
{alpha}({alpha} | {digit})* => ( Tokens.IDENTIFIER( yytext ,!pos , !pos )) ; 
([+]?)({digit}){digit}* => ( Tokens.POSNUMERAL( yytext , !pos , !pos ) ) ; 
"~" => (Tokens.NEG(!pos , !pos)) ; 






