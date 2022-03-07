(* glue.sml Create a lexer and a parser *)
structure MyLrVals = MyLrValsFun(
structure Token = LrParser.Token);
structure MyLex = MyLexFun(
structure Tokens = MyLrVals.Tokens);
structure MyParser = Join(
structure ParserData = MyLrVals.ParserData
structure Lex=MyLex
structure LrParser=LrParser);
