val maxMemSize = 100 ;

(*Exception Definitions*)
exception DivisionByZero
exception ModulusByZero
exception IndexOutOfBoundForCodeVector
exception InValidOpCode	      
exception IndexOutOfBound

(* converts ["1","2"] to [1,2]*)
fun StringListToIntList( stringList : string list) =
    if null stringList
    then
	[]
    else
	valOf ( Int.fromString( hd stringList )) :: StringListToIntList( tl stringList)

								       
(* Removes the brackets and \n and tokenizes the string with respect to , *)
fun processLine( line : string ) =
    let 
	val removedEnds = String.substring( line  , 1 , String.size( line)-3)
    in
	( String.tokens( fn ch => ch = #",") )  removedEnds 
    end
	

(* creates the read stream for the file*)
fun readFile( infile : string ) =
    let
	val inStream = TextIO.openIn( infile )
    in
	inStream
    end
	
(* Keeps reading till end of input stream*)	
fun readNextLine( instream : TextIO.instream ) =
    let
	val x =  TextIO.inputLine( instream )
    in
	if isSome(x) andalso valOf(x) <> "\n"
	then
	( ( processLine( valOf x))) :: readNextLine( instream ) 
	else
	    []
    end
	
		
fun generateCode( filename : string ) =
    ( readNextLine(readFile(filename)))
	
fun convert( a : string list list ) = 
	   if null a
	   then
	       []
	   else
	     ( StringListToIntList( hd a)) :: convert( tl a )

(* Initialize memory as an array*)
fun initializeArray( size : int ) =
    Array.array( size  , 0   )

fun getInput() =
   ( print("Input: ") ; 
    let
	val str = valOf(TextIO.inputLine TextIO.stdIn)
    in
	valOf ( Int.fromString( str ) )
    end)
	
		   
 
val memory = initializeArray maxMemSize ; 

(* Helper functions *)
fun inverse( num : int ) =
    if num = 0
    then 1
    else
	0


fun IntToBool( num : int ) =
    if num = 0
    then 0
    else
	1
fun logicalOR( a : int , b : int ) =
    if( a = 0  andalso b = 0 )
    then 0
    else
	1
fun logicalAND( a : int , b : int ) =
    if( a <> 0 andalso b<> 0 )
    then 1
    else
	0
fun BoolToInt( a : bool ) =
    if a then 1 else 0
	    	
fun CheckIdxError( idx : int ) = if( idx < 0 orelse idx >= maxMemSize ) then true else false 



(* The main function for executing statements line by line, given an index the function 
solves it and moves to the next appropriate index*)
	    	
fun solve( idx : int , codeVector : int list vector , codeSize : int ) = 
    if (  idx >= codeSize orelse idx < 0 ) 
    then
	raise IndexOutOfBoundForCodeVector
    else
	let
	    val quad  = Vector.sub( codeVector , idx )
	 
	in 
	    let
		val opCode = hd quad
		val op1 = hd ( tl quad)
		val op2 = hd ( tl (tl quad))
		val target = hd( tl ( tl ( tl (quad))))
			     
	    in
		if opCode = 0 then () (*Halting*)
		else if ( ((opCode >=1 andalso opCode<=12) orelse (opCode = 16)) andalso CheckIdxError(target)) (*Checks if target is an invalid index in relevant cases *)
		then  raise IndexOutOfBound
		else if( ( opCode >=4 andalso opCode <=12) andalso CheckIdxError(op2))(*Checks if op2 is an invalid index in relevant cases *)
		then raise IndexOutOfBound
		else if(((opCode >=2 andalso opCode <= 13) orelse (opCode=15)) andalso CheckIdxError(op1))(*Checks if op1 is an invalid index in relevant cases *)
		then raise IndexOutOfBound
		else if opCode = 1 then
		   ( Array.update( memory , target , getInput() ) ; 
		    solve(idx+1, codeVector , codeSize))
		else if opCode = 2 then
		    (Array.update( memory , target , Array.sub( memory , op1)); solve(idx+1, codeVector , codeSize))
		else if opCode = 3
		then
		    (Array.update(memory , target ,inverse( Array.sub(memory , op1)) ) ; solve( idx + 1, codeVector , codeSize ))  
		else
		    let
			val a = Array.sub( memory , op1)
			val b = Array.sub( memory , op2 )		 
		    in
			if opCode = 4
			then (Array.update(memory , target , logicalOR(a,b)) ; solve(idx+1, codeVector , codeSize))
			else if opCode = 5
			then (Array.update( memory, target , logicalAND(a,b)) ; solve(idx+1, codeVector , codeSize))    
			else if opCode = 6
			then ( Array.update( memory , target , a + b ); solve(idx+1, codeVector , codeSize) )
			else if opCode = 7
			then ( Array.update( memory , target , a-b) ; solve(idx+1, codeVector , codeSize))
			else if opCode = 8
			then (Array.update( memory , target , a*b) ; solve(idx+1, codeVector , codeSize))
			else if opCode = 9
			then
			    if(b = 0 )
			    then
				raise DivisionByZero
			    else
				(Array.update(memory , target , a div b) ; solve(idx+1, codeVector , codeSize))
			else if opCode = 10
			then
			    if ( b = 0 )
			    then
				raise ModulusByZero
			    else
				(Array.update(memory,target , a mod b ) ; solve(idx+1, codeVector , codeSize))
			else if opCode = 11
			then
			    (Array.update(memory,target, BoolToInt(a=b)) ; solve(idx+1, codeVector , codeSize))
			else if opCode = 12
			then
			    (Array.update( memory , target , BoolToInt( a > b ) ) ; solve(idx+1, codeVector , codeSize))
			else if opCode = 13
			then
			    if (Array.sub(memory , op1 ) = 1 )
			    then solve( target, codeVector , codeSize)
			    else
				solve(idx+1, codeVector , codeSize)
			else if opCode = 14
			then
			    solve( target, codeVector , codeSize )
			else if opCode = 15
			then
			   ( print( Int.toString(Array.sub(memory , op1))^"\n"); solve(idx+1 , codeVector , codeSize ) ) 
			else if opCode = 16
			then
			    (Array.update(memory,target,op1) ; solve(idx+1, codeVector , codeSize))
			else			 
			    raise InValidOpCode
		    end
	    end
	end

(* takes the filename, generates code vector and solves it*)		 
fun interpret( filename : string  )=
    let
	val codeVector =Vector.fromList (  convert (  generateCode( filename)) ) 
	val codeSize  = Vector.length codeVector
	val memory = initializeArray maxMemSize
    in
	solve(0 , codeVector , codeSize )
    end
