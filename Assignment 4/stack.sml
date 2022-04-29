signature STACK =
sig
    type 'a Stack
    exception EmptyStack
    exception Error of string
    val create : 'a Stack
    val push : 'a * 'a Stack -> 'a Stack
    val pop : 'a Stack -> 'a Stack
    val top : 'a Stack -> 'a
    val empty: 'a Stack -> bool 
    val poptop : 'a Stack -> ('a * 'a Stack) option
    val nth : 'a Stack * int -> 'a
    val drop : 'a Stack * int -> 'a Stack
    val depth : 'a Stack -> int  
    val app : ('a -> unit) -> 'a Stack -> unit
    val map : ('a -> 'b) -> 'a Stack -> 'b Stack
    val mapPartial : ('a -> 'b option) -> 'a Stack -> 'b Stack
    val find : ('a -> bool) -> 'a Stack -> 'a option
    val filter : ('a -> bool) -> 'a Stack -> 'a Stack
    val foldr : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
    val foldl : ('a * 'b -> 'b) -> 'b -> 'a Stack -> 'b
    val exists : ('a -> bool) -> 'a Stack -> bool
    val all : ('a -> bool) -> 'a Stack -> bool
    val list2stack : 'a list -> 'a Stack (* Convert a list into a stack *)
    val stack2list: 'a Stack -> 'a list (* Convert a list into a stack *)
    val toString: ('a -> string) -> 'a Stack -> string 
end

structure FunStack :> STACK =
struct
    datatype 'a Stack = NilStack | CONS of 'a * 'a Stack   
    exception EmptyStack
    exception Error of string
    val create = NilStack   
    val push = fn ( x , y ) => CONS(x,y) 
    val pop = fn( x ) => case x of NilStack => raise EmptyStack | CONS(topElement, rest) => rest 
    val top = fn x => case x of NilStack => raise EmptyStack | CONS ( topElement, rest ) => topElement  
    val empty = fn x => case x of NilStack => true | CONS(topElement,rest_) => false
    fun depth(x) = case x of NilStack => 0 |  CONS(topElement,rest) => 1 + depth(rest)
    fun poptop(x) = case x of NilStack => NONE | CONS(topElement,rest ) => SOME(topElement,rest)
    fun nth(x,n) = 
            if( n < 0  orelse n>= depth(x) ) then 
                raise Error( "Invalid second argument for nth") 
            else
                if( n  =  0 ) then  
                    top(x)
                else 
                    nth( pop(x) , n-1)
    fun drop( x , n ) = 
        if( n < 0  orelse n > depth(x) ) then 
            raise Error("Invalid argument for drop")
        else 
            if( n = 0 ) then 
                x
            else 
                drop( pop(x) , n-1 )
    fun app f x  =  
        case x of NilStack => () 
                 |CONS(topElement,rest ) =>  ( f(topElement ) ;  app f rest ) 
    fun map f x  =  
        case x of NilStack => NilStack 
                 |CONS(topElement,rest ) => let val tempAns =  map f rest in CONS( f(topElement), tempAns) end              
    
    fun mapPartial f x = case x of NilStack => NilStack 
                                  | CONS( topElement, rest ) => if isSome( f topElement ) then 
                                                                    CONS ( valOf(f topElement ) , mapPartial f rest )
                                                                else 
                                                                    mapPartial f rest 
                                                        
    fun filter f x = 
        case x of NilStack => NilStack 
                 |CONS(topElement, rest) => let 
                                                val tempAns =  filter f rest 
                                            in 
                                                if ( f topElement ) then 
                                                    CONS( topElement, tempAns) 
                                                else 
                                                    tempAns 
                                           end   
    fun find f x =  case x of NilStack => NONE 
                        | CONS( topElement, rest ) => if( f topElement ) then 
                                                        SOME( topElement ) 
                                                     else 
                                                        find f rest 

    fun foldr f init x = 
        case x of NilStack => init 
                 | CONS(topElement, rest) => f( topElement , (foldr f init rest ))
    
    fun foldl f init x = 
        case x of NilStack => init 
                 | CONS(topElement, rest) => foldl f (f( topElement, init)) rest 
    
    fun exists f x  = 
        case x of NilStack => false 
                 | CONS(topElement, rest) => if ( f topElement ) then 
                                                true 
                                            else 
                                                exists f x 
    fun all f x = 
        case x of NilStack => true 
                 | CONS( topElement, rest) => if ( f topElement ) then 
                                                all f rest 
                                             else 
                                                false   
    fun list2stack l = 
        case l of [] => NilStack
                  | a :: b => CONS( a , list2stack b )

    fun stack2list s = 
        case s of NilStack => [] 
                 | CONS(x,y) => x :: stack2list y 
    fun toString f x  = 
        case x of NilStack => "" 
                 | CONS(topElement,rest) => let
                                                val delimiter = if ( empty (rest)) then "" else "."
                                            in
                                                f(topElement) ^  delimiter  ^  ( toString f rest ) 
                                            end 
end