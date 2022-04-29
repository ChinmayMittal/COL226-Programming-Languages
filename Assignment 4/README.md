### Running the program
---
On the command line go inside the directory of the assignment and type 
```
sml
```

Once the interactive shell is open type 

```
use "start.sml" ; 
```

This command compiles all code, creates the AST and then runs the VMC machine on this tree 

There will be a prompt for the filename 

```
Enter file name 
prog.txt
```

Do not delete the file ```output.txt```, for convineance I write all outputs to this file instead of the console for more readibility 

Any input to the program ( read command ) results in the following prompt 

```
Enter input for variable A
6
```

---

Files
---
1. ***stack.sml*** contains the signature STACK and the structure FunStack, which implements this signature 

2. ***VMC.sml*** is the main file and it contains the structure Vmc 

3. ***output.txt*** is used to dump the output of the program instead of dumping it on the command line ( DO NOT DELETE THIS FILE )

4. ***stackdatatypes.sml*** contains the datatypes for elements pushed on the Control Stack 

5. ***start.sml*** contains the code to compile all the files by invoking the compilation manager

6. Other files are from previous assignment 

---

### Sample programs 

--- 

Factorial Program 
```
program test::
    var A, B : int;
    var E: bool;
{
     read A ;
     B := 1 ;  
     while( A >= 1 ) do {
         B := B * A ; 
         A := A - 1 ; 
     } endwh ; 
     write B ; 

}
 
```

Output 

```
A.READ.B.1.SET.A.1.geq.B.B.A.times.SET.A.A.1.minus.SET.WHILE.B.WRITE
5040
```

---

isEven  program 

```
program test::
    var A: int;
    var isEven  : bool;
{
     read A ;
     if( (A % 2 )  = 0 ) then 
     {
         isEven := tt ; 
     } else {
         isEven := ff  ; 
     } endif ; 
     write isEven ; 

}
```
Output 
```
A.READ.A.2.mod.0.eq.isEven.true.SET.isEven.false.SET.ITE.isEven.WRITE
0
```

---

program isPrime 

```
program isPrime ::
    var A , B  , C : int;
    var isPrime , break  : bool;
{
     read A ;
     break := ff ; 
     if( (A = 0 ) || ( A = 1 )) then 
     {
         isPrime := ff ; 
     }else {
         B := 2 ; 
         C := A - 1 ; 
         isPrime := tt ; 
         while( B <= C && (! break) ) do {
            if( (A % B) = 0 ) then {
                isPrime := ff ; 
                break := tt ; 
            }else{
                B := B + 1 ; 
            } endif ; 
         } endwh ; 

     } endif ;

     write isPrime ;  

}
```

output
```
A.READ.break.false.SET.A.0.eq.A.1.eq.or.isPrime.false.SET.B.2.SET.C.A.1.minus.SET.isPrime.true.SET.B.C.leq.break.not.and.A.B.mod.0.eq.isPrime.false.SET.break.true.SET.B.B.1.plus.SET.ITE.WHILE.ITE.isPrime.WRITE
0
```

--- 

gcd program 

```
program gcd ::
    var A , B  , C , GCD  : int;
{
     read A ;
     read B ; 
     C := 1 ; 
     GCD := 1 ; 

     while( C <= A &&  C <= B ) do {
        if( A%C = 0 && B%C = 0 ) then 
        {
            GCD := C ; 
        }else{

        } endif ; 
        C := C + 1 ; 
     } endwh ; 
     write GCD ; 
}
```

```
A.READ.B.READ.C.1.SET.GCD.1.SET.C.A.leq.C.B.leq.and.A.C.mod.0.eq.B.C.mod.0.eq.and.GCD.C.SET..ITE.C.C.1.plus.SET.WHILE.GCD.WRITE
17
```
