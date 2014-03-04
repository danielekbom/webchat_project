(* findE(x, n)
 * TYPE: int/1 * int/1 -> int/1
 * PRE: n > 0
 * POST: the lowest coprime of x starting from n
 * VARIANT: The probability of (x mod n) = 0 (where n is incremented by one in every recursive step)
 *)		
fun findE(x, n) = if IntInf.eq(IntInf.mod(x, n), IntInf.fromInt(0)) then
						findE(x, IntInf.+(n, IntInf.fromInt(1))) 
					else n

(* convertPassword(pass)
 * TYPE: string -> string
 * PRE: n > 0
 * POST: pass with every char replaced by their ascii value
 * EXAMPLE: convertPassword("hello123") = 979899495051
 *)		
fun convertPassword pass = foldr (fn (x,y) => Int.toString(ord(x)) ^ y) "" (explode(pass))

(* encrypt(input)
 * TYPE: string -> string
 * PRE: size input > 1
 * POST: input as an encrypted string using a simplified version of the RSA algorithm with fixed prime numbers p and q and lowest possible value on e.
		 This means that input will always be encrypted to the same number regardless of how many times the encrypt function is called.
         The encryption is one-way in the sense that no decryption key is generated (and also not needed).  
 *)	
fun encrypt input = 
    let 
	val p = valOf(IntInf.fromString("5371393606024775251256550436773565977406724269152942136415762782810562554131599074907426010737503501"))
	val q = valOf(IntInf.fromString("2908511952812557872434704820397229928450530253990158990550731991011846571635621025786879881561814989"))
	
	val largeOne = IntInf.fromInt(1)
	
	val n = IntInf.*(p, q)
	val m = IntInf.*(IntInf.-(p, largeOne), IntInf.-(q, largeOne))

	val e = IntInf.toInt(findE(m, largeOne))

	val final = valOf(IntInf.fromString(convertPassword(input)))
    in
		IntInf.toString(IntInf.mod(IntInf.pow(final, e), n))
    end