load "IntInf"

fun findE(x, n) = if IntInf.eq(IntInf.mod(x, n), IntInf.fromInt(0)) then findE(x, IntInf.+(n, IntInf.fromInt(1))) else n

fun convertPassword pass = 
    let	
		fun convert pwList = foldr (fn (x,y) => Int.toString(ord(x)) ^ y) "" (explode(pwList))
    in 
		convert(pass)
    end

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
		IntInf.mod(IntInf.pow(final, e), n)
    end;

val x = encrypt("123a");