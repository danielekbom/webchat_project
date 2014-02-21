open Mosmlcgi;
open TextIO;

datatype user = User of (string * string * string * int) | EmptyUser
datatype message = MSG of (string * string * string)
datatype chat = Chat of (string * message list) | EmptyChat

exception Hej;

fun getName [] = []
  | getName(x::xs) = if(x = #">") then [] else x::getName(xs);

fun getUser(is, name) = 
    let
	val cond = endOfStream(is);
	val line = inputLine(is);
	val linename = implode(getName(explode(line)));
    in
	if(cond) then (closeIn(is); "") else if(linename = name) then line else getUser(is, name)
    end;

fun splitList ([], y, _) = y
  | splitList((x::[]), y::ys, char) = if(x = char) then rev(y)::ys else splitList([], (rev(x::y) :: ys), char)
  | splitList((x::xs), y::ys, char) = if(x = char) then rev(y)::splitList(xs, []::ys, char) else splitList(xs, ((x::y) :: ys), char)
  | splitList(_,_,_) = raise Domain;

fun returnUser l =
    case (map implode(splitList(explode(l),[[]], #">"))) of
	x::y::z::v::_ => User(x, y, z, valOf(Int.fromString(v)))
      | _ => EmptyUser

fun checkLogin (User(_,y,_,_), input) = y = input
  | checkLogin _ = false

fun readChat chatName = 
    let
	val chatStream = openIn(chatName)
	val totalChat = inputAll(chatStream)
	
	fun readChat'([]) = []
	  | readChat'(x::y::z::xs) = MSG(x, y, z)::readChat'(xs)
	  | readChat'(x::xs) = if x = "\n" then [] else raise Hej 
    in
	(closeIn(chatStream); Chat(chatName, readChat'(map implode(splitList(explode(totalChat), [[]], #">")))))
    end;

fun readMSG(MSG(x, y, z)) = (x ^ " " ^ y ^ ": " ^ z)

fun readMSGS (Chat(name, [])) = ""
  | readMSGS (Chat(name,x::xs)) = readMSG(x) ^ "<br />" ^ readMSGS (Chat(name,xs))
  | readMSGS _ = raise Domain


fun generateChat() =
    let
	val messages = readMSGS(readChat("../webchat/chats/Main.txt"))
    in
	print ("<div style=\"margin:auto;background-color:white; width:500px; height:200px;border:3px #73a40d solid;\">" ^ messages ^ " </div>")
    end;

fun login() =
    let
	val name = getOpt(cgi_field_string("username"), "")
	val password = getOpt(cgi_field_string("password"), "")
	val userList = getUser(openIn "../webchat/users.txt", name) 
	val user = returnUser(userList)
	
	val loginSuccess = EmptyUser <> user andalso checkLogin(user, password)
    in
	if(loginSuccess) then 
	    generateChat()
	else 
	     print ("Wrong username or password :(")
        
    end;

fun signup() =
    let
	val name = getOpt(cgi_field_string("username"), "")
	val password = getOpt(cgi_field_string("password"), "")
        val passwordRepeat = getOpt(cgi_field_string("repeatPassword"), "")
        val successPassword = password = passwordRepeat
			      
			      
    in
	if successPassword then 
	    let
		val successName = getUser(openIn "../webchat/users.txt", name) = ""
		val outStream = openAppend ("../webchat/users.txt")
	    in
		if successName then (output (outStream, name ^ ">" ^ password ^ ">" ^ Date.toString(Date.fromTimeUniv(Time.now())) ^ ">" ^ "0" ^ "\n");                 closeOut(outStream); login())
		else 
		     print ("User already exists")
	    end
	else
	     print ("Passwords do not match")
    end;

fun main() =
    let
	val formType = getOpt(cgi_field_string("formType"), "")
    in
	(print "Content-type: text/html\n\n<html><head><link rel=\"stylesheet\" type=\"text/css\" href=\"http://user.it.uu.se/~daek3938/webchat/styles/styles.css\" /></head><body>";
	(if formType = "login" then login() else signup());
	print "</body></html>")
    end;

val _ = main();
