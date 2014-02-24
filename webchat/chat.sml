open Mosmlcgi;
open TextIO;

val cfgStream = openIn("../webchat/url.cfg");
val websiteURL = inputLine;
val cgiURL = inputLine;
val a = closeIn(cfgStream);


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
	(closeIn(chatStream); Chat(chatName, readChat'(map implode(splitList(explode(totalChat), [[]], #"@")))))
    end;

fun readMSG(MSG(x, y, z)) = (x ^ " " ^ y ^ ":<br /><i>" ^ z ^ "</i><br /><div class=\"chatPostLine\"></div>")

fun readMSGS (Chat(name, [])) = ""
  | readMSGS (Chat(name,x::xs)) = readMSG(x) ^ readMSGS (Chat(name,xs))
  | readMSGS _ = raise Domain


fun generateChat(chat,name) =
    let
	val messages = readMSGS(readChat("../webchat/chats/Main.txt"))
	val currentMsgInput = getOpt(cgi_field_string("currentMsgInput"), "")
    in
	print ("<div class=\"chatMainDiv\"><div id=\"chatMessagesDiv\"><h3>" ^ chat ^ " chat</h3>" ^ messages ^ " </div><div id=\"chatListDiv\">Chats<br /></div><br /><div class=\"yourProfileDiv\"><h3>Your profile</h3>Name: " ^ name ^ "</div><br /><div class=\"writeMessageDiv\"><form name=\"postMessage\" method=\"post\" action=\"" ^ cgiURL ^ "chat.cgi\"><input type=\"text\" name=\"postTextField\" id=\"postTextField\" class=\"postTextField\" value=\"" ^ currentMsgInput ^ "\" onfocus=\"this.value = this.value;\"><input type=\"hidden\" name=\"formType\" value=\"postMessage\"><input type=\"hidden\" name=\"userName\" value=\"" ^ name ^ "\"><button type=\"submit\" name=\"submit\" value=\"post\">Post</button></form></div></div><form name=\"reloadChat\" id=\"reloadChat\" method=\"post\" action=\"" ^ cgiURL ^ "chat.cgi\"><input type=\"hidden\" name=\"formType\" value=\"reloadChat\"><input type=\"hidden\" name=\"username\" value=\"" ^ name ^ "\"><input type=\"hidden\" name=\"currentMsgInput\" id=\"currentMsgInput\" value=\"" ^ currentMsgInput ^ "\"></form><script src=\"" ^ websiteURL ^ "js/scripts.js\"></script>")
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
	    generateChat("Main",name)
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
		if successName then (output (outStream, name ^ ">" ^ password ^ ">" ^ Date.toString(Date.fromTimeUniv(Time.now())) ^ ">" ^ "0" ^ "\n"); closeOut(outStream); login())
		else 
		     print ("User already exists")
	    end
	else
	     print ("Passwords do not match")
    end;

fun saveMsgToFile (msg,chatName,userName) =
	let
		val outStream = openAppend ("../webchat/chats/" ^ chatName ^ ".txt")
	in
		(output (outStream, userName ^ "@" ^ Date.toString(Date.fromTimeUniv(Time.now())) ^ "@" ^ msg ^ "@"); closeOut (outStream))
	end;
	
fun postMessage() =
	let
		val message = getOpt(cgi_field_string("postTextField"), "")
		val userName = getOpt(cgi_field_string("userName"), "")
	in
		(saveMsgToFile(message,"Main",userName); generateChat("Main",userName))
	end;

fun main() =
    let
	val formType = getOpt(cgi_field_string("formType"), "")
	val name = getOpt(cgi_field_string("username"), "")
    in
	(print ("Content-type: text/html\n\n<!DOCTYPE html><html><head><meta charset=\"UTF-8\"><link rel=\"stylesheet\" type=\"text/css\" href=\"" ^ websiteURL ^ "styles/styles.css\" /></head><body>");
	print "<div class=\"headerDiv\"></div>";
	(if formType = "login" then login() else if formType = "signup" then signup() else if formType="postMessage" then postMessage() else if formType="reloadChat" then generateChat("Main",name) else raise Domain);
	print "</body></html>")
    end;

val _ = main();
