open Mosmlcgi;
open TextIO;

val cfgStream = openIn("../webchat/url.cfg");
val websiteURL = implode(List.filter (fn x => x <> #"\n") (explode(inputLine(cfgStream))));
val cgiURL = implode(List.filter (fn x => x <> #"\n") (explode(inputLine(cfgStream))));
val a = closeIn(cfgStream);

(* REPRESENTATION CONVENTION: Represents a user of a chat forum
   User(name, password, date, postCount) - A user with name, password, a creation date and a post count
   EmtptyUser - An empty user
	
 * REPRESENTATION INVARIANT: name is unique (no other user can have the same name), length of password ?, postCount >= 0
 *)
datatype user = User of (string * string * string * int) | EmptyUser

(* REPRESENTATION CONVENTION: Represents a message in a chat forum
   MSG(username, postdate, text) - A message written by a user with name username at date postdate with text as the content 
 
 * REPRESENTATION INVARIANT: None 
 *)
datatype message = MSG of (string * string * string)

(* REPRESENTATION CONVENTION: Represents a forum in the web chat
   Chat(name, msgList) - A forum with name and msgList as a list of messages
   EmptyChat - Represents and empty chat with no name
	
 * REPRESENTATION INVARIANT: None
 *)
datatype chat = Chat of (string * message list) | EmptyChat

fun getSmiley (#"P") = "<img class=\"smiley\" src=\"" ^ websiteURL ^ "styles/images/smileys/blub.jpg\" />"
  | getSmiley (#"D") = "<img class=\"smiley\" src=\"" ^ websiteURL ^ "styles/images/smileys/happy.jpg\" />"
  | getSmiley (#")") = "<img class=\"smiley\" src=\"" ^ websiteURL ^ "styles/images/smileys/original.jpg\" />"
  | getSmiley (#"(") = "<img class=\"smiley\" src=\"" ^ websiteURL ^ "styles/images/smileys/sad.jpg\" />"
  | getSmiley (#"O") = "<img class=\"smiley\" src=\"" ^ websiteURL ^ "styles/images/smileys/suprised.jpg\" />"
  | getSmiley (x) = ":" ^ implode([x]) (*Char.toString(x)*)
 
fun insertSmiley [] = ""
  | insertSmiley (x::[]) = implode([x]) (*Char.toString(x)*)
  | insertSmiley ((#":")::(#":")::tail) = ":" ^ insertSmiley(#":"::tail)
  | insertSmiley ((#":")::y::tail) = getSmiley(y) ^ insertSmiley(tail)
  | insertSmiley (x::tail) = implode([x]) ^ insertSmiley(tail)

fun filterChar #"<" = "&lt;"
  | filterChar #"&" = "&amp;"
  | filterChar ch = implode([ch]) (*Char.toString(ch)*);
  
fun filterString(s) = foldr (fn (x,y) => filterChar(x) ^ y) "" (explode(s))
  
(* getName x 
 * TYPE: char list -> char list
 * PRE: none
 * POST: The name from x which is the characters from the start to the first ">" character
 *
 * VARIANT: length of x
 *)
fun getName [] = []
  | getName(x::xs) = if(x = #">") then [] else x::getName(xs);

(* getUser(stream, name) 
 * TYPE: instream * string -> string
 * PRE: none
 * POST: 
 *
 * VARIANT: length of x
 *)
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
  | checkLogin _ = raise Domain

fun readChat chatName = 
    let
	val chatStream = openIn(chatName)
	val totalChat = inputAll(chatStream)
	
	fun readChat'([]) = []
	  | readChat'(x::y::z::xs) = MSG(x, y, z)::readChat'(xs)
	  | readChat'(x::xs) = if x = "\n" then [] else raise Domain 
    in
	(closeIn(chatStream); Chat(chatName, readChat'(map implode(splitList(explode(totalChat), [[]], #"@")))))
    end;

fun readMSG(MSG(x, y, z)) = (x ^ " " ^ y ^ ":<br /><i>" ^ z ^ "</i><br /><div class=\"chatPostLine\"></div>")

fun readMSGS (Chat(name, [])) = ""
  | readMSGS (Chat(name,x::xs)) = readMSG(x) ^ readMSGS (Chat(name,xs))
  | readMSGS _ = raise Domain


fun generateChat(chat,user) =
    let
	val messages = readMSGS(readChat("../webchat/chats/Main.txt"))
	val currentMsgInput = getOpt(cgi_field_string("currentMsgInput"), "")
	val User(userName,_,date,postCount) = user
    in
	print ("<div class=\"chatMainDiv\"><div id=\"chatMessagesDiv\"><h3>"
	^ chat ^ " chat</h3>" 
	^ messages ^ " </div><div id=\"chatListDiv\">Chats<br /></div><br /><div class=\"yourProfileDiv\"><h3>Your profile</h3>Name: " 
	^ userName ^ "<br />Posts: " ^ Int.toString(postCount) ^ "</div><br /><div class=\"writeMessageDiv\"><form name=\"postMessage\" method=\"post\" action=\""
	^ cgiURL ^ "chat.cgi\"><input type=\"text\" name=\"postTextField\" id=\"postTextField\" class=\"postTextField\" value=\""
	^ currentMsgInput ^ "\" onfocus=\"this.value = this.value;\"><input type=\"hidden\" name=\"formType\" value=\"postMessage\"><input type=\"hidden\" name=\"username\" value=\""
	^ userName ^ "\"><button type=\"submit\" name=\"submit\" value=\"post\">Post</button></form></div></div><form name=\"reloadChat\" id=\"reloadChat\" method=\"post\" action=\""
	^ cgiURL ^ "chat.cgi\"><input type=\"hidden\" name=\"formType\" value=\"reloadChat\"><input type=\"hidden\" name=\"username\" value=\""
	^ userName ^ "\"><input type=\"hidden\" name=\"currentMsgInput\" id=\"currentMsgInput\" value=\""
	^ currentMsgInput ^ "\"></form><script src=\""
	^ websiteURL ^ "js/scripts.js\"></script>")
    end;

fun login(user) =
    let
	val name = getOpt(cgi_field_string("username"), "")
	val password = getOpt(cgi_field_string("password"), "")
	
	val loginSuccess = EmptyUser <> user andalso checkLogin(user, password)
    in
	if(loginSuccess) then 
	    generateChat("Main",user)
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
		val date = Date.toString(Date.fromTimeUniv(Time.now()))
	    in
		if successName then (output (outStream, name ^ ">" ^ password ^ ">" ^ date ^ ">" ^ "0" ^ "\n"); closeOut(outStream); login(User(name,password,date,0)))
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
		
fun replaceUserInfo(x::y::z::v::xs, i, name, change) = if x = name then 
		case i of 
		  1 => change::y::z::v::xs (*should not be changed*)
		| 2 => x::change::z::v::xs(*should not be changed*)
		| 3 => x::y::change::v::xs (*should not be changed*)
		| 4 => x::y::z::change::xs
		| _ => raise Domain
	else 
		x::y::z::v::replaceUserInfo(xs, i, name, change)
  | replaceUserInfo(_, _, _, _) = raise Domain
	
	
fun addToPostCount(User(name, pass, date, post)) =
	let
		val userStream = openIn("../webchat/users.txt")
		val usersStringBlown = explode(inputAll(userStream))
		val newText = String.concatWith ">" (replaceUserInfo(map implode (splitList(explode(totalChat), [[]], #">")), 4, name, Int.toString(post+1)))
		val closeStream = closeIn(userStream)
		val openUserStream = openOut("../webchat/users.txt")
	in
		(output(openUserStream, newText); closeOut(openUserStream))
	end
  | addToPostCount(EmptyUser) = raise Domain
	
fun postMessage(user) =
	let
		val message = getOpt(cgi_field_string("postTextField"), "")
		val filteredMsg = filterString(message)
		val smileysAddedMsg = insertSmiley(explode(filteredMsg))
		val userName = getOpt(cgi_field_string("userName"), "")
	in
		(saveMsgToFile(smileysAddedMsg,"Main",userName); addToPostCount(user); generateChat("Main",user))
	end;

fun main() =
    let
	val formType = getOpt(cgi_field_string("formType"), "")
	val name = getOpt(cgi_field_string("username"), "")
	val userList = getUser(openIn "../webchat/users.txt", name) 
	val user = returnUser(userList)
    in
	(print ("Content-type: text/html\n\n<!DOCTYPE html><html><head><meta charset=\"UTF-8\"><link rel=\"stylesheet\" type=\"text/css\" href=\"" ^ websiteURL ^ "styles/styles.css\" /></head><body>");
	print "<div class=\"headerDiv\"></div>";
	(if formType = "login" then login(user) else if formType = "signup" then signup() else if formType="postMessage" then postMessage(user) else if formType="reloadChat" then generateChat("Main",user) else raise Domain);
	print "</body></html>")
    end;

val _ = main();
