open Mosmlcgi;
open TextIO;

val cfgStream = openIn("../webchat/url.cfg");
val websiteURL = implode(List.filter (fn x => x <> #"\n") (explode(inputLine(cfgStream))));
val cgiURL = implode(List.filter (fn x => x <> #"\n") (explode(inputLine(cfgStream))));
val a = closeIn(cfgStream);

exception generalErrorMsg of string

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
  
fun filterString(sList) = foldr (fn (x,y) => filterChar(x) ^ y) "" (explode(sList))
  
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

(* returnUser l 
 * TYPE: char list -> user
 * PRE: none
 * POST: If l is on the format subList1::">"::subList2::">"subList3::">"::(subList4 able to be parsed as integer) + possible extra characters starting with ">" 
		 then User with attributes string of sublist1, string of subList2, string of subList3, integer parsed from string of subList4
         else EmptyUser
 *)
fun returnUser l =
    case (map implode(splitList(explode(l),[[]], #">"))) of
	x::y::z::v::_ => User(x, y, z, valOf(Int.fromString(v)))
      | _ => EmptyUser

(* checkLogin (user, input)
 * TYPE: user * string -> bool
 * PRE: none
 * POST: true if user is on the from user(a, b, c, d) and b = input else false 
 *)	  
fun checkLogin (User(_,y,_,_), input) = y = input
  | checkLogin (EmptyUser, _) = raise generalErrorMsg "Error in function checkLogin"

fun mainChatList(userName) = 
	let
		val inStream = openIn("../webchat/chats/chats.master")
		fun mainChatList'(stream) = 
			let
				val endOfFile = endOfStream(stream)
				val chatNameFromFile = inputLine(inStream)
				val chatName::_ = String.fields (fn x => x = #"\n") chatNameFromFile
			in
				if endOfFile then "" else ("<form method=\"post\" action=\"" ^ cgiURL ^ "chat.cgi\" class=\"chooseChatForm\"><input type=\"hidden\" name=\"chatName\" value=\"" ^ chatName ^ "\"><input type=\"hidden\" name=\"username\" value=\"" ^ userName ^ "\"><input type=\"hidden\" name=\"formType\" value=\"reloadChat\"><button type=\"submit\" value=\"" ^ chatName ^ "\">" ^ chatName ^ "</button></form>" ^ mainChatList'(stream))
			end
	in
		mainChatList'(inStream)
	end;

fun readChat chatName = 
    let
	val chatStream = openIn(chatName)
	val totalChat = inputAll(chatStream)
	
	fun readChat'([]) = []
	  | readChat'(x::y::z::xs) = MSG(x, y, z)::readChat'(xs)
	  | readChat'(x::xs) = if x = "\n" then [] else []
    in
	(closeIn(chatStream); Chat(chatName, readChat'(map implode(splitList(explode(totalChat), [[]], #"@")))))
    end;

fun readMSG(MSG(x, y, z)) = (x ^ " " ^ y ^ ":<br /><i>" ^ z ^ "</i><br /><div class=\"chatPostLine\"></div>")

fun readMSGS (Chat(name, [])) = ""
  | readMSGS (Chat(name,x::xs)) = readMSG(x) ^ readMSGS (Chat(name,xs))
  | readMSGS _ = raise Domain


fun generateChat(chat,User(userName,_,date,postCount)) = 
	let
		val messages = readMSGS(readChat("../webchat/chats/" ^ chat ^ ".txt"))
		val currentMsgInput = getOpt(cgi_field_string("currentMsgInput"), "")
	in
		print ("<div class=\"chatMainDiv\"><div id=\"chatMessagesDiv\"><h3>"
		^ chat ^ " chat</h3>" 
		^ messages ^ " </div><div id=\"chatListDiv\">Chats<br />" ^ mainChatList(userName) ^ "</div><br /><div class=\"createChatDiv\">Create chat<br /></div><br /><div class=\"yourProfileDiv\"><h3>Your profile</h3>Name: " 
		^ userName ^ "<br />Posts: " ^ Int.toString(postCount) ^ "</div><br /><div class=\"writeMessageDiv\"><form name=\"postMessage\" method=\"post\" action=\""
		^ cgiURL ^ "chat.cgi\"><input type=\"text\" name=\"postTextField\" id=\"postTextField\" class=\"postTextField\" value=\""
		^ currentMsgInput ^ "\" onfocus=\"this.value = this.value;\"><input type=\"hidden\" name=\"formType\" value=\"postMessage\"><input type=\"hidden\" name=\"username\" value=\""
		^ userName ^ "\"><input type=\"hidden\" name=\"chatName\" value=\"" ^ chat ^ "\"><button type=\"submit\" name=\"submit\" value=\"post\">Post</button></form></div></div><form name=\"reloadChat\" id=\"reloadChat\" method=\"post\" action=\""
		^ cgiURL ^ "chat.cgi\"><input type=\"hidden\" name=\"formType\" value=\"reloadChat\"><input type=\"hidden\" name=\"username\" value=\""
		^ userName ^ "\"><input type=\"hidden\" name=\"currentMsgInput\" id=\"currentMsgInput\" value=\""
		^ currentMsgInput ^ "\"><input type=\"hidden\" name=\"chatName\" value=\"" ^ chat ^ "\"></form><script src=\""
		^ websiteURL ^ "js/scripts.js\"></script>")
	end
  | generateChat(_,EmptyUser) = raise generalErrorMsg "Error in function generateChat"

fun login(user) =
    let
		val name = getOpt(cgi_field_string("username"), "")
		val password = getOpt(cgi_field_string("password"), "")
		val loginSuccess =  user <> EmptyUser andalso checkLogin(user, password)
    in
		if(loginSuccess) then 
			generateChat("Main",user)
		else 
			print ("Wrong username or password :(")
    end;

fun signup() =
    let
		val password = getOpt(cgi_field_string("password"), "")
		val passwordRepeat = getOpt(cgi_field_string("repeatPassword"), "")
    in
		if password = passwordRepeat then 
			let
				val name = getOpt(cgi_field_string("username"), "")
				val inStream = openIn "../webchat/users.txt"
				val successName = getUser(inStream, name) = ""
				val outStream = (closeIn(inStream); openAppend ("../webchat/users.txt"))
				val date = Date.toString(Date.fromTimeUniv(Time.now()))
			in
				if successName then (output(outStream, name ^ ">" ^ password ^ ">" ^ date ^ ">" ^ "0\n"); closeOut(outStream); login(User(name,password,date,0)))
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
		

fun changeUserField(name, stream, replacement, whichField) = let 
	exception postCountUpdate of string
	val thisLine = inputLine(stream)
	infix 6 >^
	fun x >^ y = x^ ">" ^y
in
	case String.tokens (fn y => #">" = y) thisLine of
		x::y::z::v::[] => if x = name then
				case whichField of 
					4 => x >^ y >^ z >^ replacement ^ "\n" ^ input(stream)
				  |	1 => replacement >^ y >^ z >^ v ^ "\n" ^ input(stream)
				  | 2 => x >^ replacement >^ z >^ v ^ "\n" ^ input(stream)
				  | 3 => x >^ y >^ replacement >^ v ^ "\n" ^ input(stream) 
				  | _ => raise postCountUpdate "whichField must be between 1 and 4"
			else
				thisLine ^ changeUserField(name, stream, replacement, whichField)
	  | [x] => raise postCountUpdate x
	  | [] => raise postCountUpdate "[]"
	  | _ => raise postCountUpdate "unknown error 2-3 elements"
end	
	
fun addToPostCount(User(name, _, _, post)) =
	let
		val userStream = openIn("../webchat/users.txt")
		val newText = changeUserField(name, userStream, Int.toString(post + 1), 4)
		val closeStream = closeIn(userStream)
		val openUserStream = openOut("../webchat/users.txt")
	in
		(output(openUserStream, newText); closeOut(openUserStream))
	end
  | addToPostCount(EmptyUser) = raise generalErrorMsg "Error in function addToPostCount"
	
fun postMessage(user as User(name, pw, date, postCount),chatName) =
	let
		val message = getOpt(cgi_field_string("postTextField"), "")
		val filteredMsg = insertSmiley(explode(filterString(message)))
	in
		(saveMsgToFile(filteredMsg,chatName,name); addToPostCount(user); generateChat(chatName,User(name, pw, date, postCount + 1)))
	end
  | postMessage(EmptyUser,_) = raise generalErrorMsg "Error in function postMessage"

fun main() =
    let
		val formType = getOpt(cgi_field_string("formType"), "")
		val name = getOpt(cgi_field_string("username"), "")
		val chatName = if getOpt(cgi_field_string("chatName"), "") = "" then "Main" else getOpt(cgi_field_string("chatName"), "")
		val userList = getUser(openIn "../webchat/users.txt", name) 
		val user = returnUser(userList)
    in
		(print ("Content-type: text/html\n\n<!DOCTYPE html><html><head><meta charset=\"UTF-8\"><link rel=\"stylesheet\" type=\"text/css\" href=\"" ^ websiteURL ^ "styles/styles.css\" /></head><body>");
		print "<div class=\"headerDiv\"></div>";
		(if formType = "login" then login(user) else if formType = "signup" then signup() else if formType="postMessage" then postMessage(user,chatName) else if formType="reloadChat" then generateChat(chatName,user) else raise generalErrorMsg "Error in function Main");
		print "</body></html>")
    end;

val _ = main();
