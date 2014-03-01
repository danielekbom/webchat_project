(* 	Om man glömmer:
	cd public_html/cgi-bin/
	mosmlc -o chat.cgi ../webchat/chat.sml
*)

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
	
 * REPRESENTATION INVARIANT: none
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
  | filterChar #"@" = "&#64;"
  | filterChar #">" = "&#62;"
  | filterChar ch = implode([ch]) (*Char.toString(ch)*);
  
fun filterString(sList) = foldr (fn (x,y) => filterChar(x) ^ y) "" (explode(sList))

fun nameToLower name = String.map Char.toLower name
  
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
 * POST: returns the first line in stream that begins with name, if no line begins with name then ""
 *
 * SIDE-EFFECTS: advances the current stream position
 *               closes stream if it is endofstream, freeing associated resources
 *
 * VARIANT: length of stream
 *)
fun getUser(is, name) = 
    let
	val cond = endOfStream(is);
	val line = inputLine(is);
	val linename = implode(getName(explode(line)));
    in
	if(cond) then (closeIn(is); "") else if(nameToLower(linename) = nameToLower(name)) then line else getUser(is, name)
    end;

(* splitList x, y, c
 * TYPE: ''a list * ''a list list * ''a -> ''a list list
 * PRE: none
 * POST: y added with elements from x separated by c... 
 *
 * EXCEPTIONS: raises Domain if...
 *)
fun splitList ([], y, _) = y
  | splitList((x::[]), y::ys, char) = if(x = char) then rev(y)::ys else splitList([], (rev(x::y) :: ys), char)
  | splitList((x::xs), y::ys, char) = if(x = char) then rev(y)::splitList(xs, []::ys, char) else splitList(xs, ((x::y) :: ys), char)
  | splitList(_,_,_) = raise Domain;

(* returnUser l 
 * TYPE: string -> user
 * PRE: none
 * POST: If l is on the format subString1::">"::subString2::">"subString3::">"::(subString4 able to be parsed as integer) + possible extra characters starting with ">" 
		 then User with attributes subString1, subString2, subString3, integer parsed from subString4
         else EmptyUser
 *)
fun returnUser l =
    case (map implode(splitList(explode(l),[[]], #">"))) of
	x::y::z::v::_ => User(x, y, z, valOf(Int.fromString(v)))
      | _ => EmptyUser

fun formatDate date = String.substring(date,4,7) ^ String.substring(date,size(date)-4,4)

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
				if endOfFile then "" else (("<form method=\"post\" action=\"" ^ cgiURL ^ "chat.cgi\" class=\"chooseChatForm\"><input type=\"hidden\" name=\"chatName\" value=\"" ^ chatName ^ "\"><input type=\"hidden\" name=\"username\" value=\"" ^ userName ^ "\"><input type=\"hidden\" name=\"formType\" value=\"reloadChat\"><button type=\"submit\">" ^ chatName ^ "</button></form>") ^ (if userName = "admin" then ("<form method=\"post\" action=\"" ^ cgiURL ^ "chat.cgi\" class=\"clearChatForm\"><input type=\"hidden\" name=\"chatName\" value=\"" ^ chatName ^ "\"><input type=\"hidden\" name=\"username\" value=\"" ^ userName ^ "\"><input type=\"hidden\" name=\"formType\" value=\"clearChat\"><button type=\"submit\"></button></form>") else "") ^ (if userName = "admin" andalso chatName <> "Main" then ("<form method=\"post\" action=\"" ^ cgiURL ^ "chat.cgi\" class=\"deleteChatForm\"><input type=\"hidden\" name=\"chatName\" value=\"" ^ chatName ^ "\"><input type=\"hidden\" name=\"username\" value=\"" ^ userName ^ "\"><input type=\"hidden\" name=\"formType\" value=\"deleteChat\"><button type=\"submit\"></button></form>") else "") ^ mainChatList'(stream))
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

(* readMSG msg
 * TYPE: message -> string
 * PRE: none
 * POST: the attributes from msg added together with proper html code
 *)	
fun readMSG(MSG(x, y, z)) = ("<b>" ^ x ^ "</b>" ^ " " ^ y ^ ":<br /><i>" ^ z ^ "</i><br /><div class=\"chatPostLine\"></div>")

(* readMSGS chat
 * TYPE: chat -> string
 * PRE: none
 * POST: the attributes from msg added together with proper html code
 *
 * EXCEPTIONS: raises Domain if...
 *)	
fun readMSGS (Chat(name, [])) = ""
  | readMSGS (Chat(name,x::xs)) = readMSG(x) ^ readMSGS (Chat(name,xs))
  | readMSGS _ = raise Domain (* Varför raise domain här? Ska vi inte pattern matcha EmptyChat på detta ställe?*)

(* generateChat (chat, user)
 * TYPE: string * user -> ()
 * PRE: none
 * POST: ()
 *
 * SIDE-EFFECTS: Prints html code including the data from the user and the name and messages from the file with name chat
 * EXCEPTIONS: raises generalErrorMsg if user is EmptyUser
 *)	
fun generateChat(chat,User(userName,_,date,postCount)) = 
	let
		val messages = readMSGS(readChat("../webchat/chats/" ^ chat ^ ".txt"))
		val currentMsgInput = getOpt(cgi_field_string("currentMsgInput"), "")
		val formatedDate = formatDate(date)
	in
		print ("<div class=\"chatMainDiv\"><div id=\"chatMessagesDiv\"><h3>"
		^ chat ^ " chat</h3>" 
		^ messages ^ " </div><div id=\"chatListDiv\">Chats<br />" ^ mainChatList(userName) ^ "</div><br /><div class=\"createChatDiv\">Create chat<br /><form action=\"" ^ cgiURL ^ "chat.cgi\" method=\"post\" class=\"createChatForm\"><input type=\"text\" name=\"chatName\" class=\"newChatTextField\"><br /><input type=\"hidden\" name=\"formType\" value=\"createNewChat\"><input type=\"hidden\" name=\"username\" value=\""
		^ userName ^ "\"><button type=\"submit\">Create</button></form></div><br /><div class=\"yourProfileDiv\"><h3>Your profile</h3>Name: " 
		^ userName ^ "<br />Posts: " ^ Int.toString(postCount) ^ "<br />Signup: " ^ formatedDate ^ "</div><br /><div class=\"logoutDiv\"><form action=\"" ^ websiteURL ^ "\" method=\"post\"><button type=\"submit\">Logout</button></form></div><div class=\"writeMessageDiv\"><form name=\"postMessage\" method=\"post\" action=\""
		^ cgiURL ^ "chat.cgi\"><input type=\"text\" onkeyup=\"msgChanged()\" name=\"postTextField\" id=\"postTextField\" class=\"postTextField\" value=\""
		^ currentMsgInput ^ "\" onfocus=\"this.value = this.value;\"><input type=\"hidden\" name=\"formType\" value=\"postMessage\"><input type=\"hidden\" name=\"username\" value=\""
		^ userName ^ "\"><input type=\"hidden\" name=\"chatName\" value=\"" ^ chat ^ "\"><button type=\"submit\" name=\"submit\" value=\"post\" id=\"postSubmit\">Post</button></form></div></div><form name=\"reloadChat\" id=\"reloadChat\" method=\"post\" action=\""
		^ cgiURL ^ "chat.cgi\"><input type=\"hidden\" name=\"formType\" value=\"reloadChat\"><input type=\"hidden\" name=\"username\" value=\""
		^ userName ^ "\"><input type=\"hidden\" name=\"currentMsgInput\" id=\"currentMsgInput\" value=\""
		^ currentMsgInput ^ "\"><input type=\"hidden\" name=\"chatName\" value=\"" ^ chat ^ "\"></form><script src=\""
		^ websiteURL ^ "js/scripts.js\"></script>")
	end
  | generateChat(_,EmptyUser) = raise generalErrorMsg "Error in function generateChat"

(* login user
 * TYPE: user -> ()
 * PRE: none
 * POST: ()
 *
 * SIDE-EFFECTS: Prints html code for the main chat if password from user is equal to the field password else prints a message declaring that username or password is wrong.
 *)
fun login(user) =
    let
		val password = getOpt(cgi_field_string("password"), "")
		val loginSuccess =  user <> EmptyUser andalso checkLogin(user, password)
    in
		if(loginSuccess) then 
			generateChat("Main",user)
		else 
			print ("Wrong username or password :(")
    end;

fun signup(name) =
    let
		val password = getOpt(cgi_field_string("password"), "")
		val passwordRepeat = getOpt(cgi_field_string("repeatPassword"), "")
    in
		if password = passwordRepeat then 
			let
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
		val resizedMsg = if size msg > 1000 then (String.substring(msg,0,999) ^ "...") else msg
	in
		(output (outStream, userName ^ "@" ^ Date.toString(Date.fromTimeUniv(Time.now())) ^ "@" ^ resizedMsg ^ "@"); closeOut (outStream))
	end;
		

(* changeUserFieldInFile(userName, userFileLine, replacement, whichField)
 * TYPE: string * string * string * int -> string
 * PRE: none
 * POST: If userFileLine is in the format subString1 ^ ">" ^ subString2 ^ ">" ^ subString3 ^ ">" ^ subString4 then
			if userName = subString1 then
				replacement as subString(whichField) in subString1 ^ ">" ^ subString2 ^ ">" ^ subString3 ^ ">" ^ subString4
			else
				userFileLine
		else
			raise exception
 * EXCEPTIONS: if not 0 < whichField < 5 then raise postCountUpdate("whichField must be between 1 and 4")
			   if userFileLine is not in the format of subString1 ^ ">" ^ subString2 ^ ">" ^ subString3 ^ ">" ^ subString4 then raise postCountUpdate
 *)
fun changeUserFieldInFile(name, streamLine, replacement, whichField) = let
		exception postCountUpdate of string
		infix 6 >^
		fun x >^ y = x^ ">" ^y
	in
		case String.tokens (fn y => #">" = y) streamLine of
			x::y::z::v::[] => if x = name then
					case whichField of 
						4 => x >^ y >^ z >^ replacement (*when whichField = 4 replacement should end on "\n"*)
					  |	1 => replacement >^ y >^ z >^ v
					  | 2 => x >^ replacement >^ z >^ v
					  | 3 => x >^ y >^ replacement >^ v
					  | _ => raise postCountUpdate "whichField must be between 1 and 4"
				else
					streamLine
		  | [x] => raise postCountUpdate x
		  | [] => raise postCountUpdate "[]"
		  | _ => raise postCountUpdate "unknown error 2-3 elements"
	end

(* changeUserField(userName, userFileStream, replacement, whichField)
 * TYPE: string * TextIO.instream * string * int -> ()
 * PRE: none
 * POST: if userName = (a substring of a line in userFileStream starting from index 0 and ending at the index of the first ">" in the line) then
			The contents of userFileStream as a string where the line matching the above condition named userFileLine equals
			if userFileLine is in the format subString1 ^ ">" ^ subString2 ^ ">" ^ subString3 ^ ">" ^ subString4 then
				replacement as subString[whichField] in subString1 ^ ">" ^ subString2 ^ ">" ^ subString3 ^ ">" ^ subString4
			else postCountUpdate raised by changeUserFieldInFile
		 else postCountUpdate raised by changeUserFieldInFile
		
			
 * VARIANT: lines in UserFileStream
 * SIDE EFFECTS: advances the current stream position
 *)		
fun changeUserField(name, stream, replacement, whichField) =
	let
		val condition = endOfStream(stream)
		val thisLine = inputLine(stream)
		val newField = changeUserFieldInFile(name, thisLine, replacement, whichField)
	in
		if condition then
			""
		else 
			if newField <> thisLine then newField ^ inputAll(stream) else newField ^ changeUserField(name, stream, replacement, whichField)
	end
	
fun addToPostCount(User(name, _, _, post)) =
	let
		val userStream = openIn("../webchat/users.txt")
		val newText = changeUserField(name, userStream, (Int.toString(post + 1) ^ "\n"), 4)
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

fun chatExistsAux (chatName,stream) =
	let
		val condition = endOfStream(stream)
		val thisLine = inputLine(stream)
	in
		if condition then
			false
		else if thisLine = (chatName ^ "\n") then 
			true 
		else chatExistsAux (chatName,stream)
	end;

fun chatExists chatName = 
	let
		val chatStream = openIn("../webchat/chats/chats.master")
	in
		chatExistsAux (chatName,chatStream)
	end;

fun createNewChat (chatName,user) =
	let
		val outStream = openAppend ("../webchat/chats/chats.master")
	in
		(if not(chatExists(chatName)) then (output (outStream,(chatName ^ "\n")); closeOut (outStream); openOut("../webchat/chats/" ^ chatName ^ ".txt"); generateChat(chatName,user)) else print("Chat already exists!"))
	end;
	
fun clearChat (chatName,user) =
	let
		val openChatStream = openOut("../webchat/chats/" ^ chatName ^ ".txt")
	in
		(output(openChatStream,""); closeOut(openChatStream); generateChat(chatName,user))
	end;

fun deleteChatAux (chatName,stream) =
	let
		val condition = endOfStream(stream)
		val thisLine = inputLine(stream)
	in
		if condition then 
			""
		else if thisLine = (chatName ^ "\n") then deleteChatAux(chatName,stream) else thisLine ^ deleteChatAux(chatName,stream) 
	end;

fun deleteChat (chatName,user) =
	let
		val chatStream = openIn("../webchat/chats/chats.master")
		val newText = deleteChatAux(chatName,chatStream)
		val closeStream = closeIn(chatStream)
		val openChatStream = openOut("../webchat/chats/chats.master")
	in
		(output(openChatStream,newText); closeOut(openChatStream); OS.FileSys.remove("../webchat/chats/" ^ chatName ^ ".txt"); generateChat("Main",user))
	end;
	
fun nameToLower name = String.map Char.toLower name
  
fun returnUserName(User(name, _,_,_)) = name
  | returnUserName(_) = ""
   
fun main() =
	let
		val formType = getOpt(cgi_field_string("formType"), "")
		val name = filterString(getOpt(cgi_field_string("username"), ""))
		val lowerName = nameToLower name
		val chatName = if getOpt(cgi_field_string("chatName"), "") = "" then "Main" else getOpt(cgi_field_string("chatName"), "")
		val userList = getUser(openIn "../webchat/users.txt", name)
		val user = returnUser(userList)
		val userName = returnUserName(user)
		(*val user = if userName <> "" then if nameToLower(userName) = lowerName then user else raise generalErrorMsg "user in main function does not match" else user*)
    in
		(print ("Content-type: text/html\n\n<!DOCTYPE html><html><head><meta charset=\"UTF-8\"><link rel=\"stylesheet\" type=\"text/css\" href=\"" ^ websiteURL ^ "styles/styles.css\" /></head><body>");
		print "<div class=\"headerDiv\"></div>";
		(if formType = "login" then login(user) else if formType = "signup" then signup(name) else if formType="postMessage" then postMessage(user,chatName) else if formType="reloadChat" then generateChat(chatName,user) else if formType="createNewChat" then createNewChat(chatName,user) else if formType="clearChat" then clearChat(chatName,user) else if formType="deleteChat" then deleteChat(chatName,user) else raise generalErrorMsg "Error in function Main");
		print "</body></html>")
    end;

val _ = main();
