(* 	Om man glömmer:
	cd public_html/webchat/
	mosmlc rsa.sml chat.sml -o ../cgi-bin/chat.cgi
*)

open Mosmlcgi;
open TextIO;
open rsa;

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
(*
 *  Important user note
 *  Users are saved in the file "webchat/users.txt", we will refer to it as the "users text file".
 *  An EmptyUser is not saved in the users text file
 *  A User(uName, uPassword, uDate, uPostCount) is saved as one line in the users text file.
 *  The form of this is uname ^ ">" ^ uPassword ^ ">" ^ uDate ^ ">" ^ uPostCount ^ "\n"
 *  Example: "daniel>3124823948589>Sun Mar  2 15:01:50 2014>1\nKlas>11044963957762426472653228025194827091584>Sun Mar  2 15:01:50 2014>1"
 *  If the user text file contained this it would contain 2 users, named daniel and Klas.
 *  Henceforth if we are modifying a user in the users text we will refer to this user by its uName.
 *  Example1: This function deletes the user of uName xxxx in the users text file.
 *  What we mean here is that we remove the whole substring "xxxx>uPassword>uDate>uPostCount\n"
 *  Example2: This functions increase the uPostCount of the user xxxxx by 1.
 *  This means that "xxxx>uPassword>uDate>uPostCount\n" the substring uPostCount which will always be a number is increased by 1. The rest of the file remains unchanged
 *  We may also write: we add the User(xxx, yyy, zzz, vvv) to the users text file. This means that we add xxx ^ ">" ^ yyy ^ ">" ^ zzz ^ ">" ^ vvv ^ "\n" to the end of the users text file
*)
datatype user = User of (string * string * string * int) | EmptyUser

(* REPRESENTATION CONVENTION: Represents a message in a chat forum
   MSG(username, postdate, text) - A message written by a user with name username at date postdate with text as the content 
 
 * REPRESENTATION INVARIANT: None 
 *)
(*
 *  Important Message notes
 *  Messages are saved in files called "webchat/chats/xxxxxx.txt", where xxxxxx is a name, we will refer to it as the "chat file xxxxxx", or in general as a "chat file".
 *  A MSG(muName, mPostDate, mText) is stored as a string in a chat file. Multiple messages are stored as message1^message2^message3 in a chat file.
 *  The form of a message is muName ^ "@" ^ mPostDate ^ "@" ^ mText ^ "@"
 *  Example: "daniel@Sun Mar  2 14:06:44 2014@Hej@daniel@Sun Mar  2 14:06:45 2014@då@"
 *  If a chat file contained this it would contain 2 messages, both from the user named daniel.
 *  Henceforth if we are adding a message to a chat file we will write that we add MSG(xxx, yyy, zzz) to a chat file which means we add xxx ^ "@" ^ yyy ^ "@" ^ zzz ^ "@"
 *  Example1: Adds User("daniel", currentTime, "hejjjjjj") to the chat file Main.
 *  What we mean here is that we add the string "daniel" ^ "@" ^ currentTime ^ "@" ^ "hejjjjjj" ^ "@" to the end of of the chat file Main
 *)
datatype message = MSG of (string * string * string)

(* REPRESENTATION CONVENTION: Represents a chat in the web chat
   Chat(cName, msgList) - A chat with name cName and msgList as a list of messages
   EmptyChat - Represents and empty chat with no name
	
 * REPRESENTATION INVARIANT: None
 *)
(*
 *  Important chat note
 *  Chats are saved in the file called "webchat/chats/chats.master", which we will refer to as the "master file".
 *  EmptyChats are not saved in any file.
 *  A Chat(cName, mList) is stored as a string in a the master file and a string in the chat file cName.
 *  cName is stored in the master file and in the form cName ^ "\n". Multiple chats are stored as cName1 ^ "\n" ^ cName2 ^ "\n" ^ cName3 ^ "\n"
 *  Example: "Main\nGames\nIT\n"
 *  mList is stored in the chat file cName as described in the Important Message notes.
 *  Henceforth if we are creating or deleting the Chat(cName, cmList) we will write that we add/remove chat cName to/from the master file 
 *  Example1: Created chat "cars"
 *  What we mean here is that we add the string "cars\n" to the end of of the master file, and also created the chat file "cars.txt"
 *)
datatype chat = Chat of (string * message list) | EmptyChat

(* getSmiley char 
 * TYPE: char -> string
 * PRE: none
 * POST: if char is "P", "D", ")", "(", "O" or "3" then a string of html code for displaying and finding the address for the corresponding jpg image
		 else the string ":" concatenated with char
 *)
fun getSmiley (#"P") = "<img class=\"smiley\" src=\"" ^ websiteURL ^ "styles/images/smileys/blub.jpg\" />"
  | getSmiley (#"D") = "<img class=\"smiley\" src=\"" ^ websiteURL ^ "styles/images/smileys/happy.jpg\" />"
  | getSmiley (#")") = "<img class=\"smiley\" src=\"" ^ websiteURL ^ "styles/images/smileys/original.jpg\" />"
  | getSmiley (#"(") = "<img class=\"smiley\" src=\"" ^ websiteURL ^ "styles/images/smileys/sad.jpg\" />"
  | getSmiley (#"O") = "<img class=\"smiley\" src=\"" ^ websiteURL ^ "styles/images/smileys/suprised.jpg\" />"
  | getSmiley (#"3") = "<img class=\"smiley\" src=\"" ^ websiteURL ^ "styles/images/smileys/heart.jpg\" />"
  | getSmiley (x) = ":" ^ implode([x]) (*Char.toString(x)*)
 
(* insertSmiley x 
 * TYPE: char list -> string
 * PRE: none
 * POST: x as a string with sublists of x such as [#":", #"P"], [#":", #"D"], [#":", #")"], [#":", #"("], [#":", #"O"], [#":", #"3"] replaced with html code for displaying and finding the address for the corresponding jpg image
 *
 * VARIANT: length of x
 *)
fun insertSmiley [] = ""
  | insertSmiley (x::[]) = implode([x]) (*Char.toString(x)*)
  | insertSmiley ((#":")::(#":")::tail) = ":" ^ insertSmiley(#":"::tail)
  | insertSmiley ((#":")::y::tail) = getSmiley(y) ^ insertSmiley(tail)
  | insertSmiley (x::tail) = implode([x]) ^ insertSmiley(tail)

(* filterChar char 
 * TYPE: char -> string
 * PRE: none
 * POST: if char is "<" then "&lt;" 
		 else if char = "&" then "&amp;" 
		 else if char = "@" then "&#64;" 
		 else if char = ">" then "&#62;"
		 else the element of char as a string
 *)
fun filterChar #"<" = "&lt;"
  | filterChar #"&" = "&amp;"
  | filterChar #"@" = "&#64;"
  | filterChar #">" = "&#62;"
  | filterChar ch = implode([ch]) (*Char.toString(ch)*);
 
(* filterString sList 
 * TYPE: string -> string
 * PRE: none
 * POST: sList with the characters "<", "&", "@" and ">" replaced with the strings "&lt;", "&amp;", "&#64;", "&#62;" respectively
 *)
fun filterString(sList) = foldr (fn (x,y) => filterChar(x) ^ y) "" (explode(sList))

(* alphaNumCheck x 
 * TYPE: char list -> bool
 * PRE: none
 * POST: true if every char in x is a letter or digit else false
 *
 * VARIANT: length of x
 *)
fun alphaNumCheck [] = true
  | alphaNumCheck (x::xs) = if (Char.isAlphaNum x) then alphaNumCheck xs else false
 
(* stringSizeCheck s 
 * TYPE: string -> bool
 * PRE: none
 * POST: true if the size of s is greater or equal than 3 and less or equal than 10 else false
 *)
fun stringSizeCheck s = if size s <= 10 andalso size s >= 3 then true else false

(* nameToLower name 
 * TYPE: string -> string
 * PRE: none
 * POST: All of the characters from name but with every alphabetic letter as lowercase
 *)
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

(* formatDate date
 * TYPE: string -> string
 * PRE: none
 * POST: the characters from date in the index intervals 4 - 10 + (length - 4) - length
 *
 * EXAMPLE: formatDate "Fri Mar 21 14:10:45 2014" = "Mar 21 2014"
 *)
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
			in
				if endOfFile then "" else case (String.fields (fn x => x = #"\n") chatNameFromFile) of
					chatName::_ => (("<form method=\"post\" action=\"" ^ cgiURL ^ "chat.cgi\" class=\"chooseChatForm\"><input type=\"hidden\" name=\"chatName\" value=\"" ^ chatName ^ "\"><input type=\"hidden\" name=\"username\" value=\"" ^ userName ^ "\"><input type=\"hidden\" name=\"formType\" value=\"reloadChat\"><button type=\"submit\">" ^ chatName ^ "</button></form>") ^ (if nameToLower(userName) = "admin" then ("<form method=\"post\" action=\"" ^ cgiURL ^ "chat.cgi\" class=\"clearChatForm\"><input type=\"hidden\" name=\"chatName\" value=\"" ^ chatName ^ "\"><input type=\"hidden\" name=\"username\" value=\"" ^ userName ^ "\"><input type=\"hidden\" name=\"formType\" value=\"clearChat\"><button type=\"submit\"></button></form>") else "") ^ (if userName = "admin" andalso chatName <> "Main" then ("<form method=\"post\" action=\"" ^ cgiURL ^ "chat.cgi\" class=\"deleteChatForm\"><input type=\"hidden\" name=\"chatName\" value=\"" ^ chatName ^ "\"><input type=\"hidden\" name=\"username\" value=\"" ^ userName ^ "\"><input type=\"hidden\" name=\"formType\" value=\"deleteChat\"><button type=\"submit\"></button></form>") else "") ^ mainChatList'(stream))
				  | _ => raise generalErrorMsg "function mainChatList Error"
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
		^ messages ^ " </div><div id=\"chatListDiv\">Chats<br />" ^ mainChatList(userName) ^ "</div><br /><div class=\"createChatDiv\">Create chat<br /><form action=\"" 
		^ cgiURL ^ "chat.cgi\" method=\"post\" class=\"createChatForm\"><input type=\"text\" name=\"chatName\" class=\"newChatTextField\"><br /><input type=\"hidden\" name=\"formType\" value=\"createNewChat\"><input type=\"hidden\" name=\"username\" value=\""
		^ userName ^ "\"><button type=\"submit\">Create</button></form></div><br /><div class=\"yourProfileDiv\"><h3>Your profile</h3>Name: " 
		^ userName ^ "<br />Posts: " ^ Int.toString(postCount) ^ "<br />Signup: " ^ formatedDate ^ "</div><br /><div class=\"logoutDiv\"><form action=\"" 
		^ websiteURL ^ "\" method=\"post\"><button type=\"submit\">Logout</button></form></div><div class=\"writeMessageDiv\"><form name=\"postMessage\" method=\"post\" action=\""
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
		val password = encrypt(getOpt(cgi_field_string("password"), ""))
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
		val stringSizeChecked = stringSizeCheck(password) andalso stringSizeCheck(name)
    in
		if password = passwordRepeat then 
			let
				val password = encrypt(password)
				val inStream = openIn "../webchat/users.txt"
				val successName = getUser(inStream, name) = ""
				val outStream = (closeIn(inStream); openAppend ("../webchat/users.txt"))
				val date = Date.toString(Date.fromTimeUniv(Time.now()))
				val nameAlphaNumCheck = (alphaNumCheck(explode(name)) andalso alphaNumCheck(explode(password)))
			in
				if successName andalso nameAlphaNumCheck andalso stringSizeChecked then (output(outStream, name ^ ">" ^ password ^ ">" ^ date ^ ">" ^ "0\n"); closeOut(outStream); login(User(name,password,date,0)))
			else 
				if not(nameAlphaNumCheck) then print("Username/password cantains illegal characters,<br />please use alpha-numeric characters only.") else if not(stringSizeChecked) then print("Username/password must contain between 3 and 10 characters.") else print ("User already exists")
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

(* postMessage(user as User(name, pw, date, postCount), chatName)
 * TYPE: User * string -> ()
 * PRE: none
 * POST: ()
 * SIDE EFFECTS: val message = the value of postTextField in the html script
				 if message <> "" then
					val message = remove illegal chars and replace smiley text with the html code for smiley images for message
					Adds MSG(user, currentDate, message) to the chat chatName
					Increases the uPostCount of user by 1
					
					generateChat(chatName, User(name, pw, date, postCount+1))
				 else
					genereateChat(chatName, user)
 * EXCEPTIONS: if user = EmptyUser then raise generalErrorMsg
 *)  
fun postMessage(user as User(name, pw, date, postCount),chatName) =
	let
		val message = getOpt(cgi_field_string("postTextField"), "")
		val filteredMsg = insertSmiley(explode(filterString(message)))
	in
		(if filteredMsg <> "" then (saveMsgToFile(filteredMsg,chatName,name); addToPostCount(user); generateChat(chatName,User(name, pw, date, postCount + 1))) else generateChat(chatName,user))
	end
  | postMessage(EmptyUser,_) = raise generalErrorMsg "Error in function postMessage"

(* chatExists(chatName, stream)
 * TYPE: string * instream -> bool
 * PRE: none
 * POST: if the chat chatName exists in the master file then true
		 else false
 * SIDE EFFECTS: Advances the stream
				 Closes stream
 *)
fun chatExists (chatName,stream) =
	let
		val condition = endOfStream(stream)
		val thisLine = inputLine(stream)
	in
		if condition then
			(closeIn(stream); false)
		else if thisLine = (chatName ^ "\n") then 
			(closeIn(stream); true )
		else chatExists (chatName,stream)
	end;


(* createNewChat(chatName, user)
 * TYPE: string * User -> ()
 * PRE: none
 * POST: ()
 * SIDE EFFECTS: Opens an outstream to the master file
				 if chatName contains only alphanumeric chars then
					Opens an instream to the master file
					if chat chatName does not exist in the master file then
						Create chat chatName
						Closes the previously opened outstream
						
						generateChat(chatName, user)
					else
						prints "Chat already exists!"
				 else
					 Closes the previously opened outstream
					 prints "Chatname cantains illegal characters,<br />please use alpha-numeric characters only."
 *) 
fun createNewChat (chatName,user) =
	let
		val outStream = openAppend ("../webchat/chats/chats.master")
		val chatAlphaNumCheck = alphaNumCheck(explode(chatName))
	in
		if chatAlphaNumCheck then
			(if not(chatExists(chatName, openIn("../webchat/chats/chats.master"))) then (output (outStream,(chatName ^ "\n")); closeOut (outStream); openOut("../webchat/chats/" ^ chatName ^ ".txt"); generateChat(chatName,user)) else print("Chat already exists!"))
		else
			(closeOut(outStream); print("Chatname cantains illegal characters,<br />please use alpha-numeric characters only."))
	end;

(* clearChat(chatName, user)
 * TYPE: string * User -> ()
 * PRE: none
 * POST: ()
 * SIDE EFFECTS: Opens an outstream to the chat chatName
				 Outputs "" to the previously opened outstream
				 Closes the previously opened outstream
				 
				 generateChat(chatName, user)
 *)
fun clearChat (chatName,user) =
	let
		val openChatStream = openOut("../webchat/chats/" ^ chatName ^ ".txt")
	in
		(output(openChatStream,""); closeOut(openChatStream); generateChat(chatName,user))
	end;

(* deleteChatAux(chatName, stream)
 * TYPE: string * instream -> string
 * PRE: none
 * POST: The contents of stream with the line equal to chatName ^ "\n" removed
 * SIDE EFFECTS: Advances stream
 * VARIANT: lines in stream
 *)
fun deleteChatAux (chatName,stream) =
	let
		val condition = endOfStream(stream)
		val thisLine = inputLine(stream)
	in
		if condition then 
			""
		else if thisLine = (chatName ^ "\n") then deleteChatAux(chatName,stream) else thisLine ^ deleteChatAux(chatName,stream) 
	end;

(* deleteChat(chatName, user)
 * TYPE: string * User -> ()
 * PRE: a file named chatName ^ .txt exists in ../webchat/chats
 * POST: ()
 * SIDE EFFECTS: Opens an instream to the master file
				 Closes the previously opened instream
				 Opens an outstream from the master file
				 Deletes chat chatName
				 Closes the previously opened outStream
				 
				 generateChat("Main", user)
 *)
fun deleteChat (chatName,user) =
	let
		val chatStream = openIn("../webchat/chats/chats.master")
		val newText = deleteChatAux(chatName,chatStream)
		val closeStream = closeIn(chatStream)
		val openChatStream = openOut("../webchat/chats/chats.master")
	in
		(output(openChatStream,newText); closeOut(openChatStream); OS.FileSys.remove("../webchat/chats/" ^ chatName ^ ".txt"); generateChat("Main",user))
	end;

(* nameToLower(name)
 * TYPE: string -> string
 * PRE: none
 * POST: name where all alphabetical chars are uppercase.
 *)	
fun nameToLower name = String.map Char.toLower name
   
fun main() =
	let
		val formType = getOpt(cgi_field_string("formType"), "")
		val name = filterString(getOpt(cgi_field_string("username"), ""))
		val lowerName = nameToLower name
		val chatName = if getOpt(cgi_field_string("chatName"), "") = "" then "Main" else getOpt(cgi_field_string("chatName"), "")
		val userList = getUser(openIn "../webchat/users.txt", name)
		val user = returnUser(userList)
    in
		(print ("Content-type: text/html\n\n<!DOCTYPE html><html><head><meta charset=\"UTF-8\"><link rel=\"stylesheet\" type=\"text/css\" href=\"" ^ websiteURL ^ "styles/styles.css\" /></head><body>");
		print "<div class=\"headerDiv\"></div>";
		(if formType = "login" then login(user) else if formType = "signup" then signup(name) else if formType="postMessage" then postMessage(user,chatName) else if formType="reloadChat" then generateChat(chatName,user) else if formType="createNewChat" then createNewChat(chatName,user) else if formType="clearChat" then clearChat(chatName,user) else if formType="deleteChat" then deleteChat(chatName,user) else raise generalErrorMsg "Error in function Main");
		print "</body></html>")
    end;

val _ = main();