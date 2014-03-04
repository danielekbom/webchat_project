(* 	Om man glömmer:
	cd public_html/webchat/
	mosmlc rsa.sml chat.sml -o ../cgi-bin/chat.cgi
*)

load "IntInf";
load "Mosmlcgi";
load "Date";
load "OS";

open Mosmlcgi;
open TextIO;

(* The following four lines gets the website URL's from the url.cfg file *)
val cfgStream = openIn("../webchat/url.cfg");
val websiteURL = implode(List.filter (fn x => x <> #"\n") (explode(inputLine(cfgStream))));
val cgiURL = implode(List.filter (fn x => x <> #"\n") (explode(inputLine(cfgStream))));
val cfgStream = closeIn(cfgStream);

exception generalErrorMsg of string

(* REPRESENTATION CONVENTION: Represents a user of a chat forum
   User(name, password, date, postCount) - A user with a name, a password, a creation date and a post count
   EmtptyUser - An empty user
	
 * REPRESENTATION INVARIANT: none
 *)
(*
 *  Important user note
 *  Users are saved in the file "webchat/users.txt", we will refer to it as the "users text file".
 *  An EmptyUser is not saved in the users text file
 *  A User(uName, uPassword, uDate, uPostCount) is saved as one line in the users text file.
 *  The form of this is uname ^ ">" ^ uPassword ^ ">" ^ uDate ^ ">" ^ uPostCount ^ "\n", we will call this a user string.
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

(* REPRESENTATION CONVENTION: Represents a message in a chatroom
   MSG(username, postdate, text) - A message written by a user with name username at date postdate with text as the content 
 
 * REPRESENTATION INVARIANT: None 
 *)
(*
 *  Important Message notes
 *  Messages are saved in files called "webchat/chats/xxxxxx.txt", where xxxxxx is a name, we will refer to it as the "chat file xxxxxx", or in general as a "chat file".
 *  A MSG(muName, mPostDate, mText) is stored as a string in a chat file. Multiple messages are stored as message1^message2^message3 in a chat file.
 *  The form of a message is muName ^ "@" ^ mPostDate ^ "@" ^ mText ^ "@", we will call this a message string
 *  Example: "daniel@Sun Mar  2 14:06:44 2014@Hej@daniel@Sun Mar  2 14:06:45 2014@då@"
 *  If a chat file contained this it would contain 2 messages, both from the user named daniel.
 *  Henceforth if we are adding a message to a chat file we will write that we add MSG(xxx, yyy, zzz) to a chat file which means we add xxx ^ "@" ^ yyy ^ "@" ^ zzz ^ "@"
 *  Example1: Adds User("daniel", currentTime, "hejjjjjj") to the chat file Main.
 *	currentTime is the current servertime as a string.
 *  What we mean here is that we add the string "daniel" ^ "@" ^ currentTime ^ "@" ^ "hejjjjjj" ^ "@" to the end of of the chat file Main
 *)
datatype message = MSG of (string * string * string)

(* REPRESENTATION CONVENTION: Represents a chat in the web chat
   Chat(cName, msgList) - A chat with name cName and msgList as a list of messages
   EmptyChat - Represents an empty chat with no name
	
 * REPRESENTATION INVARIANT: None
 *)
(*
 *  Important chat notes
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
 * PRE: true
 * POST: pass with every char replaced by their ascii value
 * EXAMPLE: convertPassword("abc123") = 979899495051
 *)		
fun convertPassword pass = foldr (fn (x,y) => Int.toString(ord(x)) ^ y) "" (explode(pass))

(* encrypt(input)
 * TYPE: string -> string
 * PRE: size input > 0 & size input <= 10
 * POST: input as an encrypted string using a simplified version of the RSA algorithm with fixed prime numbers p and q and lowest possible value on e.
		 This means that input will always be encrypted to the same number regardless of how many times the encrypt function is called.
         The encryption is one-way in the sense that no decryption key is generated (and also not needed).
 * EXAMPLE: encrypt "hej" = "132491313371946567748579378563577805694669051416254891136"
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

(* getSmiley char 
 * TYPE: char -> string
 * PRE: true
 * POST: if char is "P", "D", ")", "(", "O" or "3" then a string of html code for displaying and finding the address for the corresponding jpg image
		 else the string ":" concatenated with char
 * EXAMPLE: getSmiley (#"P") = "<img class=\"smiley\" src=\"http://user.it.uu.se/~osah7839/webchat/styles/images/smileys/blub.jpg\" />"
 * EXAMPLE: getSmiley (#"A") = ":A"
 *)
fun getSmiley (#"P") = "<img class=\"smiley\" src=\"" ^ websiteURL ^ "styles/images/smileys/blub.jpg\" />"
  | getSmiley (#"D") = "<img class=\"smiley\" src=\"" ^ websiteURL ^ "styles/images/smileys/happy.jpg\" />"
  | getSmiley (#")") = "<img class=\"smiley\" src=\"" ^ websiteURL ^ "styles/images/smileys/original.jpg\" />"
  | getSmiley (#"(") = "<img class=\"smiley\" src=\"" ^ websiteURL ^ "styles/images/smileys/sad.jpg\" />"
  | getSmiley (#"O") = "<img class=\"smiley\" src=\"" ^ websiteURL ^ "styles/images/smileys/suprised.jpg\" />"
  | getSmiley (#"3") = "<img class=\"smiley\" src=\"" ^ websiteURL ^ "styles/images/smileys/heart.jpg\" />"
  | getSmiley (x) = ":" ^ implode([x])
 
(* insertSmiley x 
 * TYPE: char list -> string
 * PRE: true
 * POST: x as a string with sublists of x such as [#":", #"P"], [#":", #"D"], [#":", #")"], [#":", #"("], [#":", #"O"], [#":", #"3"] replaced with html code for displaying and finding the address for the corresponding jpg image
 * EXAMPLE: insertSmiley [#"h",#"e",#"j",#" ",#":",#"P"] = "hej <img class=\"smiley\" src=\"http://user.it.uu.se/~osah7839/webchat/styles/images/smileys/blub.jpg\" />"
 * VARIANT: length of x
 *)
fun insertSmiley [] = ""
  | insertSmiley (x::[]) = implode([x])
  | insertSmiley ((#":")::(#":")::tail) = ":" ^ insertSmiley(#":"::tail)
  | insertSmiley ((#":")::y::tail) = getSmiley(y) ^ insertSmiley(tail)
  | insertSmiley (x::tail) = implode([x]) ^ insertSmiley(tail)

(* filterChar char 
 * TYPE: char -> string
 * PRE: true
 * POST: if char is "<" then "&lt;" 
		 else if char = "&" then "&amp;" 
		 else if char = "@" then "&#64;" 
		 else if char = ">" then "&#62;"
		 else the element of char as a string
 * EXAMPLE: filterChar #"<" = "&lt;"
 * EXAMPLE: filterChar #"a" = "a"
 *)
fun filterChar #"<" = "&lt;"
  | filterChar #"&" = "&amp;"
  | filterChar #"@" = "&#64;"
  | filterChar #">" = "&#62;"
  | filterChar ch = implode([ch]);
 
(* filterString sList 
 * TYPE: string -> string
 * PRE: none
 * POST: sList with the characters "<", "&", "@" and ">" replaced with the strings "&lt;", "&amp;", "&#64;", "&#62;" respectively
 * EXAMPLE: filterString "daniel&oscar<@>" = "daniel&anp;oscar&lt;&#64;&#62;"
 *)
fun filterString(sList) = foldr (fn (x,y) => filterChar(x) ^ y) "" (explode(sList))

(* alphaNumCheck x 
 * TYPE: char list -> bool
 * PRE: none
 * POST: true if every char in x is a letter or digit else false
 * EXAMPLE: alphaNumCheck [#"h",#"!"] = false
 * EXAMPLE: alphaNumCheck [#"h",#"e",#"j"] = true
 * VARIANT: length of x
 *)
fun alphaNumCheck [] = true
  | alphaNumCheck (x::xs) = if (Char.isAlphaNum x) then alphaNumCheck xs else false
 
(* stringSizeCheck s 
 * TYPE: string -> bool
 * PRE: none
 * POST: if the size of s >= 3 and size s <= 10 then true else false
 * EXAMPLE: stringSizeCheck "ab" = false
 * EXAMPLE: stringSizeCheck "abc" = true
 *)
fun stringSizeCheck s = size s <= 10 andalso size s >= 3

(* nameToLower name 
 * TYPE: string -> string
 * PRE: none
 * POST: All of the characters from name but with every alphabetic letter as lowercase
 * EXAMPLE: nameToLower "HEJ" = "hej"
 *)
fun nameToLower name = String.map Char.toLower name
  
(* getName x 
 * TYPE: char list -> char list
 * PRE: none
 * POST: The name from x which is the characters from the start to the first ">" character, if no ">" exists then x
 * EXAMPLE: getName [#"d", #"a", #"n", #"i", #"e", #"l", #">", #"o", #"s", #"c"] = [#"d", #"a", #"n", #"i", #"e", #"l"]
 * EXAMPLE: getName getName [#"d", #"a", #"n", #"i", #"e", #"l"] = [#"d", #"a", #"n", #"i", #"e", #"l"]
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
 *               closes stream
 * EXAMPLE: getUser (openString("../webchat/users.txt"),"admin") = "admin>7287013848011365457336734959811416142851>Sun Mar  2 13:50:27 2014>" ^ x, where x is a string representing the current postcount for user admin
 * VARIANT: length of stream
 *)
fun getUser(is, name) = 
    let
	val cond = endOfStream(is);
	val line = inputLine(is);
	val linename = implode(getName(explode(line)));
    in
	if(cond) then (closeIn(is); "") else if(nameToLower(linename) = nameToLower(name)) then (closeIn(is); line) else getUser(is, name)
    end;

(* splitList (x, y, c)
 * TYPE: ''a list * ''a list list * ''a -> ''a list list
 * PRE: y may not be empty when x isn't empty
 * POST: if the first element of x = c then a list starting with the reverse ordered first element of y followed by lists of x divided by and not including elements of x = c, followed by the rest of the elements in y
		 else a list starting with the reverse ordered first element of y followed by elements of x ending when an element of x = c, followed by lists of x divided by and not including elements of x = c, followed by the rest of the elements in y
 * EXAMPLE: splitList([1,2,3],[[]],2) = [[1], [3]]
 * EXAMPLE: splitList([1,2,3],[[4,5,6],[7,8,9]],1) = [[6, 5, 4], [2, 3], [7, 8, 9]]
 * EXAMPLE: splitList([1,2,3],[[]],1) = [[], [2, 3]]
 * EXCEPTIONS: if y is empty but x isn't then raise Domain 
 *)
fun splitList ([], y, _) = y
  | splitList((x::[]), y::ys, char) = if(x = char) then rev(y)::ys else splitList([], (rev(x::y) :: ys), char)
  | splitList((x::xs), y::ys, char) = if(x = char) then rev(y)::splitList(xs, []::ys, char) else splitList(xs, ((x::y) :: ys), char)
  | splitList(_,_,_) = raise Domain;

(* returnUser userString
 * TYPE: string -> user
 * PRE: none
 * POST: If userString is in the format subString1 ^ ">" ^ subString2 ^ ">" ^ subString3 ^ ">" ^ (subString4 able to be parsed as integer) ^ "\n" (^ the rest of userString)
		 then User(subString1, subString2, subString3, integer parsed from subString4)
         else EmptyUser
 * EXAMPLE: returnUser "daniel>hej>tjena>1\n lsdkflksdjksjdfdjk" = User("daniel","hej","tjena",1)
 * EXAMPLE: returnUser "daniel>hej" = EmptyUser
 *)
fun returnUser l =
    case (map implode(splitList(explode(l),[[]], #">"))) of
	x::y::z::v::_ => User(x, y, z, valOf(Int.fromString(v)))
      | _ => EmptyUser

(* formatDate date
 * TYPE: string -> string
 * PRE: size string > 10
 * POST: the characters from date in the index interval (4 -> 10) and interval ((length - 4) -> length)
 * EXAMPLE: formatDate "Fri Mar 21 14:10:45 2014" = "Mar 21 2014"
 * EXAMPLE: formatDate "Hej jag heter janne" = "jag hetanne"
 *)
fun formatDate date = String.substring(date,4,7) ^ String.substring(date,size(date)-4,4)

(* checkLogin (user, input)
 * TYPE: user * string -> bool
 * PRE: user <> EmptyUser
 * POST: true if user is on the form User(a, b, c, d) and b = input else false
 * EXAMPLE: checkLogin(User(_,"hej",_,_),"hej") = true
 * EXAMPLE: checkLogin(User(_,"hej",_,_),"tjena") = false
 *)	  
fun checkLogin (User(_,y,_,_), input) = y = input
  | checkLogin (EmptyUser, _) = raise generalErrorMsg "Error in function checkLogin"

(* mainChatList(userName)
 * TYPE: string -> string
 * PRE: true
 * POST: for every line in 'is', where 'is' is the instream opened by the function, the line embedded in a html form, where the line of 'is' is the value of the form submit button. 
			The form also contains a hidden field with the name "formType" and value "reloadChat".
		 if userName = "admin" then another html form is concatenated to the previous form for every line in is.
			The form also contains a hidden field with the name "formType" and value "clearChatForm".
		 if userName = "admin" and the line of the stream <> "Main" then an additional html form is concatenated for every line in is.
			The form also contains a hidden field with the name "formType" and value "deleteChat".
		 The action value of all forms is the destination of the chat.cgi file. All forms also contain two hidden fields with the values chatName and userName.
 * SIDE EFFECTS: open instream 'is'
 *)	  
fun mainChatList(userName) = 
	let
		val inStream = openIn("../webchat/chats/chats.master")
		(* mainChatList'(is)
		 * TYPE: instream -> string
		 * PRE: if not 'is' is at the end of stream, then 'is' must contain the char \n at least once
		 * POST: for every line in 'is', the line embedded in a html form, where the line of 'is' is the value of the form submit button. 
					The form also contains a hidden field with the name "formType" and value "reloadChat".
				 if userName = "admin" then another html form is concatenated to the previous form for every line in is.
					The form also contains a hidden field with the name "formType" and value "clearChatForm".
				 if userName = "admin" and the line of the stream <> "Main" then an additional html form is concatenated for every line in is.
					The form also contains a hidden field with the name "formType" and value "deleteChat".
				The action value of all forms is the destination of the chat.cgi file. All forms also contain two hidden fields with the values chatName and userName.
		 * SIDE EFFECTS: closes the stream 'is'
		 * EXCEPTIONS: raises generalErrorMsg if last line if 'is' is not equal to string "\n"
		 *)	
		fun mainChatList'(stream) = 
			let
				val endOfFile = endOfStream(stream)
				val chatNameFromFile = inputLine(inStream)
			in
				if endOfFile then (closeIn(stream); "") else 
					case (String.fields (fn x => x = #"\n") chatNameFromFile) of
						chatName::_ => 
						let 
							val formHiddenFields = "<input type=\"hidden\" name=\"chatName\" value=\"" ^ chatName ^ "\"><input type=\"hidden\" name=\"username\" value=\"" ^ userName ^ "\">"
						in
							((
								"<form method=\"post\" action=\"" ^ cgiURL ^ "chat.cgi\" class=\"chooseChatForm\">" ^ formHiddenFields ^
									"<input type=\"hidden\" name=\"formType\" value=\"reloadChat\">" ^
									"<button type=\"submit\">" ^ chatName ^ "</button>" ^
								"</form>") ^ 
							(if nameToLower(userName) = "admin" then (
								"<form method=\"post\" action=\"" ^ cgiURL ^ "chat.cgi\" class=\"clearChatForm\">" ^ formHiddenFields ^
									"<input type=\"hidden\" name=\"formType\" value=\"clearChat\">" ^
									"<button type=\"submit\"></button>" ^
								"</form>") 
							else "") ^ 
							(if userName = "admin" andalso chatName <> "Main" then (
								"<form method=\"post\" action=\"" ^ cgiURL ^ "chat.cgi\" class=\"deleteChatForm\">" ^ formHiddenFields ^
									"<input type=\"hidden\" name=\"formType\" value=\"deleteChat\">" ^
									"<button type=\"submit\"></button>" ^
								"</form>") 
							else "") ^ mainChatList'(stream))
						end
						| _ => raise generalErrorMsg "function mainChatList Error"
			end
	in
		mainChatList'(inStream)
	end;
	
(* readChat chatName
 * TYPE: string -> chat
 * PRE: chat file chatName exists and is a chat file
 * POST: Chat(chatName, (a message list where every element is a message string from chat file chatName)
 * SIDE EFFECTS: opens instream to chat file chatName.
				 advances instream.
				 closes previously instream.
 *)
fun readChat chatName = 
    let
		val chatStream = openIn(chatName)
		val totalChat = inputAll(chatStream)
		val _ = closeIn(chatStream)
		
		(* readChat' msgList
		 * TYPE: string list -> message list
		 * PRE: true
		 * POST: if length(msgList) >= 3 then a list of MSG(x,y,z) where x,y,z are 3 consecutive elements in msgList. When there are less than 3 elements in msgList remaining nothing more is added
				 else []
		 * VARIANT: length msgList
		 *)
		fun readChat'([]) = []
		  | readChat'(x::y::z::xs) = MSG(x, y, z)::readChat'(xs)
		  | readChat'(x::xs) = []
    in
		Chat(chatName, readChat'(map implode(splitList(explode(totalChat), [[]], #"@"))))
    end;

(* readMSG msg
 * TYPE: message -> string
 * PRE: none
 * POST: the constructors of msg added together with proper html code
 * EXAMPLE: readMSG(MSG("Hej","pa","dej")) = "<b>Hej</b> pa:<br /><i>dej</i><br /><div class=\"chatPostLine\"></div>"
 *)	
fun readMSG(MSG(x, y, z)) = ("<b>" ^ x ^ "</b>" ^ " " ^ y ^ ":<br /><i>" ^ z ^ "</i><br /><div class=\"chatPostLine\"></div>")

(* readMSGS(chat as Chat(_, msgList))
 * TYPE: chat -> string
 * PRE: chat <> EmptyChat
 * POST: the constructors of every message in msgList added together with proper html code
 * EXAMPLE: readMSGS(Chat(_,[MSG("Hej","pa","dej"),MSG("tja","pa","er")])) = "<b>Hej</b> pa:<br /><i>dej</i><br /><div class=\"chatPostLine\"></div><b>tja</b> pa:<br /><i>er</i><br /><div class=\"chatPostLine\"></div>"
 * VARIANT: length msgList
 * EXCEPTIONS: raises generalErrorMsg if chat = EmptyChat
 *)	
fun readMSGS (Chat(name, [])) = ""
  | readMSGS (Chat(name,x::xs)) = readMSG(x) ^ readMSGS (Chat(name,xs))
  | readMSGS _ = raise generalErrorMsg "Error in function readMSGS"

(* generateChat (chatName, user)
 * TYPE: string * user -> unit
 * PRE: user <> EmptyUser
 * POST: ()
 * SIDE-EFFECTS: Prints html code including the data from the user and the name and messages from chatName chat file.
 * EXCEPTIONS: raises generalErrorMsg if user is EmptyUser
 *)	
fun generateChat(chat,User(userName,_,date,postCount)) = 
	let
		val messages = readMSGS(readChat("../webchat/chats/" ^ chat ^ ".txt"))
		val currentMsgInput = getOpt(cgi_field_string("currentMsgInput"), "")
		val formatedDate = formatDate(date)
	in
		print (
		"<div class=\"chatMainDiv\">" ^
			"<div id=\"chatMessagesDiv\"><h3>" ^ chat ^ " chat</h3>" ^ messages ^ " </div>" ^
			"<div id=\"chatListDiv\">Chats<br />" ^ mainChatList(userName) ^ "</div><br />" ^
			"<div class=\"createChatDiv\">Create chat<br />" ^
				"<form action=\"" ^ cgiURL ^ "chat.cgi\" method=\"post\" class=\"createChatForm\">" ^
					"<input type=\"text\" name=\"chatName\" class=\"newChatTextField\"><br />" ^
					"<input type=\"hidden\" name=\"formType\" value=\"createNewChat\">" ^
					"<input type=\"hidden\" name=\"username\" value=\"" ^ userName ^ "\">" ^
					"<button type=\"submit\">Create</button>" ^
				"</form>" ^
			"</div><br />" ^
			"<div class=\"yourProfileDiv\"><h3>Your profile</h3>Name: " ^ userName ^ "<br />Posts: " ^ Int.toString(postCount) ^ "<br />Signup: " ^ formatedDate ^ "</div><br />" ^
			"<div class=\"logoutDiv\">" ^
				"<form action=\"" ^ websiteURL ^ "\" method=\"post\">" ^
					"<button type=\"submit\">Logout</button>" ^
				"</form>" ^
			"</div>" ^
			"<div class=\"writeMessageDiv\">" ^
				"<form name=\"postMessage\" method=\"post\" action=\"" ^ cgiURL ^ "chat.cgi\">" ^
					"<input type=\"text\" onkeyup=\"msgChanged()\" name=\"postTextField\" id=\"postTextField\" class=\"postTextField\" value=\"" ^ currentMsgInput ^ "\" onfocus=\"this.value = this.value;\">" ^
					"<input type=\"hidden\" name=\"formType\" value=\"postMessage\">" ^
					"<input type=\"hidden\" name=\"username\" value=\"" ^ userName ^ "\">" ^
					"<input type=\"hidden\" name=\"chatName\" value=\"" ^ chat ^ "\">" ^
					"<button type=\"submit\" name=\"submit\" value=\"post\" id=\"postSubmit\">Post</button>" ^
				"</form>" ^
			"</div>" ^
		"</div>" ^
		"<form name=\"reloadChat\" id=\"reloadChat\" method=\"post\" action=\"" ^ cgiURL ^ "chat.cgi\">" ^
			"<input type=\"hidden\" name=\"formType\" value=\"reloadChat\">" ^
			"<input type=\"hidden\" name=\"username\" value=\"" ^ userName ^ "\">" ^
			"<input type=\"hidden\" name=\"currentMsgInput\" id=\"currentMsgInput\" value=\"" ^ currentMsgInput ^ "\">" ^
			"<input type=\"hidden\" name=\"chatName\" value=\"" ^ chat ^ "\">" ^
		"</form>" ^
		"<script src=\"" ^ websiteURL ^ "js/scripts.js\"></script>")
	end
  | generateChat(_,EmptyUser) = raise generalErrorMsg "Error in function generateChat"

(* login(user as User(name, pw, _, _), password)
 * TYPE: user * string -> unit
 * PRE: none
 * POST: ()
 *
 * SIDE-EFFECTS: if user <> EmptyUser and pw = password then
					Prints html code including the data from the user and the name and messages from chat file "Main"
				 else prints a message declaring that username or password is wrong.
 *)
fun login(user, password) =
    let
		val password = if password = "" then "" else encrypt(password)
		val loginSuccess =  user <> EmptyUser andalso checkLogin(user, password)
    in
		if(loginSuccess) then 
			generateChat("Main",user)
		else 
			print ("Wrong username or password :(")
    end;

(* signup(userName, pw, repeatPw)
 * TYPE: string * string * string -> unit
 * PRE: none
 * POST: ()
 * SIDE-EFFECTS: if pw <> repeatPw then prints "Username/password cantains illegal characters,<br />please use alpha-numeric characters only."
				 else if size of password and name are not in the interval [3, 10] then prints "Username/password must contain between 3 and 10 characters."
				 else if a user with uName = userName in the users text file exists then prints "User already exists"
				 else a user User(username, encrypt(pw), currentTime, 0) is saved in the users text file and then prints html code including the data from the user and the name and messages from chat file "Main".
 *)
fun signup(name, password, passwordRepeat) =
    let
		val stringSizeChecked = stringSizeCheck(password) andalso stringSizeCheck(name)
    in
		if password = passwordRepeat then 
			let
				val password = encrypt(password)
				val inStream = openIn "../webchat/users.txt"
				val successName = getUser(inStream, name) = ""
				val outStream = openAppend("../webchat/users.txt")
				val date = Date.toString(Date.fromTimeUniv(Time.now()))
				val nameAlphaNumCheck = (alphaNumCheck(explode(name)) andalso alphaNumCheck(explode(password)))
			in
				if successName andalso nameAlphaNumCheck andalso stringSizeChecked then (output(outStream, name ^ ">" ^ password ^ ">" ^ date ^ ">" ^ "0\n"); closeOut(outStream); login(User(name,password,date,0), passwordRepeat))
			else 
				if not(nameAlphaNumCheck) then print("Username/password cantains illegal characters,<br />please use alpha-numeric characters only.") else if not(stringSizeChecked) then print("Username/password must contain between 3 and 10 characters.") else print ("User already exists")
			end
		else
			 print ("Passwords do not match")
    end;

	
(* saveMsgToFile(message,chatName,userName)
 * TYPE: string * string * string -> unit
 * PRE: true
 * POST: ()
 * SIDE EFFECTS: Adds MSG(userName, current servertime as a string, if the size of message > 1000 then (the first 1000 chars of message) ^ "..." else message) to chatName chat file
 *)
fun saveMsgToFile(msg,chatName,userName) =
	let
		val outStream = openAppend ("../webchat/chats/" ^ chatName ^ ".txt")
		val resizedMsg = if size msg > 1000 then (String.substring(msg,0,999) ^ "...") else msg
	in
		(output(outStream, userName ^ "@" ^ Date.toString(Date.fromTimeUniv(Time.now())) ^ "@" ^ resizedMsg ^ "@"); closeOut(outStream))
	end;
	
(* changeUserFieldInFile(userName, userFileLine, replacement, whichField)
 * TYPE: string * string * string * int -> string
 * PRE: userFileLine is in the form of a user string as saved in the users text file and the statement (0 < whichField < 5) is true
 * POST:if userName = uName of userFileLine then
			if whichField = 1 then userFileLine with uName replaced by replacement
			if whichField = 2 then userFileLine with uPassword replaced by replacement
			if whichField = 3 then userFileLine with uDate replaced by replacement
			if whichField = 4 then userFileLine with uPostCount replaced by replacement
		else
			userFileLine
 * EXAMPLE: changeUserFieldInFile("Daniel","Daniel>348934853498>hej>2\n","bertil",3) = "Daniel>348934853498>bertil>2\n"
 * EXAMPLE: changeUserFieldInFile("Daniel","Oscar>348934853498>hej>2\n","bertil",3) = "Oscar>348934853498>hej>2\n"
 * EXCEPTIONS: if not(0 < whichField < 5) then raise postCountUpdate
			   if userFileLine is not in the format of a user string then raise postCountUpdate
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
 * TYPE: string * TextIO.instream * string * int -> string
 * PRE: 0 < whichField < 5. userFileStream must only contain user strings.
 * POST: if userName = (a substring of a line in userFileStream starting from index 0 and ending at the index of the first ">" in the line) then
			The contents of userFileStream as a string where the line matching the above condition named userFileLine is in the format
			subString1 ^ ">" ^ subString2 ^ ">" ^ subString3 ^ ">" ^ subString4 then
				replacement as subString[whichField] in subString1 ^ ">" ^ subString2 ^ ">" ^ subString3 ^ ">" ^ subString4
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

	
(* addToPostCount(user as User(uName, pw, date, postCount))
 * TYPE: user -> unit
 * PRE: user <> EmptyUser
 * POST: ()
 * SIDE EFFECTS: Opens instream to the user text file, advances instream, closes instream.
				 Opens outstream to the user text file.
				 Sets the uPostCount of user uName to postCount+1 in the outstream.
				 Closes outstream.
 * EXCEPTIONS: if user = EmptyUser then raise generalErrorMsg
 *)
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
 * TYPE: User * string -> unit
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
 * TYPE: string * User -> unit
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
			(if not(chatExists(chatName, openIn("../webchat/chats/chats.master"))) then
				(output(outStream,(chatName ^ "\n")); closeOut (outStream); closeOut(openOut("../webchat/chats/" ^ chatName ^ ".txt")); generateChat(chatName,user)) 
			else
				print("Chat already exists!"))
		else
			(closeOut(outStream); print("Chatname cantains illegal characters,<br />please use alpha-numeric characters only."))
	end;

(* clearChat(chatName, user)
 * TYPE: string * User -> unit
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
 * SIDE EFFECTS: Advances the position of stream
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
 * TYPE: string * User -> unit
 * PRE: chat file chatName exists
 * POST: ()
 * SIDE EFFECTS: Deletes chat chatName
				 
				 generateChat("Main", user)
 *)
fun deleteChat(chatName,user) =
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


(* main()
 * TYPE: unit -> unit
 * PRE: true
 * POST: ()
 * SIDE EFFECTS: Prints initial html code for the webpage...
 *               MOAR
 * EXCEPTIONS: if the html variable formType is not equal to one of the following strings:
				"login"
				"signup"
				"postMessage"
				"createNewChat"
				"clearChat"
				"deleteChat"
				"reloadChat"
				then raise generalErrorMsg
 *)	
fun main() =
	let
		val formType = getOpt(cgi_field_string("formType"), "")
		
		val name = filterString(getOpt(cgi_field_string("username"), ""))
		val password = getOpt(cgi_field_string("password"), "")
		val passwordRepeat = getOpt(cgi_field_string("repeatPassword"), "")
		
		val chatName = if getOpt(cgi_field_string("chatName"), "") = "" then "Main" else getOpt(cgi_field_string("chatName"), "")
		val userList = getUser(openIn "../webchat/users.txt", name)
		val user = returnUser(userList)
    in
		(print ("Content-type: text/html\n\n<!DOCTYPE html><html><head><meta charset=\"UTF-8\"><link rel=\"stylesheet\" type=\"text/css\" href=\"" ^ websiteURL ^ "styles/styles.css\" /></head><body>");
		print "<div class=\"headerDiv\"></div>";
		(case formType of
			"login" => login(user, password)
		  | "signup" => signup(name, password, passwordRepeat)
		  | "postMessage" => postMessage(user,chatName)
		  | "createNewChat" => createNewChat(chatName,user)
		  | "clearChat" => clearChat(chatName,user)
		  | "deleteChat" => deleteChat(chatName,user)
		  | "reloadChat" => generateChat(chatName,user)
		  | _ => raise generalErrorMsg "Error in function Main");
		print "</body></html>")
    end;

("1. encrypt = ", encrypt "hej" = "132491313371946567748579378563577805694669051416254891136");
("2. convertPassword = ", convertPassword("abc123") = "979899495051");
("3. convertPassword = ", convertPassword("1") = "49");
("4. findE = ", IntInf.toString(findE(IntInf.fromInt(24),IntInf.fromInt(1))) = "5");


("5. getSmiley = ", getSmiley (#"P") = "<img class=\"smiley\" src=\"" ^ websiteURL ^ "styles/images/smileys/blub.jpg\" />");
("6. getSmiley = ", getSmiley (#"A") = ":A");
("7. insertSmiley = ", insertSmiley [#"h",#"e",#"j",#" ",#":",#"P"] = "hej <img class=\"smiley\" src=\"" ^ websiteURL ^ "styles/images/smileys/blub.jpg\" />");

("8. filterChar = ", filterChar #"<" = "&lt;");
("9. filterChar = ", filterChar #"a" = "a");
("10. filterString = ", filterString "daniel&oscar<@>" = "daniel&amp;oscar&lt;&#64;&#62;");
("11. alphaNumCheck = ", alphaNumCheck [#"h",#"!"] = false);
("12. alphaNumCheck = ", alphaNumCheck [#"h",#"e",#"1"] = true);
("13. stringSizeCheck = ", stringSizeCheck "ab" = false);
("14. stringSizeCheck = ", stringSizeCheck "abc" = true);
("15. nameToLower = ", nameToLower "HEJ" = "hej");

("16. getName = ",  getName [#"d", #"a", #"n", #"i", #"e", #"l", #">", #"o", #"s", #"c"] = [#"d", #"a", #"n", #"i", #"e", #"l"]);
("17. getName = ",  getName [#"d", #"a", #"n", #"i", #"e", #"l"] = [#"d", #"a", #"n", #"i", #"e", #"l"]);

("18. splitList = ", splitList([1,2,3],[[]],2) = [[1], [3]]);
("19. splitList = ", splitList([1,2,3],[[4,5,6],[7,8,9]],1) = [[6, 5, 4], [2, 3], [7, 8, 9]]);
("20. splitList = ", splitList([1,2,3],[[]],1) = [[], [2, 3]]);

("21. returnUser = ", returnUser "daniel>hej>tjena>1\n lsdkflksdjksjdfdjk" = User("daniel","hej","tjena",1));
("22. returnUser = ", returnUser "daniel>hej" = EmptyUser);

("23. formatDate = ", formatDate "Fri Mar 21 14:10:45 2014" = "Mar 21 2014");
("24. formatDate = ", formatDate "Hej jag heter janne" = "jag hetanne");

("25. checkLogin = ", checkLogin(User("","hej","",1),"hej") = true);
("26. checkLogin = ", checkLogin(User("","hej","",1),"tjena") = false);

("27. readMSG = ", readMSG(MSG("Hej","pa","dej")) = "<b>Hej</b> pa:<br /><i>dej</i><br /><div class=\"chatPostLine\"></div>");
("28. readMSGS = ", readMSGS(Chat("",[MSG("Hej","pa","dej"),MSG("tja","pa","er")])) = "<b>Hej</b> pa:<br /><i>dej</i><br /><div class=\"chatPostLine\"></div><b>tja</b> pa:<br /><i>er</i><br /><div class=\"chatPostLine\"></div>");

("29. changeUserFieldInFile = ", changeUserFieldInFile("Daniel","Daniel>348934853498>hej>2\n","bertil",3) = "Daniel>348934853498>bertil>2\n");
("30. changeUserFieldInFile = ", changeUserFieldInFile("Daniel","Oscar>348934853498>hej>2\n","bertil",3) = "Oscar>348934853498>hej>2\n");
