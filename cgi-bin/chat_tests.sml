(*
Authors: Oscar Ahlén, Daniel Enkvist, Daniel Ekbom
Date: 5 mars 2014
*)

load "IntInf";
load "Mosmlcgi";
load "Date";
load "OS";

use "chat.sml";

("1. encrypt = ", encrypt "hej" = "132491313371946567748579378563577805694669051416254891136");
("2. convertPassword = ", convertPassword("abc123") = "979899495051");
("3. convertPassword = ", convertPassword("1") = "49");
("4. findE = ", IntInf.toString(findE(IntInf.fromInt(24),IntInf.fromInt(2))) = "5");
("5. euclid = ", IntInf.toString(euclid(IntInf.fromInt(1071),IntInf.fromInt(1029))) = "21");

("6. getSmiley = ", getSmiley (#"P") = "<img class=\"smiley\" src=\"" ^ websiteURL ^ "styles/images/smileys/blub.jpg\" />");
("7. getSmiley = ", getSmiley (#"A") = ":A");
("8. insertSmiley = ", insertSmiley [#"h",#"e",#"j",#" ",#":",#"P"] = "hej <img class=\"smiley\" src=\"" ^ websiteURL ^ "styles/images/smileys/blub.jpg\" />");

("9. filterChar = ", filterChar #"<" = "&lt;");
("10. filterChar = ", filterChar #"a" = "a");
("11. filterString = ", filterString "daniel&oscar<@>" = "daniel&amp;oscar&lt;&#64;&#62;");
("12. alphaNumCheck = ", alphaNumCheck [#"h",#"!"] = false);
("13. alphaNumCheck = ", alphaNumCheck [#"h",#"e",#"1"] = true);
("14. stringSizeCheck = ", stringSizeCheck "ab" = false);
("15. stringSizeCheck = ", stringSizeCheck "abc" = true);
("16. nameToLower = ", nameToLower "HEJ" = "hej");

("17. getName = ",  getName [#"d", #"a", #"n", #"i", #"e", #"l", #">", #"o", #"s", #"c"] = [#"d", #"a", #"n", #"i", #"e", #"l"]);
("18. getName = ",  getName [#"d", #"a", #"n", #"i", #"e", #"l"] = [#"d", #"a", #"n", #"i", #"e", #"l"]);

("19. splitList = ", splitList([1,2,3],[[]],2) = [[1], [3]]);
("20. splitList = ", splitList([1,2,3],[[4,5,6],[7,8,9]],1) = [[6, 5, 4], [2, 3], [7, 8, 9]]);
("21. splitList = ", splitList([1,2,3],[[]],1) = [[], [2, 3]]);

("22. returnUser = ", returnUser "daniel>hej>tjena>1\n lsdkflksdjksjdfdjk" = User("daniel","hej","tjena",1));
("23. returnUser = ", returnUser "daniel>hej" = EmptyUser);

("24. formatDate = ", formatDate "Fri Mar 21 14:10:45 2014" = "Mar 21 2014");
("25. formatDate = ", formatDate "Hej jag heter janne" = "jag hetanne");

("26. checkLogin = ", checkLogin(User("","hej","",1),"hej") = true);
("27. checkLogin = ", checkLogin(User("","hej","",1),"tjena") = false);

("28. readMSG = ", readMSG(MSG("Hej","pa","dej")) = "<b>Hej</b> pa:<br /><i>dej</i><br /><div class=\"chatPostLine\"></div>");
("29. readMSGS = ", readMSGS(Chat("",[MSG("Hej","pa","dej"),MSG("tja","pa","er")])) = "<b>Hej</b> pa:<br /><i>dej</i><br /><div class=\"chatPostLine\"></div><b>tja</b> pa:<br /><i>er</i><br /><div class=\"chatPostLine\"></div>");

("30. changeUserFieldInFile = ", changeUserFieldInFile("Daniel","Daniel>348934853498>hej>2\n","bertil",3) = "Daniel>348934853498>bertil>2\n");
("31. changeUserFieldInFile = ", changeUserFieldInFile("Daniel","Oscar>348934853498>hej>2\n","bertil",3) = "Oscar>348934853498>hej>2\n");

("32. nameToLower = ", nameToLower "JANNE" = "janne");
("33. nameToLower = ", nameToLower "janne" = "janne");
