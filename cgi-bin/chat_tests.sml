load "IntInf";
load "Mosmlcgi";
load "Date";
load "OS";

use "chat.sml";

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
