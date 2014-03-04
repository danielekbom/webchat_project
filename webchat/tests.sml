

("1. encrypt = ", encrypt "hej" = "132491313371946567748579378563577805694669051416254891136");
("2. convertPassword = ", convertPassword("abc123") = "979899495051");
("3. convertPassword = ", convertPassword("1") = "49");
("4. findE = ", IntInf.toString(findE(IntInf.fromInt(24),IntInf.fromInt(1))) = "5")


("5. getSmiley = ", getSmiley (#"P") = "<img class=\"smiley\" src=\"" ^ websiteURL ^ "styles/images/smileys/blub.jpg\" />")
("6. getSmiley = ", getSmiley (#"A") = ":A")
("7. insertSmiley = ", insertSmiley [#"h",#"e",#"j",#" ",#":",#"P"] = "hej <img class=\"smiley\" src=\"" ^ websiteURL ^ "styles/images/smileys/blub.jpg\" />")

("8. filterChar = ", filterChar #"<" = "&lt;")
("9. filterChar = ", filterChar #"a" = "a")
("10. filterString = ", filterString "daniel&oscar<@>" = "daniel&anp;oscar&lt;&#64;&#62;")
("11. alphaNumCheck = ", alphaNumCheck [#"h",#"!"] = false)
("12. alphaNumCheck = ", alphaNumCheck [#"h",#"e",#"1"] = true)
("13. stringSizeCheck = ", stringSizeCheck "ab" = false)
("14. stringSizeCheck = ", stringSizeCheck "abc" = true)
("15. nameToLower = ", nameToLower "HEJ" = "hej")

("16. getName = ",  getName [#"d", #"a", #"n", #"i", #"e", #"l", #">", #"o", #"s", #"c"] = [#"d", #"a", #"n", #"i", #"e", #"l"])
("17. getName = ",  getName [#"d", #"a", #"n", #"i", #"e", #"l"] = [#"d", #"a", #"n", #"i", #"e", #"l"])
