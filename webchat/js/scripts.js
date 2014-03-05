// JavaScript Document

//Authors: Oscar Ahlén, Daniel Enkvist, Daniel Ekbom
//Date: 5 mars 2014

var websiteURL = "http://user.it.uu.se/~daek3938/webchat/";
var currentChatMessages;

//Scrolls down the chatwindow scrollbar to the bottom automaticly.
function scrollChatWindow(){
	var messageDiv = document.getElementById("chatMessagesDiv");
	messageDiv.scrollTop = messageDiv.scrollHeight;
}

//Sets focus on the message textfield onload
function setTextFieldFocus(){
	document.getElementById('postTextField').focus();
}

//Gets the content from the chat text file of the current chat
function getChatMessages(){
	var xmlhttp;
	if (window.XMLHttpRequest){
		// code for IE7+, Firefox, Chrome, Opera, Safari
  		xmlhttp=new XMLHttpRequest();
  	}
	else{
		// code for IE6, IE5
  		xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
  	}
	xmlhttp.onreadystatechange=function(){
  		if (xmlhttp.readyState==4 && xmlhttp.status==200){
    			window.currentChatMessages = xmlhttp.responseText;
		
    	}
	}
	var chatName = document.getElementById('chatNameToJs').value;
	xmlhttp.open("POST",websiteURL + "chats/" + chatName + ".txt",true);
	xmlhttp.send();
}

//This script runs every 5 seconds. It gets the content from the chat text file of the current chat.
//It then compares the content with the content gotten from the "getChatMessages" function.
//If the content has any difference the page is reloaded.
function compareChatMessages()
{
	var xmlhttp;
	if (window.XMLHttpRequest){
		// code for IE7+, Firefox, Chrome, Opera, Safari
  		xmlhttp=new XMLHttpRequest();
  	}
	else{
		// code for IE6, IE5
  		xmlhttp=new ActiveXObject("Microsoft.XMLHTTP");
  	}
	xmlhttp.onreadystatechange=function(){
  		if (xmlhttp.readyState==4 && xmlhttp.status==200){
    		var txtDocText = xmlhttp.responseText;
			if(txtDocText != window.currentChatMessages){
				document.getElementById('currentMsgInput').value = document.getElementById('postTextField').value;
				document.getElementById('reloadChat').submit();
			}
    	}
	}
	var chatName = document.getElementById('chatNameToJs').value;
	xmlhttp.open("POST",websiteURL + "chats/" + chatName + ".txt",true);
	xmlhttp.send();
}

//This function checks if the text message field for chat messages is empty or not.
//If it is empty the submitbuttons disabled is set to true, else to false.
function msgChanged(){
	if (document.getElementById('postTextField').value != ""){
		document.getElementById('postSubmit').disabled = false
	}
	else{
		document.getElementById('postSubmit').disabled = true
	}
}

//Runs functions when the window has loaded.
window.onload = scrollChatWindow();
window.onload = getChatMessages();
window.onload = setTextFieldFocus();
window.onload = msgChanged();
//Runs the compareChatMessages function every 5 seconds.
window.setInterval(function(){
  compareChatMessages();
}, 5000);