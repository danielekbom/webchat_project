// JavaScript Document

var currentChatMessages;

function scrollChatWindow(){
	var messageDiv = document.getElementById("chatMessagesDiv");
	messageDiv.scrollTop = messageDiv.scrollHeight;
}

function setTextFieldFocus(){
	document.getElementById('postTextField').focus();
}

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
	xmlhttp.open("POST","http://user.it.uu.se/~daek3938/webchat/chats/Main.txt",true);
	xmlhttp.send();
}

function loadXMLDoc()
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
			//var chatMessagesDiv = document.getElementById("chatMessagesDiv");
			//var chatMessages = chatMessagesDiv.innerHTML;
    		var txtDocText = xmlhttp.responseText;
			if(txtDocText != window.currentChatMessages){
				document.getElementById('currentMsgInput').value = document.getElementById('postTextField').value;
				document.getElementById('reloadChat').submit();
			}
    	}
	}
	xmlhttp.open("POST","http://user.it.uu.se/~daek3938/webchat/chats/Main.txt",true);
	xmlhttp.send();
}

window.onload = scrollChatWindow();
window.onload = getChatMessages();
window.onload = setTextFieldFocus();
window.setInterval(function(){
  loadXMLDoc();
}, 5000);