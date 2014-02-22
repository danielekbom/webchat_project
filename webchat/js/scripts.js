// JavaScript Document

function scrollChatWindow(){
	var messageDiv = document.getElementById("chatMessagesDiv");
	messageDiv.scrollTop = messageDiv.scrollHeight;
}

window.onload = scrollChatWindow();