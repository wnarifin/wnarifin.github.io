now = new Date();
month = new Array("January","February","March","April","May","June","July","August","September","October","November","December");
date = now.getDate() + " " + month[now.getMonth()] + " " + now.getFullYear(); 
document.write(date);
