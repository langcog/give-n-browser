
function show(c){
	var el = document.getElementById(c + "_content");
	if(el.style.display == 'block'){
		el.style.display = 'none';
	} else {
		el.style.display = 'block';
	}
}