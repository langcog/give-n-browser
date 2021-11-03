
function show(c){
	var el = document.getElementById(c + "_content");
	if(el.style.display == 'block'){
		el.style.display = 'none';
		document.getElementById(c).setAttribute('data-active', 'off');
	} else {
		el.style.display = 'block';
		document.getElementById(c).setAttribute('data-active', 'on');
	}
}