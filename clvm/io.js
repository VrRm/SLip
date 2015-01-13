LispMachine.io =
(function (machine) {
this.cl_load = function (url) {
    var xhr = new XMLHttpRequest();
    xhr.open("GET", url, false);
    xhr.send(null);
    return xhr.responseText;
};

this.cl_load_fasls = function (files, cont) {
    var count = files.length;
    var fasls = [];
    files.forEach(function(filename, i){
        filename = filename.replace(/(\.lisp)?$/, ".fasl");
        log("Loading: " + filename);
	fasls[i] = cl_load(filename + "?killCache=" + Date.now());
	if (--count == 0) {
            fasls.forEach(function(code){
		machine._exec(LispMachine.unserialize(code));
            });
            cont();
	}
    });
};

this.cl_recompile_all = function () {
    cl_load_fasls([ "lisp/compiler.lisp"], function(){
        compile(lisp_files, function(){
	    log("DONE — I will reload in 3 seconds");
	    setTimeout(function(){
                window.parent.opener.location.reload(true);
	    }, 3000);
        });
    });
};
return this;
})(window.MACHINE);
