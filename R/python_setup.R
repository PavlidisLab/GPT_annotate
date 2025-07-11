#' @export
python_setup = function(){
    path = reticulate::install_python("3.13:latest")
    reticulate::virtualenv_create('GPTests313',python = path)
    reticulate::use_virtualenv("GPTests313")

    reticulate::py_install("openai==1.54.1","GPTests313")
    reticulate::py_install("numpy==2.1.3","GPTests313")
    reticulate::py_install("tiktoken==0.9.0","GPTests313")
    reticulate::py_install("scipy==.1.15.1","GPTests313")


}


.onLoad <- function(libname, pkgname){
    tryCatch({
        reticulate::use_virtualenv("GPTests313")
    },
    error = function(e){
        python_setup()
    })
}
