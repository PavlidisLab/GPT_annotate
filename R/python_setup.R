#' @export
python_setup = function(){
    path = reticulate::install_python("3.13:latest")
    reticulate::virtualenv_create('GPTests313',python = path,
                                  packages = c('numpy',
                                               'openai',
                                               'tiktoken'))
    reticulate::use_virtualenv("GPTests313")

    reticulate::py_install("openai","GPTests313")
    reticulate::py_install("numpy","GPTests313")
    reticulate::py_install("tiktoken","GPTests313")
    reticulate::py_install("scipy","GPTests313")


}


.onLoad <- function(libname, pkgname){
    tryCatch({
        reticulate::use_virtualenv("GPTests313")
    },
    error = function(e){
        python_setup()
    })
}
