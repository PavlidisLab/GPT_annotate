devtools::load_all()
library(magrittr)
strain_list = readRDS("data-raw/strain_data/strain_list.rds")


strain_names <- strain_list %>% purrr::map_chr('value')
strain_synonyms <- strain_list %>% purrr::map('synonyms')
strain_synonyms %<>% sapply(function(x){
    x[nchar(x)>3]
})
names(strain_synonyms) = strain_names

# remove bad for regex strains
strain_synonyms = strain_synonyms[!names(strain_synonyms) %in% c("NOR")]
strain_synonyms$`CAST/EiJ` = NULL


input_list = list.files('data-raw/strain_data/strain_inputs/',full.names = TRUE)
outputs = lapply(input_list,\(input){
    inp = jsonlite::fromJSON(readLines(input),simplifyVector = FALSE)
    out = inp %>% pbapply::pblapply(find_and_quote,strain_synonyms)
    
    path = paste0(dirname(input),"_regex")
    dir.create(path,showWarnings = FALSE)
    
    # file = paste0(tools::file_path_sans_ext(basename(input)),'.rds')
    file = basename(input)
    
    saveRDS(out,file = file.path(path,paste0(file)))
    return(out)
})
