library(magrittr)
library(ontologyIndex)
library(gemma.R)
library(pbapply)
embedding_price = 0.130
python = new.env()
reticulate::source_python(system.file('gpt.py',package= 'GPTAnnotate'),python)
gpt = python$gpt_query()

clo = ontologyIndex::get_ontology('data-raw/ontologies/CLO.obo',extract_tags = 'everything')

efo = ontologyIndex::get_ontology('data-raw/ontologies/EFO.obo',extract_tags = 'everything')
efo_cells = get_descendants(efo,"CL:0000000")


process_ontology_term = function(ontology,i){
    list(ID = jsonlite::unbox(ontology$id[[i]]),
         value = jsonlite::unbox(ontology$name[[i]]),
         description = jsonlite::unbox(ontology$def[[i]] %>% gsub("\\]|\\[|\\\"","",x=.) %>% trimws()),
         synonyms = ontology$synonym[[i]] %>%
             stringr::str_extract_all('(?<=").*?(?=")') %>%
             unlist )
}

cell_line_list = seq_along(clo$id) %>% lapply(\(i){
    process_ontology_term(clo,i)
})
names(cell_line_list) = cell_line_list %>% purrr::map_chr('ID')

efo_cells_list  = which(efo$id %in% efo_cells) %>% lapply(\(i){
    process_ontology_term(efo,i)
})
names(efo_cells_list) = efo_cells_list %>% purrr::map_chr('ID')
commons = intersect(names(cell_line_list),names(efo_cells_list))
efo_cells_list = efo_cells_list[!names(efo_cells_list) %in% commons]


cell_lines = c(cell_line_list, efo_cells_list)

cell_line_list = cell_lines
usethis::use_data(cell_line_list)
cell_line_strings = cell_lines %>% lapply(jsonlite::toJSON) %>% lapply(as.character)
tokens = gpt$count_tokens(cell_line_strings)
tokens*embedding_price/1000000

token_counts = cell_line_strings %>% sapply(gpt$num_tokens)

n_token_batches = function(inputs, token_limit = 50000){
    i = 1
    current_start = 1
    out = list()
    current_input = list()
    current_sum = 0

    while(i <= length(inputs)){
        tokens = gpt$num_tokens(inputs[[i]])
        if (current_sum+tokens > token_limit){
            out[[glue::glue("{stringi::stri_pad_left(current_start,4,0)}_to_{stringi::stri_pad_left(i-1,4,0)}")]] = current_input
            current_input = list()
            current_sum = tokens
            current_start = i
        } else if ( i ==  length(inputs)){
            current_input = c(current_input, inputs[i])
            out[[glue::glue("{stringi::stri_pad_left(current_start,4,0)}_to_{stringi::stri_pad_left(i,4,0)}")]] = current_input

        } else{
            current_sum = tokens + current_sum
        }

        current_input = c(current_input, inputs[i])
        i = i+1
    }
    return(out)
}
embedding_inputs = n_token_batches(cell_line_strings)

embedded_batches = embedding_inputs %>% pblapply(gpt$embed_text)
embeddings = embedded_batches %>% unname %>% do.call(c,.)
saveRDS(embeddings,file='data-raw/cell_line_embeddings.rds')
