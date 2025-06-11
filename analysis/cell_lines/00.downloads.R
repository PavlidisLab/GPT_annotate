options(timeout=240)
library(gemma.R)
library(dplyr)
library(pbapply)
library(ontologyIndex)
library(magrittr)

# get annotations of datasets with cell line annotations ----

dir.create('data-raw/cell_line_data/',showWarnings = FALSE)

cell_line_datasets = get_datasets(filter = 'allCharacteristics.categoryUri = http://purl.obolibrary.org/obo/CLO_0000031') %>% get_all_pages()
saveRDS(cell_line_datasets,file = 'data-raw/cell_line_data/cell_line_datasets.rds')
cell_line_datasets$experiment.ID %>% pbapply::pblapply(function(x){
    get_dataset_annotations(x)
}) -> annotations
names(annotations) = cell_line_datasets$experiment.shortName
saveRDS(annotations,file = 'data-raw/cell_line_data/cell_line_annotations.rds')


# get cell line annotations annotations of datasets with cell line annotations ----------------
cell_line_annotations = annotations %>% lapply(function(x){
    x %>% dplyr::filter(class.URI == 'http://purl.obolibrary.org/obo/CLO_0000031')
})
saveRDS(cell_line_annotations,file = 'data-raw/cell_line_data/cell_line_annotations2.rds')



# embed cell line terms ------------------
embedding_price = 0.130
python = new.env()
reticulate::source_python(system.file('gpt.py',package= 'GPTests'),python)
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
saveRDS(cell_line_list,'data-raw/cell_line_data/cell_line_list.rds')
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
saveRDS(embeddings,file='data-raw/cell_line_data/cell_line_embeddings.rds')

# download datasets with embedded cell lines -------------


embedded_cell_line_annotations = cell_line_annotations %>% purrr::map(\(x){
    x %>% dplyr::filter(term.URI %>% uri_to_id() %in% names(embeddings))
})
is_in_embeddings = embedded_cell_line_annotations %>% sapply(nrow) %>% {.>0}
input_names = names(cell_line_annotations)[is_in_embeddings]
input_datasets = cell_line_datasets[match(input_names,cell_line_datasets$experiment.shortName),]
input_datasets$annotations = annotations[match(input_names,names(annotations))]
input_datasets$cell_line_uris = embedded_cell_line_annotations[input_names] %>%
    purrr::map_chr(\(x){
        paste0(trimws(x$term.URI),collapse = ',')
    })
input_datasets$cell_line_names = embedded_cell_line_annotations[input_names] %>%
    purrr::map_chr(\(x){
        paste0(trimws(x$term.name),collapse = ',')
    })
saveRDS(input_datasets,file = 'data-raw/cell_line_input_datasets.rds')

downloaded_geo = list.files(getOption('META_PATH')) %>% tools::file_path_sans_ext()
not_yet_downloaded = input_datasets %>% dplyr::filter(!gsub('\\.[.0-9]*$',"",experiment.shortName) %in% downloaded_geo)

not_yet_downloaded$experiment.shortName %>% rev %>% gsub('\\.[.0-9]*$','',.) %>% unique %>% pblapply(\(sn){
    tryCatch(download_geo_data(sn),
             error = function(e){
                 NULL
             })
})

# create inputs for datasets ----------------------
gpt = python$gpt_query(prompt = readLines('analysis/cell_lines/prompt') %>% paste(collapse = '\n'),
                       response_format = cell_line_output)

downloaded_geo = list.files(META_PATH) %>% tools::file_path_sans_ext()
input_names =  input_datasets$experiment.shortName %>%  gsub('\\.[.0-9]*$','',.) %>% unique
input_names = input_names[input_names %in% downloaded_geo]


all_inputs = input_names %>% pbapply::pblapply(create_input)
names(all_inputs) = input_names
saveRDS(all_inputs,'data-raw/cell_line_data/cell_line_inputs_all.rds')
inputs = create_input_batches(all_inputs )
dir.create('data-raw/cell_line_data/cell_line_inputs/')
for (inp in names(inputs)){
    writeLines(jsonlite::toJSON(inputs[[inp]],pretty = TRUE),con = glue::glue('data-raw/cell_line_data/cell_line_inputs/{inp}.json'))
}