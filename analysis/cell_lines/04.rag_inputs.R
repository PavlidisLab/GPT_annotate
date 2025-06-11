devtools::load_all()
reticulate::source_python(system.file('gpt.py',package= 'GPTests'),python)

reticulate::source_python(system.file('gpt.py',package= 'GPTests'),python)
gpt = python$gpt_query(prompt = readLines('analysis/cell_lines/rag_prompt') %>% paste(collapse = '\n'),
                       response_format = cell_line_annotation)

result_ranks = readRDS("data-raw/cell_line_data/cell_line_experiment_embedding_ranks.rds")
input_names =  result_ranks %>% names

raw_gpt_outputs = list.files('data-raw/cell_line_data/cell_line_inputs_gpt/',full.names = TRUE) %>%
    lapply(function(x){
        gpt_out = x %>% readLines() %>% jsonlite::fromJSON(simplifyVector = FALSE)
        gpt_out %>% lapply(function(y){
            if(length(y$output) ==0){
                return(NULL)
            } else{
                y$output[[1]]
            }
        })
    }) %>% do.call(c,.)

all_inputs = input_names %>% pbapply::pblapply(\(x){
    
    result_ranks[[x]] %>% lapply(\(y){
        out = cell_line_list[names(y[1:50])]
        out = out %>% lapply(\(z){
            z$cosine_similarity = y[z$ID] %>% unname
            z
        })
        out
    }) -> top_matches
    
    gpt_inference = raw_gpt_outputs[[x]]$cell_lines
    
    input=create_input(x,additional_data = list(gpt_inference = gpt_inference,
                                                ontology_terms = top_matches))
})
names(all_inputs) = input_names
saveRDS(all_inputs,'data-raw/cell_line_data/cell_line_inputs_second_pass.rds')
inputs = create_input_batches(all_inputs)
dir.create('data-raw/cell_line_data/cell_line_inputs_second_pass/')
for (inp in names(inputs)){
    writeLines(jsonlite::toJSON(inputs[[inp]],pretty = TRUE),con = glue::glue('data-raw/cell_line_data/cell_line_inputs_second_pass/{inp}.json'))
}

