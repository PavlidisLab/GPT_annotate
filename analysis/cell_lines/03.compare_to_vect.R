devtools::load_all()
library(lsa)
python = new.env()
gpt_outputs = list.files('data-raw/cell_line_data/cell_line_inputs_gpt/',full.names = TRUE)
embeddings = readRDS(file='data-raw/cell_line_data/cell_line_embeddings.rds')

reticulate::source_python(system.file('gpt.py',package= 'GPTAnnotate'),python)
gpt = python$gpt_query(gpt_model = "gpt-4o",prompt = readLines('analysis/cell_lines/prompt') %>% paste(collapse = '\n'),
                       response_format = cell_line_output)

gpt_frame = gpt_outputs %>% lapply(\(x){
    gpt_out = x %>% readLines() %>% jsonlite::fromJSON(simplifyVector = FALSE)
    
    seq_along(gpt_out) %>% lapply(\(i){
        print(i)
        if(length(gpt_out[[i]]$output)==0){
            return(
                data.frame(shortName = names(gpt_out[i]),
                           gpt_tokens_used = gpt_out[[i]]$usage$total_tokens,
                           gpt_cell_lines = "",
                           gpt_description =   "",
                           gpt_quote =  ""
                )            )
        }
        df = data.frame(shortName = names(gpt_out[i]),
                        #gpt_cell_lines = list(gpt_out[[i]]$output[[1]]$cell_lines %>% purrr::map_chr('cell_line_name')), #%>% paste(collapse = ','),
                        #gpt_description =   list(gpt_out[[i]]$output[[1]]$cell_lines %>% purrr::map_chr('description')),# %>% paste(collapse = ','),
                        #gpt_quote =  list(gpt_out[[i]]$output[[1]]$cell_lines %>% purrr::map_chr(\(z){paste(z$quote,collapse = '❗')})  ),# %>%
                        # paste(collapse = '❕'),
                        gpt_tokens_used = gpt_out[[i]]$usage$total_tokens
        )
        
        df$gpt_cell_lines = list(gpt_out[[i]]$output[[1]]$cell_lines %>% purrr::map_chr('cell_line_name'))
        df$gpt_description =  list(gpt_out[[i]]$output[[1]]$cell_lines %>% purrr::map_chr('description'))
        df$gpt_quote = list(gpt_out[[i]]$output[[1]]$cell_lines %>% purrr::map_chr(\(z){paste(z$quote,collapse = '❗')})  )
        return(df)
    }) %>% do.call(rbind,.)
}) %>% do.call(rbind,.)


cell_line_annotations = readRDS('data-raw/cell_line_data/cell_line_annotations2.rds')

gemma_term = gpt_frame$shortName %>% lapply(function(sn){
    base_names = names(cell_line_annotations)%>%  gsub('\\.[.0-9]*$','',.)
    cell_line_annotations[base_names %in% sn] %>% purrr::map('term.name') # %>% purrr::map_chr(paste,collapse = ',')%>% paste(collapse = ';')
})

gemma_uri =  gpt_frame$shortName %>% lapply(function(sn){
    base_names = names(cell_line_annotations)%>%  gsub('\\.[.0-9]*$','',.)
    cell_line_annotations[base_names %in% sn] %>%  purrr::map('term.URI') #%>% purrr::map_chr(paste,collapse = ',')%>% paste(collapse = ';')
})

gpt_frame$gemma_term = gemma_term
gpt_frame$gemma_uri = gemma_uri
saveRDS(gpt_frame,'data-raw/cell_line_data/cell_lines_gpt_frame.rds')

texts_to_embed =  nrow(gpt_frame) %>% seq_len() %>% lapply(function(i){
    
    out = paste(gpt_frame$gpt_cell_lines[[i]],'\n',gpt_frame$gpt_description[[i]])
    names(out) = gpt_frame$gpt_cell_lines[[i]]
    out %>% as.list()
})
names(texts_to_embed) = gpt_frame$shortName
gpt_embeddings = texts_to_embed %>% lapply(\(x){
    gpt$embed_text(x)
})
saveRDS(gpt_embeddings, 'data-raw/cell_line_data/cell_line_experiment_embeddings.Rds')

embedding_distances = gpt_embeddings %>% parallel::mclapply(function(x){
    x %>% lapply(function(y){
        embeddings %>% sapply(function(z){
            cosine(y$embedding,z$embedding)
        })
    })
    
},mc.cores =  16)


result_ranks = embedding_distances %>%
    lapply(\(x){
        x %>% lapply(sort,decreasing = TRUE)
    })
saveRDS(result_ranks,"data-raw/cell_line_data/cell_line_experiment_embedding_ranks.rds")
