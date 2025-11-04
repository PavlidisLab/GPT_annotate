devtools::load_all()
library(magrittr)
gpt_outputs = list.files('data-raw/cell_line_data/cell_line_inputs_gpt',full.names = TRUE)
gpt_inputs = list.files('data-raw//cell_line_data/cell_line_inputs',full.names = TRUE)

second_pass_outputs = list.files('data-raw/cell_line_data/cell_line_inputs_second_pass_gpt/',full.names = TRUE)


gpt_frame = gpt_outputs %>% lapply(\(x){
    print(x)
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


# due to an issue in input processing, some experiments were run twice. this
# just takes the first output. a clean re-run should make this obsolete
# test with
# assertthat::assert_that(any(duplicated(gpt_frame$shortName)))
gpt_frame = gpt_frame[!duplicated(gpt_frame$shortName),]


gpt_frame2 = second_pass_outputs %>% lapply(\(x){
    gpt_out = x %>% readLines() %>% jsonlite::fromJSON(simplifyVector = FALSE)
    seq_along(gpt_out) %>% lapply(\(i){
        print(i)
        if(length(gpt_out[[i]]$output)==0){
            return(
                data.frame(shortName = names(gpt_out[i]),
                           gpt_tokens_used = gpt_out[[i]]$usage$total_tokens,
                           gpt_cell_line_term = "",
                           gpt_cell_line_term_id =   ""
                )            )
        }
        
        df = data.frame(shortName = names(gpt_out[i]),
                        #gpt_cell_lines = list(gpt_out[[i]]$output[[1]]$cell_lines %>% purrr::map_chr('cell_line_name')), #%>% paste(collapse = ','),
                        #gpt_description =   list(gpt_out[[i]]$output[[1]]$cell_lines %>% purrr::map_chr('description')),# %>% paste(collapse = ','),
                        #gpt_quote =  list(gpt_out[[i]]$output[[1]]$cell_lines %>% purrr::map_chr(\(z){paste(z$quote,collapse = '❗')})  ),# %>%
                        # paste(collapse = '❕'),
                        gpt_tokens_used = gpt_out[[i]]$usage$total_tokens
        )
        
        df$gpt_cell_line_term = list(gpt_out[[i]]$output[[1]]$cell_lines %>% purrr::map_chr('cell_line_name'))
        df$gpt_cell_line_term_id =  list(gpt_out[[i]]$output[[1]]$cell_lines %>% purrr::map_chr('cell_line_ID'))
        return(df)
    }) %>% do.call(rbind,.)
}) %>% do.call(rbind,.)

gpt_frame = dplyr::left_join(gpt_frame,gpt_frame2,by = 'shortName')
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

seq_len(nrow(gpt_frame)) %>% sapply(function(i){
    gpt_terms = gpt_frame$gpt_cell_line_term_id[[i]]
    gemma_terms = gpt_frame$gemma_uri[[i]] %>% unlist %>% uri_to_id()
    any(gpt_terms %in% gemma_terms)
}) -> simple_matches
gpt_frame$basic_match = simple_matches

seq_len(nrow(gpt_frame)) %>% sapply(function(i){
    gpt_terms = gpt_frame$gpt_cell_line_term_id[[i]]
    gemma_terms = gpt_frame$gemma_uri[[i]] %>% unlist %>% uri_to_id()
    all(gemma_terms %in% gpt_terms)
}) -> sensitive
gpt_frame$sensitive = sensitive

seq_len(nrow(gpt_frame)) %>% sapply(function(i){
    gpt_terms = gpt_frame$gpt_cell_line_term_id[[i]]
    gemma_terms = gpt_frame$gemma_uri[[i]] %>% unlist %>% uri_to_id()
    all(gpt_terms %in% gemma_terms)
}) -> specific
gpt_frame$specific = specific

clo = ontologyIndex::get_ontology('data-raw/ontologies/CLO.obo',extract_tags = 'everything')

efo = ontologyIndex::get_ontology('data-raw/ontologies/EFO.obo',extract_tags = 'everything')
efo_cells = ontologyIndex::get_descendants(efo,"CL:0000000")


onto_master_list = data.frame(id = c(clo$id,efo$id), name = c(clo$name,efo$name))

seq_len(nrow(gpt_frame)) %>% sapply(\(i){
    term_ids = gpt_frame$gpt_cell_line_term_id[i] %>% unlist
    term_names = gpt_frame$gpt_cell_line_term[i] %>% unlist
    
    all(onto_master_list$name[match(term_ids,onto_master_list$id)] == term_names)
}) -> correct_naming

result_ranks = readRDS("data-raw/cell_line_data/cell_line_experiment_embedding_ranks.rds")

seq_len(nrow(gpt_frame)) %>% pbapply::pbsapply(\(i){
    result_ranks[[gpt_frame$shortName[[i]]]] %>% sapply(\(x){
        min(which(names(x) %in% gpt_frame$gpt_cell_line_term_id[[i]]))
    }) %>% min
}) -> top_rank_gpt

seq_len(nrow(gpt_frame)) %>% pbapply::pbsapply(\(i){
    result_ranks[[gpt_frame$shortName[[i]]]] %>% sapply(\(x){
        min(which(names(x) %in% uri_to_id(unlist(gpt_frame$gemma_uri[[i]]))))
    }) %>% min
}) -> top_rank_gemma


gpt_frame$top_embedding_rank_gpt = top_rank_gpt
gpt_frame$top_embedding_rank_gemma = top_rank_gemma
saveRDS(gpt_frame,'data-raw/cell_line_data/main_frame.rds')
readr::write_tsv(gpt_frame,'data-raw/cell_line_data/cell_line_frame.tsv')

mismatches = gpt_frame[!simple_matches,]



get_related_terms = function(x){
    clo_data = all_properties(x,clo)
    efo_data = all_properties(x,efo)
    
    clo_properties = clo_data$property_value
    clo_related = clo_properties[grepl('seeAlso',clo_properties)] %>%
        stringr::str_extract("[A-Z]+_[0-9]+") %>% na.omit() %>% gsub("_",":",.)
    
    clo_alternative_labels = clo_properties[grepl('IAO:0000118',clo_properties)] %>%
        stringr::str_extract("(?<=IAO:0000118 \").+(?=\" )")
    
    efo_links = clo_related %>% grepl('EFO',.) %>% {clo_related[.]}
    efo_links = efo_links %>% sapply(\(y){
        track_obsolete_efo(y,efo)
    })
    
    efo_synonyms = efo_data$synonym %>% {.[grepl("EXACT",.)]} %>%
        stringr::str_extract("(?<=\").*(?=\")")
    clo_synonyms = clo_data$synonym %>%   {.[grepl("EXACT",.)]} %>%
        stringr::str_extract("(?<=\").*(?=\")")
    
    return(c(efo_links,efo_data$xref,efo_data$alt_id,clo_alternative_labels))
    
}

simplify_name = function(x){
    x %>% tolower %>%  gsub(' cell','',.) %>% gsub('-','',.) %>% gsub(' ','',.)
}

seq_len(nrow(mismatches)) %>% pbapply::pbsapply(\(i){
    gpt_terms = mismatches[i,]$gpt_cell_line_term_id %>% unlist
    gemma_terms = mismatches[i,]$gemma_uri %>% unlist %>%uri_to_id
    
    gemma_term_name = onto_master_list$name[onto_master_list$id %in% gemma_terms] %>% simplify_name
    gpt_term_name =  onto_master_list$name[onto_master_list$id %in%  gpt_terms] %>% simplify_name
    
    
    gemma_related = lapply(gemma_terms,get_related_terms) %>% unlist
    gpt_related = lapply(gpt_terms, get_related_terms) %>% unlist
    
    gemma_related = c(simplify_name(gemma_related), gemma_term_name)
    gpt_related = c(simplify_name(gpt_related),gpt_term_name)
    
    
    out = any(gpt_terms %in% gemma_related) |
        any(gemma_terms %in% gpt_related) |
        any(gemma_related %in% gpt_related)
    
    return(out)
    
}) -> tracked_matches


real_mismatches = mismatches[!tracked_matches,]

real_mismatches = real_mismatches[,
                                  !colnames(real_mismatches) %in% c('gpt_tokens_used.x',
                                                                    'gpt_tokens_used.y')]

real_mismatches$gpt_cell_lines %<>%sapply(\(x){
    paste(unlist(x),collapse = ',')
})
real_mismatches$gpt_description %<>% sapply(\(x){
    paste(unlist(x),collapse = ',')
})
real_mismatches$gpt_quote %<>% sapply(\(x){
    paste(unlist(x),collapse = '‼️')
})

cell_line_term_ids = real_mismatches$gpt_cell_line_term_id

real_mismatches$gpt_cell_line_term_id %<>% sapply(\(x){
    paste(unlist(x),collapse = ',')
})

real_mismatches$gpt_cell_line_term = cell_line_term_ids %>% sapply(\(x){
    onto_master_list$name[onto_master_list$id %in% x] %>% paste(collapse=',')
})


real_mismatches$gemma_uri %<>% sapply(\(x){
    paste(unlist(x),collapse = ',')
})

real_mismatches$gemma_term %<>% sapply(\(x){
    paste(unlist(x),collapse = ',')
})

seq_len(ncol(real_mismatches)) %>% sapply(\(i){class(real_mismatches[[i]])})

sheet = googlesheets4::gs4_create("GPT cell line evaluation",sheets = 'sheet')
real_mismatches$top_embedding_rank_gpt %<>% {.[is.infinite(.)] = NA;.}

cell_line_inputs = readRDS('data-raw/cell_line_data/cell_line_inputs_all.rds')

real_mismatches$paper_accessible = real_mismatches$shortName %>% sapply(\(x){
    length(cell_line_inputs[[x]]$papers)>0
})
googlesheets4::write_sheet(real_mismatches,ss = sheet,sheet = 'sheet')


# sensitivity mismatches


