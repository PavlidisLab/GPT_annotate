devtools::load_all()


regex_outputs = list.files('data-raw/strain_data/strain_inputs_regex/',full.names = TRUE)
gpt_outputs = list.files('data-raw/strain_data/strain_inputs_gpt/',full.names = TRUE)



regex_frame = regex_outputs %>% lapply(\(x){
    regex_out = x %>% readRDS()
    
    seq_along(regex_out) %>% lapply(\(i){
        data.frame(shortName = names(regex_out)[i],
                   regex_strains = names(regex_out[[i]]) %>% paste(collapse = ','),
                   regex_quote = regex_out[[i]] %>%
                       unlist %>%
                       unique %>%
                       paste(collapse = '❗')
        )
    }) %>% do.call(rbind,.)
}) %>% do.call(rbind,.)

gpt_frame = gpt_outputs %>% lapply(\(x){
    gpt_out = x %>% readLines() %>% jsonlite::fromJSON(simplifyVector = FALSE)
    
    seq_along(gpt_out) %>% lapply(\(i){
        
        if(length(gpt_out[[i]]$output)==0){
            return(
                data.frame(shortName = names(gpt_out[i]),
                           gpt_tokens_used = gpt_out[[i]]$usage$total_tokens,
                           gpt_strains = "",
                           gpt_uris =   "",
                           gpt_quote =  "")
            )
        }
        df = data.frame(shortName = names(gpt_out[i]),
                        gpt_tokens_used = gpt_out[[i]]$usage$total_tokens)
        
        df$gpt_strains = list (gpt_out[[i]]$output[[1]]$strains %>% purrr::map_chr('value'))
        df$gpt_uris = list(gpt_out[[i]]$output[[1]]$strains %>% purrr::map_chr('URI'))
        df$gpt_quote =  gpt_out[[i]]$output[[1]]$strains %>% purrr::map_chr(\(z){paste(z$quote,collapse = '❗')}) %>%
            paste(collapse = '❕')
        return(df)
    }) %>% do.call(rbind,.)
}) %>% do.call(rbind,.)

main_frame = dplyr::left_join(regex_frame,gpt_frame,by ='shortName')
all_mouse = readRDS('data-raw/strain_data//all_mouse_strains.rds')

main_frame$strain_names = main_frame$shortName %>% lapply(\(x){
    all_mouse %>% dplyr::filter(experiment.accession %in% x) %>% {.$strain_names}
})
main_frame$strain_uris = main_frame$shortName %>% lapply(\(x){
    all_mouse %>% dplyr::filter(experiment.accession %in% x) %>% {.$strain_uris}
})


main_frame %<>% dplyr::filter(!sapply(gpt_uris,is.null))




