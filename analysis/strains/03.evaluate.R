devtools::load_all()
library(magrittr)

regex_outputs = list.files('data-raw/strain_data/strain_inputs_regex/',full.names = TRUE)
gpt_outputs = list.files('data-raw/strain_data/strain_inputs_gpt/',full.names = TRUE)
strain_list = readRDS('data-raw/strain_data/strain_list.rds')


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

main_frame$gemma_name = main_frame$shortName %>% lapply(\(x){
    all_mouse %>% dplyr::filter(experiment.accession %in% x) %>% {.$strain_names}
})
main_frame$gemma_uri = main_frame$shortName %>% lapply(\(x){
    all_mouse %>% dplyr::filter(experiment.accession %in% x) %>% {.$strain_uris}
})


# remove errors
main_frame %<>% dplyr::filter(!sapply(gpt_uris,is.null))

names(strain_list) = strain_list %>% purrr::map_chr('URI')
main_frame %>% nrow %>% seq_len() %>% sapply(\(i){
    gpt_uris = main_frame$gpt_uris[[i]]
    gpt_strains = main_frame$gpt_strains[[i]]
    
    real_values = strain_list[gpt_uris] %>% {.[!sapply(.,is.null)]} %>% purrr::map_chr('value')
    
    if (all(gpt_strains %in% real_values)){
        TRUE
    } else{
        FALSE
    }
}) -> matching_uri
main_frame$gpt_uris_match = matching_uri
non_matching_uris = main_frame[!matching_uri,]


non_matching_uris %>% nrow %>% seq_len() %>% lapply(\(i){
    print(i)
    gpt_uris = non_matching_uris$gpt_uris[[i]]
    gpt_strains = non_matching_uris$gpt_strains[[i]]
    
    real_values = strain_list[gpt_uris] %>% purrr::map('value')
    real_values[sapply(real_values,is.null)] = ""
    real_values = unlist(real_values) %>% unname
    
    out = data.frame(
        gpt_uris = gpt_uris,
        gpt_strains = gpt_strains,
        real_values = real_values
    ) %>% dplyr::filter(gpt_strains != real_values)
    
    return(out)
}) %>% do.call(rbind,.) -> uri_mismatches

# main_frame$gpt_uris_match = FALSE
# main_frame$gpt_uris_match[!main_frame$shortName %in% non_matching_uris$shortName] = TRUE


main_frame %>% nrow() %>% seq_len() %>% sapply(function(i){
    gemma_uri = main_frame$gemma_uri[[i]]
    gpt_uris =  main_frame$gpt_uris[[i]]
    
    all(gemma_uri %in% gpt_uris)
})-> basic_match

main_frame$basic_match = basic_match

papers = list.files(getOption("PAPER_PATH"),full.names = TRUE) %>% lapply(list.files) %>% sapply(length)
names(papers) =  list.files(getOption("PAPER_PATH"),full.names = TRUE) %>% basename
main_frame$paper_accessible = main_frame$shortName %>% sapply(\(x){
    out = papers[names(papers) %in% gsub("\\.[0-1]+","",x)] > 0
    if(length(out)==0){
        out = FALSE
    }
    return(out)
})

main_frame %>% nrow() %>% seq_len() %>% sapply(function(i){
    gemma_uri = main_frame$gemma_uri[[i]] 
    gpt_uris =  main_frame$gpt_uris[[i]]
    
    all(gemma_uri %in% gpt_uris)
})-> basic_match
main_frame$basic_match = basic_match
saveRDS(main_frame,'data-raw/strain_data/main_frame.rds')

main_frame$gpt_strains %<>% sapply(\(x){
    paste(x,collapse=',')
})
main_frame$gpt_uris %<>% sapply(\(x){
    paste(x,collapse=',')
})
main_frame$gemma_name %<>% sapply(\(x){
    paste(x,collapse=',')
})
main_frame$gemma_uri %<>% sapply(\(x){
    paste(x,collapse=',')
})
main_frame %<>% dplyr::select(-basic_match)
sheet = googlesheets4::gs4_create("Mouse_strain_mismatches",sheets = 'sheet')
googlesheets4::write_sheet(main_frame %>% dplyr::filter(!basic_match),ss = sheet,sheet = 'sheet')


# sanity with the old results

old_results = readr::read_tsv('../GPTests/data-raw/strained_mice.tsv')
main_frame$basic_match = basic_match

matchin_results = main_frame %>% dplyr::filter(shortName %in% old_results$shortName)

matchin_results = matchin_results[match(old_results$shortName,matchin_results$shortName),]
comparison = cbind(old_results,matchin_results)
comparison[,names(comparison) %in% "gpt_uris_match"] %>% table
sheet = googlesheets4::gs4_create("sanity check",sheets = 'sheet')
googlesheets4::write_sheet(comparison,ss = sheet,sheet = 'sheet')
