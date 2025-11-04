devtools::load_all()
library(magrittr)


strains = readRDS('data-raw/strain_data/main_frame.rds')
cell_lines = readRDS('data-raw/cell_line_data/main_frame.rds')
inputs = readRDS('data-raw/delegator_data/delegator_inputs.rds')

gpt_outputs = list.files('data-raw/delegator_data/delegator_inputs_gpt//',full.names = TRUE)



gpt_frame = gpt_outputs %>% lapply(\(x){
    gpt_out = x %>% readLines() %>% jsonlite::fromJSON(simplifyVector = FALSE)
    
    seq_along(gpt_out) %>% lapply(\(i){
        
        if(length(gpt_out[[i]]$output)==0){
            return(
                data.frame(shortName = names(gpt_out[i]),
                           gpt_tokens_used = gpt_out[[i]]$usage$total_tokens,
                           gpt_delegate = "")
            )
        }
        df = data.frame(shortName = names(gpt_out[i]),
                        gpt_tokens_used = gpt_out[[i]]$usage$total_tokens)
        
        df$gpt_delegate = gpt_out[[i]]$output[[1]]$delegate_to %>% unlist %>% paste(collapse= ',')
        return(df)
    }) %>% do.call(rbind,.)
}) %>% do.call(rbind,.)



gpt_frame %>% nrow %>% seq_len() %>% sapply(\(i){
    strain_check = TRUE
    if(any(grepl(gpt_frame$shortName[i],strains$shortName))){
        strain_check = FALSE
        if(grepl('strains',gpt_frame$gpt_delegate[i])){
            strain_check = TRUE
        }
    }
    cell_line_check = TRUE
    if(any(grepl(gpt_frame$shortName[i],cell_lines$shortName))){
        cell_line_check = FALSE
        if(grepl('cell_lines',gpt_frame$gpt_delegate[i])){
            cell_line_check = TRUE
        }
    }
    return(strain_check & cell_line_check)
})
            