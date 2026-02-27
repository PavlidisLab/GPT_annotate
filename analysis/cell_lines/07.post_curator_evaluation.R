devtools::load_all()
library(magrittr)
library(ontologyIndex)

clo = ontologyIndex::get_ontology('data-raw/ontologies/CLO.obo',extract_tags = 'everything')
efo = ontologyIndex::get_ontology('data-raw/ontologies/EFO.obo',extract_tags = 'everything')


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
    
    return(c(efo_links,efo_synonyms,clo_synonyms,efo_data$xref,efo_data$alt_id,clo_alternative_labels,clo_data$name,efo_data$name) %>% unlist)
}


gpt_outputs = list.files('data-raw/cell_line_data/cell_line_inputs_gpt',full.names = TRUE)
cell_line_inputs = readRDS('data-raw/cell_line_data/cell_line_inputs_all.rds')
gpt_inputs = list.files('data-raw/cell_line_data/cell_line_inputs',full.names = TRUE)
second_pass_outputs = list.files('data-raw/cell_line_data/cell_line_inputs_second_pass_gpt',full.names = TRUE)

main_frame = readRDS('data-raw/cell_line_data/main_frame.rds')

sheet = readr::read_tsv('data-raw/cell_line_data/curation.tsv')
sheetProcessed = sheet %>% dplyr::filter(!grepl("uncurated",notes))

sheetProcessed$`# lines used`[is.na(sheetProcessed$`# lines used`)] = sheetProcessed$gemma_uri[is.na(sheetProcessed$`# lines used`)]  %>% strsplit(',') %>% sapply(length)
sheetProcessed$`# TRUE guesses by GPT`[is.na(sheetProcessed$`# TRUE guesses by GPT`)] = 0
to_remove = sheet$shortName[!sheet$shortName %in% sheetProcessed$shortName | sheet$information_unavailable]


curated_sheet = main_frame
curated_sheet = curated_sheet[!curated_sheet$shortName %in% to_remove,]


if (file.exists('data-raw/cell_line_data/gpt_correct_guess_count.rds')){
    gpt_correct_guess_count = readRDS('data-raw/cell_line_data/gpt_correct_guess_count.rds')
} else{
    curated_sheet %>% nrow %>% seq_len %>% parallel::mclapply(\(i){
        terms = curated_sheet$gpt_cell_line_term_id[[i]] %>% unique
        terms = terms[terms!='']
        gemma_terms = unlist(curated_sheet$gemma_uri[[i]])
        gemma_term_names =  unlist(curated_sheet$gemma_term[[i]])
        matches = sapply(terms,\(x){
            related =  x %>% get_related_terms() %>% {c(x,.)}
            name_match = any(tolower(related) %in% tolower(gemma_term_names)) 
            related %<>% stringr::str_replace(':','_')
            id_match = any(grepl(ogbox::regexMerge(related),gemma_terms))
            return(name_match || id_match)
        })
        if(length(matches)>0){
            return(sum(matches))
        } else{
            return(0)
        }
    },mc.cores= 16) %>% unlist -> gpt_correct_guess_count 
    saveRDS(gpt_correct_guess_count,file = 'data-raw/cell_line_data/gpt_correct_guess_count.rds')
}

if (file.exists('data-raw/cell_line_data/gemma_gpt_match_count.rds')){
    gemma_gpt_match_count = readRDS('data-raw/cell_line_data/gemma_gpt_match_count.rds')
} else{
    curated_sheet %>% nrow %>% seq_len %>% parallel::mclapply(\(i){
        terms = curated_sheet$gpt_cell_line_term_id[[i]] %>% unique
        terms = terms[terms!='']
        gemma_terms = unlist(curated_sheet$gemma_uri[[i]])
        gemma_term_names =  unlist(curated_sheet$gemma_term[[i]])
        gemma_fr = data.frame(gemma_terms,gemma_term_names)
        
        matches = sapply(seq_len(nrow(gemma_fr)),\(j){
            terms %>% lapply(\(x){
                x %>% get_related_terms() %>% {c(x,.)}
            }) %>% unlist -> related
            
            name_match = tolower(gemma_fr$gemma_term_names[j]) %in% tolower(related)
            related %<>% stringr::str_replace(':','_')
            if(length(related)==0){
                id_match=FALSE
            } else{
                id_match = grepl(ogbox::regexMerge(related),gemma_fr$gemma_terms[j])
            }
            return(name_match || id_match)
        })
        if(length(matches)>0){
            return(sum(matches))
        } else{
            return(0)
        }
    },mc.cores= 16) %>% unlist -> gemma_gpt_match_count
    saveRDS(gemma_gpt_match_count,file = 'data-raw/cell_line_data/gemma_gpt_match_count.rds')
}



curated_sheet$gemma_gpt_match_count = gemma_gpt_match_count
curated_sheet$gpt_correct_guess_count = gpt_correct_guess_count
curated_sheet$gemma_count = curated_sheet$gemma_uri %>% sapply(unlist) %>% sapply(length)


all(curated_sheet$gemma_gpt_match_count <= curated_sheet$gemma_count)

curated_sheet$gpt_guess_count = curated_sheet$gpt_cell_line_term_id %>% sapply(unique) %>%  sapply(length) 

all(curated_sheet$gpt_guess_count >= curated_sheet$gpt_correct_guess_count)



curated_sheet$sensitive = curated_sheet$gemma_count == curated_sheet$gemma_gpt_match_count
curated_sheet$specific = curated_sheet$gpt_correct_guess_count == curated_sheet$gpt_guess_count




curated_sheet$specific[curated_sheet$shortName %in% sheetProcessed$shortName] = 
    sheetProcessed$gpt_specific[match(curated_sheet$shortName[curated_sheet$shortName %in% sheetProcessed$shortName],sheetProcessed$shortName)] %>%
    {.[is.na(.)]=FALSE;.}
curated_sheet$sensitive[curated_sheet$shortName %in% sheetProcessed$shortName] = 
    sheetProcessed$gpt_sensitive[match(curated_sheet$shortName[curated_sheet$shortName %in% sheetProcessed$shortName],sheetProcessed$shortName)] %>%
    {.[is.na(.)]=FALSE;.}

curated_sheet$gpt_correct_guess_count[curated_sheet$shortName %in%sheetProcessed$shortName] = 
    sheetProcessed$`# TRUE guesses by GPT`[match(curated_sheet$shortName[curated_sheet$shortName %in% sheetProcessed$shortName ],sheetProcessed$`shortName`)]


curated_sheet$gpt_guess_count[curated_sheet$shortName %in%sheetProcessed$shortName ] = 
    sheetProcessed$`# guesses by GPT`[match(curated_sheet$shortName[curated_sheet$shortName %in% sheetProcessed$shortName ],sheetProcessed$`shortName`)]

curated_sheet$gemma_count[curated_sheet$shortName %in%sheetProcessed$shortName ] =
    sheetProcessed$`# lines used`[match(curated_sheet$shortName[curated_sheet$shortName %in% sheetProcessed$shortName ],sheetProcessed$`shortName`)]

# if a curator deemed it specific, gpt_matches must equal gemma_counts
curated_sheet$gemma_gpt_match_count[curated_sheet$specific] = curated_sheet$gemma_count[curated_sheet$specific]


# experiment count ---
nrow(curated_sheet)

curated_sheet$paper_accessible  = curated_sheet$shortName %>% sapply(\(x){
    length(cell_line_inputs[[x]]$papers)>0
})
# accessible paper count ----
curated_sheet$paper_accessible %>% sum


# if a curator deemed a dataset sensitive, all guesses are TRUE. however, to avoid overly weighting cases where 
# multiple gpt guesses were deemed correct for a single term, cap it at gemma term count
curated_sheet$gpt_correct_guess_count[curated_sheet$sensitive] = curated_sheet$gemma_count[curated_sheet$sensitive]
curated_sheet$gpt_guess_count[curated_sheet$sensitive] = curated_sheet$gemma_count[curated_sheet$sensitive]


# cap the correct guess count at gemma count 
curated_sheet$gpt_correct_guess_count = pmin(curated_sheet$gemma_count,curated_sheet$gpt_correct_guess_count,curated_sheet$gpt_guess_count)

# Sensitivity specificity --------

sum(curated_sheet$sensitive)
sum(curated_sheet$sensitive)/nrow(curated_sheet)
sum(!curated_sheet$sensitive)
sum(!curated_sheet$sensitive)/nrow(curated_sheet)

sum(curated_sheet$specific)
sum(curated_sheet$specific)/nrow(curated_sheet)
sum(!curated_sheet$specific)
sum(!curated_sheet$specific)/nrow(curated_sheet)



# alt sensitivity 
sum(curated_sheet$gpt_correct_guess_count == curated_sheet$gemma_count)
# find mismatches
which((curated_sheet$gpt_correct_guess_count == curated_sheet$gemma_count) & !curated_sheet$sensitive)
which((!curated_sheet$gpt_correct_guess_count == curated_sheet$gemma_count) & curated_sheet$sensitive)

# perfect
sum((curated_sheet$sensitive & curated_sheet$specific))
sum((curated_sheet$sensitive & curated_sheet$specific))/nrow(curated_sheet)


# non perfect
sum(!(curated_sheet$sensitive & curated_sheet$specific))
sum(!(curated_sheet$sensitive & curated_sheet$specific))/nrow(curated_sheet)


# rankings check

no_correct = curated_sheet %>% dplyr::filter(gpt_correct_guess_count ==0)
no_correct %>% dim


#curated_frame[curated_frame$gpt_match != "TRUE" & curated_frame$gemma_correction == "TRUE",]

# gpt help count -----
sheetProcessed$notes %>% grepl('helped',.,ignore.case =TRUE) %>% sum


# per experiment sensitivity, specificity,sensitivity -----

curated_sheet$gpt_false_negatives = curated_sheet$gemma_count - curated_sheet$gemma_gpt_match_count

curated_sheet$gpt_false_guesses_count = curated_sheet$gpt_guess_count - curated_sheet$gpt_correct_guess_count


# curated_sheet %>%dplyr::filter((.$gpt_guess_count + .$gpt_false_negatives) == 0) %$% shortName

curated_sheet$recall = curated_sheet %>% 
    {.$gpt_correct_guess_count/(.$gpt_guess_count + .$gpt_false_negatives)}


curated_sheet$recall  %>% na.omit %>% mean
curated_sheet$recall  %>% na.omit %>% sd

curated_sheet$precision = 0
curated_sheet$precision[curated_sheet$gpt_guess_count!=0] = curated_sheet[curated_sheet$gpt_guess_count!=0,] %>%
    {.$gpt_correct_guess_count/(.$gpt_guess_count + .$gpt_false_guesses_count)} 

curated_sheet$precision  %>% na.omit %>% mean
curated_sheet$precision  %>% na.omit %>% sd

curated_sheet$f1 = 2*(curated_sheet$precision *curated_sheet$recall)/(curated_sheet$precision +curated_sheet$recall)
curated_sheet$f1[curated_sheet$f1 %>% is.nan] = 0
curated_sheet$f1 %>% mean
curated_sheet$f1 %>% sd


# sanity check
curated_sheet$gpt_false_negatives  %>% range
curated_sheet$recall  %>% range
curated_sheet$precision %>% range

