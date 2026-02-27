devtools::load_all()
library(magrittr)

sheetProcessed = readr::read_tsv('data-raw/strain_data/curation.tsv')
main_frame = readRDS('data-raw/strain_data/main_frame.rds')


sheetProcessed$`# strains used` = sheetProcessed$`# strains used` %>% 
    sapply(\(x){stringr::str_extract(x,'[0-9]+')}) %>%
    as.integer

sheetProcessed = sheetProcessed %>% dplyr::filter(!is.na(`# strains used`))

curated_sheet = main_frame
curated_sheet = curated_sheet[curated_sheet$shortName %in% sheetProcessed,]

# experiment count -----
nrow(curated_sheet)
# accessible paper count ----
curated_sheet$paper_accessible %>% sum

# everything was correct for bits that weren't sent to curators
curated_sheet$real_strain_count = curated_sheet$gemma_uri  %>% sapply(length)
curated_sheet$gpt_guess_count = curated_sheet$real_strain_count
curated_sheet$gpt_correct_guess_count =curated_sheet$real_strain_count


curated_sheet$specific[curated_sheet$shortName %in%sheetProcessed$shortName ] =
    sheetProcessed$gpt_specific[match(curated_sheet$shortName[curated_sheet$shortName %in% sheetProcessed$shortName ],sheetProcessed$`shortName`)] %>% as.logical()
curated_sheet$sensitive[curated_sheet$shortName %in%sheetProcessed$shortName ] = 
    sheetProcessed$gpt_sensitive[match(curated_sheet$shortName[curated_sheet$shortName %in% sheetProcessed$shortName ],sheetProcessed$`shortName`)] %>% as.logical()
curated_sheet$gpt_guess_count[curated_sheet$shortName %in%sheetProcessed$shortName ] = 
    sheetProcessed$`# guesses by GPT (TP+FP)`[match(curated_sheet$shortName[curated_sheet$shortName %in% sheetProcessed$shortName ],sheetProcessed$`shortName`)]
curated_sheet$gpt_correct_guess_count[curated_sheet$shortName %in%sheetProcessed$shortName ] = 
    sheetProcessed$`# TRUE guesses by GPT (=TP)`[match(curated_sheet$shortName[curated_sheet$shortName %in% sheetProcessed$shortName ],sheetProcessed$`shortName`)]
curated_sheet$real_strain_count[curated_sheet$shortName %in%sheetProcessed$shortName ] = 
    sheetProcessed$`# strains used`[match(curated_sheet$shortName[curated_sheet$shortName %in% sheetProcessed$shortName ],sheetProcessed$`shortName`)] %>% 
    purrr::map_int(\(x){
        out = stringr::str_extract(x,"[0-9]+") %>% as.integer()
        if(length(out)==0){
            out = 0
        }
        return(out)
    })

# Sensitivity specificity --------
sum(curated_sheet$specific)
sum(curated_sheet$specific)/nrow(curated_sheet)
sum(curated_sheet$sensitive)
sum(curated_sheet$sensitive)/nrow(curated_sheet)

sum(!curated_sheet$specific)
sum(!curated_sheet$specific)/nrow(curated_sheet)
sum(!curated_sheet$sensitive)
sum(!curated_sheet$sensitive)/nrow(curated_sheet)

# perfect
sum((curated_sheet$sensitive & curated_sheet$specific))
sum((curated_sheet$sensitive & curated_sheet$specific))/nrow(curated_sheet)

# non perfect
sum(!(curated_sheet$sensitive & curated_sheet$specific))
sum(!(curated_sheet$sensitive & curated_sheet$specific))/nrow(curated_sheet)

sum(!(curated_sheet$sensitive & curated_sheet$specific) & !curated_sheet$sensitive)
sum(!(curated_sheet$sensitive & curated_sheet$specific) & !curated_sheet$specific)



non_sensitives = curated_sheet %>% dplyr::filter(!sensitive)
non_specifics = curated_sheet %>% dplyr::filter(!specific)

sheetProcessed$notes %>% grepl('helped',.,ignore.case =TRUE) %>% sum

paper_present = curated_sheet %>% dplyr::filter(paper_accessible)
paper_not_present = curated_sheet %>% dplyr::filter(!paper_accessible)


sum(paper_present$sensitive)/nrow(paper_present)
sum(paper_not_present$sensitive)/nrow(paper_not_present)
sum(paper_present$specific)/nrow(paper_present)
sum(paper_not_present$specific)/nrow(paper_not_present)

paper_not_present %>% dplyr::filter(!sensitive) %$% shortName

# per experiment sensitivity, specificity,sensitivity
curated_sheet$strain_count = curated_sheet$gemma_uri %>% as.character %>% strsplit(',') %>% sapply(length)
curated_sheet$gpt_guess_count= curated_sheet$strain_count 
curated_sheet$gpt_correct_guess_count = curated_sheet$strain_count 

curated_sheet$strain_count[curated_sheet$shortName %in% sheetProcessed$shortName] = 
    sheetProcessed$`# strains used`[match(curated_sheet$shortName[curated_sheet$shortName %in% sheetProcessed$shortName ],sheetProcessed$`shortName`)]


curated_sheet$gpt_guess_count[curated_sheet$shortName %in% sheetProcessed$shortName] = 
    sheetProcessed$`# guesses by GPT (TP+FP)`[match(curated_sheet$shortName[curated_sheet$shortName %in% sheetProcessed$shortName ],sheetProcessed$`shortName`)]

curated_sheet$gpt_correct_guess_count[curated_sheet$shortName %in% sheetProcessed$shortName] = 
    sheetProcessed$`# TRUE guesses by GPT (=TP)`[match(curated_sheet$shortName[curated_sheet$shortName %in% sheetProcessed$shortName ],sheetProcessed$`shortName`)]

curated_sheet$gpt_false_guesses_count = curated_sheet$gpt_guess_count - curated_sheet$gpt_correct_guess_count

acc_sheet = curated_sheet %>% dplyr::filter(strain_count != 0)

acc_sheet$gpt_false_negatives = acc_sheet$strain_count - acc_sheet$gpt_correct_guess_count

acc_sheet$recall = acc_sheet %>% 
    {.$gpt_correct_guess_count/(.$gpt_correct_guess_count + .$gpt_false_negatives)}

acc_sheet$precision = 0
acc_sheet$precision[acc_sheet$gpt_guess_count!=0] = acc_sheet[acc_sheet$gpt_guess_count!=0,] %>%
    {.$gpt_correct_guess_count/(.$gpt_correct_guess_count + .$gpt_false_guesses_count)} 


# sanity check
acc_sheet$gpt_false_negatives  %>% range
acc_sheet$recall  %>% range
acc_sheet$precision %>% range



acc_sheet$recall  %>% na.omit %>% mean
acc_sheet$recall  %>% na.omit %>% sd

acc_sheet$precision  %>% na.omit %>% mean
acc_sheet$precision  %>% na.omit %>% sd

acc_sheet$f1 = 2*(acc_sheet$precision *acc_sheet$recall)/(acc_sheet$precision +acc_sheet$recall)
acc_sheet$f1[acc_sheet$f1 %>% is.nan] = 0
acc_sheet$f1 %>% mean
acc_sheet$f1 %>% sd



# regex things

curated_sheet %>% nrow() %>% seq_len() %>% sapply(\(i){
    gemma_term = main_frame$gemma_name[[i]]  %>% strsplit(',') %>% {.[[1]]}
    regex_names = main_frame$regex_strains[[i]] %>% strsplit(',') %>% {.[[1]]}
    all(regex_names %in% gemma_term)
}) -> regex_specific
curated_sheet$regex_specific = regex_specific


curated_sheet %>% nrow() %>% seq_len() %>% sapply(\(i){
    gemma_term = main_frame$gemma_name[[i]]  %>% strsplit(',') %>% {.[[1]]}
    regex_names = main_frame$regex_strains[[i]] %>% strsplit(',') %>% {.[[1]]}
    all(gemma_term %in% regex_names)
}) -> regex_sensitive
curated_sheet$regex_sensitive = regex_sensitive

sum(curated_sheet$regex_specific)
sum(curated_sheet$regex_specific)/nrow(curated_sheet)
sum(curated_sheet$regex_sensitive)
sum(curated_sheet$regex_sensitive)/nrow(curated_sheet)

sum(!curated_sheet$regex_specific)
sum(!curated_sheet$regex_specific)/nrow(curated_sheet)
sum(!curated_sheet$regex_sensitive)
sum(!curated_sheet$regex_sensitive)/nrow(curated_sheet)

sum((curated_sheet$regex_sensitive & curated_sheet$regex_specific))
sum((curated_sheet$regex_sensitive & curated_sheet$regex_specific))/nrow(curated_sheet)
sum(!(curated_sheet$regex_sensitive & curated_sheet$regex_specific))
sum(!(curated_sheet$regex_sensitive & curated_sheet$regex_specific))/nrow(curated_sheet)

lapply(seq_len(nrow(curated_sheet)),function(i){
    print(i)
    regex_guesses = curated_sheet$regex_strains[i] %>% strsplit(',') %>% {.[[1]]}
    gemma_strains = curated_sheet$gemma_name[[i]]
    regex_false_guesses = regex_guesses[!regex_guesses %in% gemma_strains]
    correct_regex_guesses = regex_guesses[regex_guesses %in% gemma_strains]
    false_negatives = gemma_strains[!gemma_strains %in% regex_guesses]
    
    if(length(regex_guesses)==0){
        precision = 0
        recall = 0
    } else{
        precision = length(correct_regex_guesses)/(length(correct_regex_guesses)+length(regex_false_guesses))
        recall =  length(correct_regex_guesses)/(length(correct_regex_guesses)+length(false_negatives))
    }
    if(precision == 0 && recall == 0){
        f1 = 0
    } else{
        f1 = 2*precision*recall/(precision+recall)
    }
    return(c(f1 = f1, precision = precision, recall= recall))
}) %>% do.call(rbind,.)-> regex_accuracy

regex_accuracy[,'recall'] %>% mean
regex_accuracy[,'recall'] %>% sd
regex_accuracy[,'precision'] %>% mean
regex_accuracy[,'precision'] %>% sd
regex_accuracy[,'f1'] %>% mean
regex_accuracy[,'f1'] %>% sd


# regex mistakes enrichment

imperfect_count = sum(!curated_sheet$sensitive | !curated_sheet$specific)
imperfect_regex_insentitive = sum((!curated_sheet$sensitive | !curated_sheet$specific) & !curated_sheet$regex_sensitive)

phyper(q=  imperfect_regex_insentitive,
       m = sum(!curated_sheet$regex_sensitive),
       n = sum(curated_sheet$regex_sensitive),
       k = imperfect_count,lower.tail = FALSE
)

regex_insensitive_to_curate = curated_sheet %>% dplyr::filter(!regex_sensitive & (!sensitive | !specific)) %>% nrow()


# regex mistakes enrichment by sentitivity

sum(!curated_sheet$sensitive & !curated_sheet$regex_sensitive)
sum(!curated_sheet$sensitive)
sum(!curated_sheet$regex_sensitive)

phyper(q=  sum(!curated_sheet$sensitive  & !curated_sheet$regex_sensitive),
       m = sum(!curated_sheet$regex_sensitive),
       n = sum(curated_sheet$regex_sensitive),
       k = sum(!curated_sheet$sensitive) ,lower.tail = FALSE
)
