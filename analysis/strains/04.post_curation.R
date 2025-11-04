devtools::load_all()
library(magrittr)

sheet = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1fihjz8_0uI33uTVMsTqNAgtJkQqyVueGcExmP1QZt_s/edit")
main_frame = readRDS('data-raw/strain_data/main_frame.rds')


sheetProcessed = sheet %>% dplyr::filter(gpt_sensitive %in% c(TRUE,FALSE), gpt_specific %in% c(TRUE,FALSE),!sapply(`# strains used`,is.null))

sheetProcessed$shortName = sheetProcessed$shortName %>% gsub('\\.[0-9]+',"",.)


sheetProcessed$`# strains used` = sheetProcessed$`# strains used` %>% 
    sapply(\(x){stringr::str_extract(x,'[0-9]+')}) %>%
    as.integer

sheetProcessed = sheetProcessed %>% dplyr::filter(!is.na(`# strains used`))

to_remove = sheet$shortName[!sheet$shortName %in% sheetProcessed$shortName]

curated_sheet = main_frame
curated_sheet = curated_sheet[!curated_sheet$shortName %in% to_remove,]

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
    {.$gpt_correct_guess_count/(.$gpt_guess_count + .$gpt_false_negatives)}

acc_sheet$precision = 0
acc_sheet$precision[acc_sheet$gpt_guess_count!=0] = acc_sheet[acc_sheet$gpt_guess_count!=0,] %>%
    {.$gpt_correct_guess_count/(.$gpt_guess_count + .$gpt_false_guesses_count)} 


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

