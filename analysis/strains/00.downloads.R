library(gemma.R)
library(magrittr)
library(pbapply)
devtools::load_all()

dir.create('data-raw/strain_data/',showWarnings = FALSE)


efo = ontologyIndex::get_ontology('data-raw/ontologies/EFO.obo',extract_tags = 'everything')
mice = ontologyIndex::get_descendants(efo,"NCBITaxon:10090",exclude_roots = TRUE)
mice %<>% {.[!grepl("NCBITaxon",.)]}
mice_synonyms = efo$synonym[mice]

strain_list = seq_along(mice) %>% lapply(function(i){
    list(URI = jsonlite::unbox(paste0('http://www.ebi.ac.uk/efo/',gsub(":","_",mice[i]))),
         value = jsonlite::unbox(efo$name[[mice[i]]]),
         description = jsonlite::unbox(efo$def[[mice[i]]] %>% gsub("\\]|\\[|\\\"","",x=.) %>% trimws()),
         synonyms = efo$synonym[[mice[i]]] %>%
             stringr::str_extract_all('(?<=").*?(?=")') %>%
             unlist )
})




tgmo = ontologyIndex::get_ontology('data-raw/ontologies/TGMO.obo',extract_tags = 'everything')


tgmo_mice = tgmo$id[tgmo$is_a == "NCBITaxon:10090"]

tgmo_strains = seq_along(tgmo_mice) %>% lapply(function(i){
    list(URI = jsonlite::unbox(paste0('http://gemma.msl.ubc.ca/ont/',gsub(":","_",tgmo_mice[i]))),
         value = jsonlite::unbox(tgmo$name[[tgmo_mice[i]]]),
         description = jsonlite::unbox(tgmo$def[[tgmo_mice[[i]]]] %>%
                                           gsub("\\]|\\[|\\\"","",x=.) %>%
                                           trimws()),
         synonyms = tgmo$synonym[[tgmo_mice[i]]] %>%
             stringr::str_extract_all('(?<=").*?(?=")') %>%
             unlist)
})



remove_strains = c(
    "http://www.ebi.ac.uk/efo/EFO_0004000",# Mus musculus strain type
    "http://www.ebi.ac.uk/efo/EFO_0003013"# Mus musculus subspecies
)



strain_list = c(strain_list,tgmo_strains)
strain_list = strain_list[!purrr::map_chr(strain_list,'value') %in% remove_strains]

saveRDS(strain_list,"data-raw/strain_data/strain_list.rds")
# strain_list = readRDS("data-raw/strain_data/strain_list.rds")
terms = strain_list %>% purrr::map_chr('URI')
term_reg = terms %>% ogbox::regexMerge()

if (!file.exists('data-raw/strain_data/all_mouse_strained.rds')){
    all_mouse = get_datasets(taxa = 'mouse') %>% get_all_pages()
    annotations = all_mouse$experiment.ID %>% pblapply(function(id){
        gemma.R::get_dataset_annotations(id)
    })
    all_mouse$annotations = annotations
    
    strain_annots = all_mouse$annotations %>% pblapply(function(x){
        data.frame(strain_name = x$term.name[grepl(term_reg,x$term.URI)],
                   strain_uri = x$term.URI[grepl(term_reg,x$term.URI)])
    })
    
    all_mouse$strain_names = strain_annots %>% sapply(function(x){
        paste0(trimws(x$strain_name),collapse = ',')
    })
    
    all_mouse$strain_uris = strain_annots %>% sapply(function(x){
        paste0(trimws(x$strain_uri),collapse = ',')
    })
    
    saveRDS(all_mouse,file = 'data-raw/strain_data/all_mouse_strains.rds')
    
} else{
    all_mouse = readRDS('data-raw/strain_data/all_mouse_strains.rds')
}


downloaded_geo = list.files(getOption("META_PATH")) %>% tools::file_path_sans_ext()

not_yet_downloaded = all_mouse %>% dplyr::filter(!gsub('\\.[.0-9]*$',"",experiment.shortName) %in% downloaded_geo)


not_yet_downloaded$experiment.shortName %>% rev %>% gsub('\\.[.0-9]*$','',.) %>% unique %>% pblapply(\(sn){
    tryCatch(download_geo_data(sn),
             error = function(e){
                 NULL
             })
})


# create inputs for datasets -------------
strained = all_mouse %>% 
    dplyr::filter(strain_names != "" )
downloaded_geo = list.files(getOption("META_PATH")) %>% tools::file_path_sans_ext()
strained = strained %>% dplyr::filter(gsub('\\.[.0-9]*$',"",experiment.shortName) %in% downloaded_geo)

python = new.env()
reticulate::source_python(system.file('gpt.py',package= 'GPTests'),python)
gpt = python$gpt_query(prompt = readLines('analysis/cell_lines/prompt') %>% paste(collapse = '\n'),
                       prompt_data = strain_list,
                        response_format = strain_output)


input_names = strained$experiment.shortName  %>%  gsub('\\.[.0-9]*$','',.) %>% unique
input_names = input_names[input_names %in% downloaded_geo]
all_inputs = input_names %>% pbapply::pblapply(create_input)
names(all_inputs) = input_names
saveRDS(all_inputs,'data-raw/strain_data/strains_inputs_all.rds')
inputs = create_input_batches(all_inputs )
dir.create('data-raw/strain_data/strain_inputs/')
for (inp in names(inputs)){
    writeLines(jsonlite::toJSON(inputs[[inp]],pretty = TRUE),con = glue::glue('data-raw/strain_data/strain_inputs/{inp}.json'))
}
