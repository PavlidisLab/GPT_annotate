.onLoad <- function(libname, pkgname){
    options(META_PATH = '/space/scratch/Ogdata/GPT_GEO_meta',
            PAPER_PATH = '/space/scratch/Ogdata/GPT_GEO_papers',
            SOFT_PATH = '/space/scratch/Ogdata/GPT_GEOs',
            TRANS_RULE = '“>\\\\\\\";”>\\\\\\\";″>\\\\\\\";‟>\\\\\\\";:: Latin-ASCII;')
    
}
#' @export
get_sections = function(passages,sections, collapse = '\n'){
    passages %>%
        purrr::map(\(x){
            x %>% purrr::map(\(y){
                types = y %>% purrr::map('infons') %>% purrr::map_chr('section_type')
                y[types == sections] %>% purrr::map('text') %>% paste(collapse = collapse)
            })
        }) %>% unlist %>% paste(collapse = '\n\n')
}


#' @export
get_paper_passages = function(file){

    paper_data = tryCatch(file %>%
        readLines() %>%
        jsonlite::fromJSON(simplifyVector = FALSE),
        error = function(e){
            if(grepl("\\[Error\\] : No result",e)){
                return(NULL)
            }
        })

    if(is.null(paper_data)){
        return(NULL)
    }

    all_passages = paper_data %>%
        purrr::map('documents') %>%
        purrr::map(function(x){
            ps = x %>% purrr::map('passages') %>%
                purrr::map(function(y){
                    # make sure the passages are ordered correctly
                    for(i in seq_along(y)){
                        y[[i]]$text = stringi::stri_trans_general(y[[i]]$text,
                                                                  'Latin-ASCII')
                    }


                    y %>% purrr::map_int('offset') %>% order() %>%
                        {y[.]}
                })
        })
}


download_geo_data = function(short_name,
                             soft_path = getOption("SOFT_PATH"),
                             meta_path = getOption('META_PATH'),
                             paper_path = getOption("PAPER_PATH"),
                             soft_redownload = FALSE){

    dir.create(soft_path,showWarnings = FALSE)

    if(file.exists(file.path(soft_path,paste0(short_name,'.soft.gz'))) && !soft_redownload){
        exp = GEOquery::getGEO(filename = file.path(soft_path,paste0(short_name,'.soft.gz')),
                               GSEMatrix = FALSE)
    } else {
        exp = GEOquery::getGEO(short_name,GSEMatrix = FALSE,destdir = soft_path)
    }

    geo_meta = list(
        title = exp@header$title,
        summary = exp@header$summary,
        overall_design = exp@header$overall_design,
        samples = exp@gsms %>% lapply(function(sample){
            list(
                title = sample@header$title,
                characteristics = sample@header$characteristics_ch1,
                protocol = sample@header$extract_protocol_ch1
            )
        }))

    writeLines(jsonlite::toJSON(geo_meta, pretty = TRUE),con = glue::glue('{meta_path}/{short_name}.json'))
    pmids = exp@header$pubmed_id

    if(!is.null(pmids)){
        dir.create(glue::glue('{paper_path}/{short_name}'),showWarnings = FALSE)
        pmids %>% lapply(function(pmid){
            pmid_out = "https://www.ncbi.nlm.nih.gov/research/bionlp/RESTful/pmcoa.cgi/BioC_json/{pmid}/unicode" %>%
                glue::glue() %>% httr::GET() %>% {.$content} %>% rawToChar()

            if(!grepl("^No record",pmid_out)){
                writeLines(pmid_out,glue::glue('{paper_path}/{short_name}/{pmid}'))
            }
        })
    }
}



create_input = function(short_name,
                        meta_path = getOption('META_PATH'),
                        paper_path = getOption('PAPER_PATH'),
                        additional_data = NULL){
    print(short_name)
    exp = glue::glue("{meta_path}/{short_name}.json") %>%
        readLines() %>%
        stringi::stri_trans_general(getOption('TRANS_RULE'),rules=TRUE)  %>%
        jsonlite::fromJSON(simplifyVector = FALSE)


    out = list(overall_design = exp$overall_design,
               summary = exp$summary,
               samples = exp$samples %>% lapply(function(sample){
                   list(
                       title = sample$title,
                       characteristics = sample$characteristics,
                       protocol = sample$protocol
                   )
               }))


    papers = list.files(glue::glue('{paper_path}/{short_name}'),
                        recursive = TRUE,include.dirs = FALSE,full.names = TRUE)

    paper_out = papers %>% lapply(function(x){
        paper_data = get_paper_passages(x)
        if(is.null(paper_data)){
            return(NULL)
        }
        out$paper = list(
            title =  paper_data %>% get_sections('TITLE') %>% jsonlite::unbox(),
            abtract = paper_data %>% get_sections('ABSTRACT') %>% jsonlite::unbox(),
            methods = paper_data %>%  get_sections('METHODS') %>% jsonlite::unbox()
        )
    })
    paper_out = paper_out[!sapply(paper_out,is.null)]

    out$papers = paper_out
    if(!is.null(additional_data)){
        out = c(out,additional_data)
    }
    return(out)
}


create_compressed_input = function(short_name,
                                   meta_path = getOption('META_PATH'),
                                   paper_path = getOption('PAPER_PATH')){

    print(short_name)
    exp = glue::glue("{meta_path}/{short_name}.json") %>%
        readLines() %>%
        stringi::stri_trans_general(TRANS_RULE,rules=TRUE)  %>%
        jsonlite::fromJSON(simplifyVector = FALSE)

    out = list(overall_design = exp$overall_design,
               summary = exp$summary,
               samples = exp$samples %>% lapply(function(sample){
                   list(
                       title = sample$title,
                       characteristics = sample$characteristics,
                       protocol = sample$protocol
                   )
               }))

    sample_titles = out$samples %>% purrr::map('title') %>% unlist %>% unique

    sample_characteristics =  out$samples %>% purrr::map('characteristics') %>% unlist %>% unique

    sample_protocol = out$samples %>% purrr::map('protocol') %>% unlist %>% unique

    out$samples = list(
        titles = sample_titles,
        characteristics = sample_characteristics,
        protocol = sample_protocol
    )

    papers = list.files(glue::glue('{paper_path}/{short_name}'),
                        recursive = TRUE,include.dirs = FALSE,full.names = TRUE)

    paper_out = papers %>% lapply(function(x){
        paper_data = get_paper_passages(x)
        out$paper = list(
            title =  paper_data %>% get_sections('TITLE') %>% jsonlite::unbox(),
            abtract = paper_data %>% get_sections('ABSTRACT') %>% jsonlite::unbox(),
            methods = paper_data %>%  get_sections('METHODS') %>% jsonlite::unbox()
        )
    })

    out$papers = paper_out
    return(out)

}

# input batches for the new gpt interface
# assumes gpt object is created outside with a populated prompt
create_input_batches = function(inputs,token_limit = 1000000, meta_path = META_PATH,
                                paper_path = PAPER_PATH){


    i = 1
    current_start = 1
    out = list()
    current_input = list()
    current_sum = 0

    while(i <= length(inputs)){
        print(i)
        tokens = gpt$count_tokens(inputs[[i]])

        if (current_sum+tokens > token_limit){
            out[[glue::glue("{stringi::stri_pad_left(current_start,4,0)}_to_{stringi::stri_pad_left(i-1,4,0)}")]] = current_input
            current_input = list()
            current_sum = tokens
            current_start = i
        } else if ( i ==  length(inputs)){
            current_input = c(current_input, inputs[i])
            out[[glue::glue("{stringi::stri_pad_left(current_start,4,0)}_to_{stringi::stri_pad_left(i,4,0)}")]] = current_input

        } else{
            current_sum = tokens + current_sum
        }

        current_input = c(current_input, inputs[i])
        i = i+1
    }

    return(out)

}


