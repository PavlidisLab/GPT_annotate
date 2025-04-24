uri_to_id = function(uri){
    uri %>% stringr::str_extract("(?<=/)[^/]*_[0-9]*") %>% gsub('_',":",.)
}

all_properties = function(term_id,ontology){
    fields = ontology %>% names
    if(term_id %in% ontology$id){
        out = fields %>% lapply(function(x){
            ontology[[x]][[which(ontology$id == term_id)]]
        })
        names(out) = fields
        class(out) = 'properties'
        return(out)
    } else{
        return(NULL)
    }

}

print.properties = function(props){
    props %>% sapply(length) %>% {
        props[.!=0]
    } %>% print
}

# use of : or _ is not consistent.
track_obsolete_efo = function(term_id, efo){
    props = all_properties(term_id,efo)
    if(!is.null(props) && props$obsolete){
        props$property_value %>% grepl('use [A-Z]+(_|:)[0-9]+ label',.) %>%
            {props$property_value[.]} %>%
            stringr::str_extract('[A-Z]+(_|:)[0-9]+') %>% gsub("_",':',.)
    } else{
        term_id
    }
}
