#' @param input Standard input containing experiment metadata
#' @param terms_list A list of term synonyms. Names of the list indicating the primary name for the term
find_and_quote = function(input, terms_list){
    term_quotes = seq_along(terms_list) %>% lapply(\(i){
        term_regex = ogbox::regexMerge(c(names(terms_list)[i],terms_list[[i]]))
        flat_input = input %>% unlist

        quotes = flat_input %>% {grep(tolower(term_regex),tolower(.))} %>% {flat_input[.]}

        quotes = quotes %>% sapply(function(x){
            m_index = x %>% {gregexpr(tolower(term_regex),tolower(.))} %>% unlist
            substr(x,m_index[1]-100,m_index[1]+100)
        })

        return(unname(quotes) %>% unique)
    })

    names(term_quotes) = names(terms_list)
    term_quotes %<>% {.[sapply(.,length)>0]}
}
