run_batch = function(input,
                     gpt,
                     input_price = 2.50/1000000,
                     output_price = 10/1000000){
    
    inp = jsonlite::fromJSON(readLines(input),simplifyVector = FALSE)
    max_output_tokens = length(inp)*1024
    token_count = gpt$count_tokens_bulk(input)
    input_tokens = token_count %>% unlist %>% sum
    expected_price = input_tokens*input_price + max_output_tokens*output_price
    print(expected_price)
    batch = gpt$create_batch(input)
    
    # logging batch sources
    if (file.exists('input_batches.rds')){
        input_batches = readRDS('input_batches.rds')
    } else{
        input_batches = list()
    }
    input_batches[input] = batch$id
    saveRDS(input_batches,'input_batches.rds')
    
    
    path = paste0(dirname(input),"_gpt")
    file = basename(input)
    dir.create(path,showWarnings = FALSE)
    i = 1
    total_wait = 0
    while(TRUE){
        Sys.sleep(15*i)
        total_wait = total_wait + 15*i
        print('taking a look...')
        
        batch_info = gpt$get_batch_info(batch['id'])
        if(!is.null(batch_info$failed_at)){
            stop('Batch ended with an error. Batch id ',batch['id'])
        }
        
        if(!is.null(batch_info$completed_at)){
            batch_out =
                gpt$read_batch(batch['id'],
                               output =  file.path(path,file))
            print('job completed!')
            print(total_wait)
            break
        }
        
        i = i + 1
    }
    
}