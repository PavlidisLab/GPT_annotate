python = new.env()

# reticulate::source_python(system.file('strainer.py',package= 'GPTests'),python)

reticulate::source_python(system.file('gpt.py',package= 'GPTests'),python)
reticulate::source_python(system.file('gpt_test_obj.py',package= 'GPTests'),python)
gpt = python$gpt_query()

spend_tokens = FALSE

test_that('token counting',{
    testthat::expect_equal(gpt$num_tokens("hey"),1)
})


test_that('api test',{
    skip_if_not(spend_tokens)
    testthat::expect_equal(gpt$api_test()$choices[[1]]$finish_reason == 'stop')
})


test_that('system prompt generation',{
    testthat::expect_equal(gpt$prep_system(),"")

    gpt$prompt = 'This is prompt\n'
    testthat::expect_equal(gpt$prep_system(), 'This is prompt\n')

    # list prompt_data
    gpt$prompt_data = list(list(a='prompt_data added'))
    testthat::expect_equal(gpt$prep_system(),"This is prompt\n[{\"a\": \"prompt_data added\"}]")

    gpt$disclude_fields = list('a')
    testthat::expect_equal(gpt$prep_system(),"This is prompt\n[{}]")

    # dict prompt_data
    gpt$prompt_data = list(a = list(a='prompt_data added',b='a removed'))
    testthat::expect_equal(gpt$prep_system(),"This is prompt\n{\"a\": {\"b\": \"a removed\"}}")


})

test_that('messages generation',{
    messages = gpt$prep_messages('hey')
    testthat::expect_equal(messages[[2]]$content[[1]]$text,'hey')
    testthat::expect_equal(messages[[2]]$role,'user')
    testthat::expect_equal(messages[[1]]$role,'system')
})



test_that('ask_gpt',{
    skip_if_not(spend_tokens)
    gpt = python$gpt_query(prompt = "Repeat the user message")
    response = gpt$ask_gpt(message = "Quick brown fox")
    testthat::expect_equal(response$output,"Quick brown fox")


    gpt = python$gpt_query(prompt = "Respond with a number from the list below",
                           prompt_data = list(numbers = c(1337,1991,2024)))

    response = gpt$ask_gpt(message="")
    testthat::expect_true(response$output %>% as.numeric %in%c(1337,1991,2024) )

    gpt = python$gpt_query(prompt = "Respond with a number from the list below",
                           prompt_data =  system.file('test_files/numbers.json',package = 'GPTests'))


    response = gpt$ask_gpt(message="")
    testthat::expect_true(response$output %>% as.numeric %in%c(1337,1991,2024))


})

test_that('ask_gpt response format',{
    skip_if_not(spend_tokens)

    response_format = list(
        type = 'json_schema',
        json_schema = list(
            name = "number_output",
            description = "A json containing a number",
            strict = TRUE,
            schema = list(
                type = "object",
                additionalProperties = FALSE,
                required = list('number'),
                properties = list(
                    number = list(
                        type = "number",
                        description = "number to be returned"
                    )
                )
            )
        )
    )

    gpt = python$gpt_query(prompt = "Respond with a number from the list below as json",
                           prompt_data = list(numbers = c(1337,1991,2024)),
                           response_format = response_format)


    response = gpt$ask_gpt(message="")
    testthat::expect_true(response$output[[1]]$number %in%c(1337,1991,2024))

})


test_that('batch requests',{
    skip_if_not(spend_tokens)

    inputs = list(input1 = 'red',
                  input2= 'blue',
                  input3 = 'green')
    gpt = python$gpt_query(prompt = "Return the hex value for the given color")
    batch = gpt$create_batch(inputs,file ='tmp_test')

    batch_info = python$get_batch_info(batch['id'])
    testthat::expect_true(batch_info$id == gpt$latest_batch$id)


    i = 1
    total_wait = 0
    while(TRUE){
        Sys.sleep(15*i)
        batch_info = python$get_batch_info(batch['id'])
        testthat::expect_true(is.null(batch_info$failed_at))


        if(!is.null(batch_info$completed_at)){
            batch_out = gpt$read_batch(batch['id'])
        }
    }

})

test_that('embedding',{
    skip_if_not(spend_tokens)
    input = list('king','queen','man','woman')

    embedding = gpt$embed_text(input)
    names(embedding) = input

    king_man = python$relatedness(embedding$king$embedding,embedding$man$embedding)
    king_woman =  python$relatedness(embedding$king$embedding,embedding$woman$embedding)
    testthat::expect_true(king_man > king_woman)

    queen_man = python$relatedness(embedding$queen$embedding,embedding$man$embedding)
    queen_woman =  python$relatedness(embedding$queen$embedding,embedding$woman$embedding)
    testthat::expect_true(queen_man < queen_woman)


    testthat::expect_length(embedding,2)

})

test_that('seeds',{
    skip_if_not(spend_tokens)
    prompt = "Always return a random response"
    message = "Return a random number"
    gpt = python$gpt_query(prompt = prompt,seed = as.integer(1))
    response = gpt$ask_gpt(message = message)
    
    gpt = python$gpt_query(prompt = prompt,seed = as.integer(1))
    response2 = gpt$ask_gpt(message = message)
    
    assertthat::are_equal(response$output,response2$output)
    
    gpt = python$gpt_query(prompt = prompt,seed = as.integer(156))
    response3 = gpt$ask_gpt(message = message)
    
    gpt = python$gpt_query(prompt = prompt,seed = as.integer(156))
    response4 = gpt$ask_gpt(message = message)
    assertthat::are_equal(response3$output,response4$output)
    
    assertthat::assert_that(response$output != response3$output)
    
    
    gpt = python$gpt_query(prompt = prompt,seed = as.integer(1))
    batch = gpt$create_batch(inputs = list('a' = message))
    Sys.sleep(100)
    
    gpt$get_jsonl_file(gpt$get_batch_info(batch$id)$output_file_id) %>%
        jsonlite::fromJSON(simplifyVector = FALSE) %>%
        {.$response$body$choices}
    
    gpt = python$gpt_query(prompt = prompt,seed = as.integer(156))
    batch = gpt$create_batch(inputs = list('a' = message))
    Sys.sleep(100)
    
    gpt$get_jsonl_file(gpt$get_batch_info(batch$id)$output_file_id) %>%
        jsonlite::fromJSON(simplifyVector = FALSE) %>%
        {.$response$body$choices}
    
    
    
    prompt = ""
    message = "Return a random sentence"
    gpt = python$gpt_query(prompt = prompt,seed = as.integer(1))
    response = gpt$ask_gpt(message = message)
    
    gpt = python$gpt_query(prompt = prompt,seed = as.integer(1))
    response2 = gpt$ask_gpt(message = message)
    
    assertthat::are_equal(response$output,response2$output)
    
    gpt = python$gpt_query(prompt = prompt,seed = as.integer(156))
    response3 = gpt$ask_gpt(message = message)
    
    gpt = python$gpt_query(prompt = prompt,seed = as.integer(156))
    response4 = gpt$ask_gpt(message = message)
    assertthat::are_equal(response3$output,response4$output)
    
    assertthat::assert_that(response$output != response3$output)
    
    
    
    
    gpt = python$gpt_query(prompt = prompt,seed = as.integer(1))
    batch = gpt$create_batch(inputs = list('a' = message,
                                           'b' = message,
                                           'c' = message))
    Sys.sleep(100)
    
    gpt$get_jsonl_file(gpt$get_batch_info(batch$id)$output_file_id) %>%
        lapply(\(x){
            x %>% jsonlite::fromJSON(simplifyVector = FALSE) %>%
                {.$response$body$choices}
        })

    
    gpt156 = python$gpt_query(prompt = prompt,seed = as.integer(156))
    batch156 = gpt156$create_batch(inputs = list('a' = message,
                                              'b' = message,
                                              'c' = message))
    Sys.sleep(100)
    
    gpt$get_jsonl_file(gpt156$get_batch_info(batch156$id)$output_file_id) %>%
        lapply(\(x){
            x %>% jsonlite::fromJSON(simplifyVector = FALSE) %>%
                {.$response$body$choices}
        })
})
