devtools::load_all()
mil = 1000000

input_price = 2/mil
output_price = 8/mil
reticulate::source_python(system.file('gpt.py',package= 'GPTests'),python)
gpt = python$gpt_query(prompt = readLines('analysis/delegator/prompt') %>% 
                           paste(collapse = '\n'),
                       response_format = delegator_output)

input_list = list.files('data-raw/delegator_data/delegator_inputs/',full.names = TRUE)
input_list = input_list %>% rev()
paths = paste0(dirname(input_list),"_gpt")
files = basename(input_list)
already_ran = file.exists(file.path(paths,files))
input_list = input_list[!already_ran]
while(!all(already_ran)){
    
    run_batch(input_list[[67]],gpt,'_gpt')
    paths = paste0(dirname(input_list),"_gpt")
    files = basename(input_list)
    already_ran = file.exists(file.path(paths,files))
    input_list = input_list[!already_ran]
    input_list =  input_list %>% rev()
}
