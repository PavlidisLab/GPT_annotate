# this runs multiple batch jobs and may take up to 24 hours per batch to process
# typically batches finish processing much sooner.

devtools::load_all()
input_price = 2.50/1000000
output_price = 10/1000000
python = new.env()
reticulate::source_python(system.file('gpt.py',package= 'GPTests'),python)
gpt = python$gpt_query(gpt_model = "gpt-4o",prompt = readLines('analysis/cell_lines/prompt') %>% paste(collapse = '\n'),
                       response_format = cell_line_output)


input_list = list.files('data-raw/cell_line_data/cell_line_inputs/',full.names = TRUE)
input_list = input_list %>% rev()
paths = paste0(dirname(input_list),"_gpt")
files = basename(input_list)
already_ran = file.exists(file.path(paths,files))


while(!all(already_ran)){
    
    run_batch(input_list[[1]],gpt)
    
    paths = paste0(dirname(input_list),"_gpt")
    files = basename(input_list)
    already_ran = file.exists(file.path(paths,files))
    input_list = input_list[!already_ran]
}

