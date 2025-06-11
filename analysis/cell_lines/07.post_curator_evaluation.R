devtools::load_all()
library(magrittr)

gpt_outputs = list.files('data-raw/cell_line_data/cell_line_inputs_gpt',full.names = TRUE)
gpt_inputs = list.files('data-raw/cell_line_data/cell_line_inputs',full.names = TRUE)
second_pass_outputs = list.files('data-raw/cell_line_data/cell_line_inputs_second_pass_gpt',full.names = TRUE)

gpt_frame = readr::read_tsv('data-raw/cell_line_data/cell_line_frame.tsv')
curated_ss = googlesheets4::gs4_get("https://docs.google.com/spreadsheets/d/1zEQLGQ4SZP0oqBWSLEpt0nSHxFX922kYMXWvp-9n5LU/edit?gid=437554175#gid=437554175")
curated_frame = googlesheets4::read_sheet(curated_ss)
mistakes = curated_frame %>% dplyr::filter(information_unavailable != TRUE & curated_frame$gpt_match != "TRUE" )
1-nrow(mistakes)/nrow(gpt_frame)
