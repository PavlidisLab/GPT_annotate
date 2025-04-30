options(timeout=240)
library(gemma.R)
library(dplyr)

# get annotations of datasets with cell line annotations ----
cell_line_datasets = get_datasets(filter = 'allCharacteristics.categoryUri = http://purl.obolibrary.org/obo/CLO_0000031') %>% get_all_pages()
saveRDS(cell_line_datasets,file = 'data-raw/cell_line_datasets.rds')
cell_line_datasets$experiment.ID %>% pbapply::pblapply(function(x){
    get_dataset_annotations(x)
}) -> annotations
names(annotations) = cell_line_datasets$experiment.shortName
saveRDS(annotations,file = 'data-raw/cell_line_annotations.rds')


# get cell line annotations annotations of datasets with cell line annotations
cell_line_annotations = annotations %>% lapply(function(x){
    x %>% dplyr::filter(class.URI == 'http://purl.obolibrary.org/obo/CLO_0000031')
})
saveRDS(cell_line_annotations,file = 'data-raw/cell_line_annotations2.rds')
cell_line_annots = do.call(rbind,cell_line_annotations)


# get CLO and EFO ontologies ---------
download.file("https://purl.obolibrary.org/obo/clo.owl",
              destfile = 'data-raw/ontologies/CLO.owl')
system('java -jar robot.jar convert --input data-raw/ontologies/CLO.owl --check false --format obo --output data-raw/CLO.obo')


download.file("https://www.ebi.ac.uk/efo/efo.obo",
              destfile = 'data-raw/ontologies/EFO.obo')



