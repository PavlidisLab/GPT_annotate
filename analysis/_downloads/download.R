options(timeout=60*10)


download.file('https://github.com/ontodev/robot/releases/download/v1.9.6/robot.jar',destfile = 'robot.jar')


download.file("https://purl.obolibrary.org/obo/clo.owl",
              destfile = 'data-raw/ontologies/CLO.owl')
system('java -jar robot.jar convert --input data-raw/ontologies/CLO.owl --check false --format obo --output data-raw/CLO.obo')


download.file("https://www.ebi.ac.uk/efo/efo.obo",
              destfile = 'data-raw/ontologies/EFO.obo')


download.file("https://raw.githubusercontent.com/PavlidisLab/TGEMO/master/TGEMO.OWL",
              destfile = 'data-raw/ontologies/TGMO.OWL')
system('java -jar robot.jar convert --input data-raw/ontologies/TGMO.OWL --format obo --output data-raw/ontologies/TGMO.obo')
