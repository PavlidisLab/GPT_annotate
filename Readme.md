Readme
================

## Readme

This repository includes code used for “Application of large language
models to the annotation of cell lines and mouse strains in genomics
data”.

### Installation

Clone the repository and run the project, make sure devtools is
installed

``` r
install.packages('devtools')
```

The accompanying package lists all dependencies, all of which will be
installed if you do

``` r
devtools::install_deps(".")
```

Most scripts depend on functions included in the package which are
loaded by doing in the beginning

``` r
devtools::load_all()
```

When this line is run for the first time this will also create the
python virtual environment used to interact with the OpenAI API. If you
have problems with this step you can run the setup process manually by
using `python_setup` function.

### Running the analysis

Analysis directory contains the code that annotates and evaluates the
annotations in `analysis/cell_lines` and `analysis/strains` directories
respectively. Both rely out the output of
`analysis/_downloads/download.R` so do

``` r
source("analysis/_downloads/download.R")
```

before continuing.

Scripts in the cell_lines and strains directories are meant to be run
sequentially, however tables containing annotations made by GPT-4o and
curator evaluations are included.

OpenAI API key is expected to be contained at `~/openai/access_key.txt`
