# ISEED argument Extractor & Aggregator (ISEED-AEA)

## Summary

This repository contains the API of the **ISEED-AEA**  (PLUMBER-API folder), related tutorials (LEARNR-TUTORIAL folder) and launcher files, which can serve as a testing prototype (alpha version: 0.2). **ISEED-AEA** has been developed by Carlo R. M. A. Santagiustina and is mostly based on regular expressions (RegEx).
The API is coded in R using the `plumber` library.

This API, and its causal argument extraction methods, have been first presented and employed in the following (preprint) paper:

- Santagiustina CRMA, Warglien M (2022) The architecture of partisan debates: The online controversy on the no-deal Brexit. PLoS ONE 17(6): e0270236. [https://doi.org/10.1371/journal.pone.0270236](https://doi.org/10.1371/journal.pone.0270236).

Please cite this paper to refer to the digital methods contained in the API.

In particular, **ISEED-AEA** contains a set of functions for extracting (through RegEx) causal statements and other forms of argumentation from (noisy) textual data, like short posts from social media.

At the present time (Feb-2022) two endpoints are available:

- `/get_cause_effect_string` (`GET` endpoint) *Extracts cause-effect relations from a sentence using a set of regular expressions, no NLP tool is employed so the function is not aware of Part-Of-Speech or Dependency-Relations among words. This function was designed to be used with short texts from social media.*

- `/get_if_then_string` (`GET` endpoint) *Extracts if-then relations from a sentence using a set of regular expressions, no NLP tool is employed so the function is not aware of Part-Of-Speech or Dependency-Relations among words. This function was designed to be used with short texts from social media. The text string must be a single sentence and must have been already pre-processed to remove non latin-1 characters like emojis, as well as URLs. Remember to set the correct language parameter ("en" by default).*


For a full list of endpoint parameters visit the auto-generated Swagger UI of the **ISEED-AEA** API available at the following link (remeber to start the API before visiting this URL):

- [http://127.0.0.1:8888/__docs__/](http://127.0.0.1:8888/__docs__/)

More endpoints and argument types will be made available in future versions of the **ISEED-AEA**.

## Funding information 

This API was developed for the [ISEED](https://iseedeurope.eu/) project, which has received funding from the European Union’s Horizon 2020 SOCIETAL CHALLENGES programme under Grant Agreement No. 960366. This API and its contents reflect only the author’s views. The European Commission is not responsible for any use that may be made of the information it contains.

ISEED website: https://iseedeurope.eu/

## Install Instructions 

1) copy this folder in your local machine. If the `git` library is installed in your machine, you can use the following command: `git clone https://github.com/carlosantagiustina/ISEED-AEA`

2)  install [R](https://cran.r-project.org/) (version 4.0.5 or later) and [RStudio](https://www.rstudio.com/products/rstudio/) (version 1.2 or later). 

3) install the latest versions of the following R libraries from CRAN: [plumber](https://www.rplumber.io/),[dplyr](https://dplyr.tidyverse.org/),[premises](https://rstudio.github.io/promises/articles/motivation.html)

4) start the API through RStudio by `Sourcing as local job` (command on top-right corner of RStudio scripting window) the R source file named: `RUN_API_port8888.R`

5) visit the SWAGGER UI of the API at the following address to test it: [http://127.0.0.1:8888/__docs__/](http://127.0.0.1:8888/__docs__/)

## Performance and parallelization

To exploit the asynchronous multisession functionality of plumber, which is necessary to accept and process contemporaneously multiple requests to the API, remember to run the API from the R script `RUN_API_port8888.R`. If you run directly the `plumber.R` file from RStudio your API will not be able to process multiple requests at the same time.

### Interactive tutorial with learnR

start the interactive tutorial through RStudio by `Sourcing as local job` (command on top-right corner of RStudio scripting window) the R source file named: `RUN_TUTORIAL_port8989.R`
Then visit the page of the interactive tutorial at the following address:[http://127.0.0.1:8989](http://127.0.0.1:8989)


# Additional information

For additional information please contact:

* Carlo R. M. A. Santagiustina [carlo.santagiustina@unive.it](carlo.santagiustina@unive.it)
