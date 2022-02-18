#AUTHOR: Carlo R. M.A. Santagiustina
#MAIL: carlo.santagiustina@unive.it
#### RUN Plumber API ####
install_and_load <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE, repo="http://cran.r-project.org")
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("plumber","quanteda")
install_and_load (packages)

AEA_API = plumber::plumb(dir = "./PLUMBER-API/")  # Where the file describing the API is located
plumber::pr_run(
  AEA_API,
  host = "127.0.0.1",# IP address of the API (127.0.0.1 is equivalent to locahost)
  port = 8888 # port number of the API
)
