#AUTHOR: Carlo R. M.A. Santagiustina
#MAIL: carlo.santagiustina@unive.it
#### RUN Plumber API ####
install_and_load <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("rmarkdown","shiny","learnr")
install_and_load (packages)

rmarkdown::run("./LEARNR-TUTORIAL/AEAtutorial.Rmd", shiny_args = list(host = "127.0.0.1",port = 8989))
