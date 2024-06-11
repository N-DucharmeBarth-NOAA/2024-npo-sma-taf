# Install R packages
install.packages("languageserver")
install.packages("jsonlite") 
install.packages("renv")
# Restore the renv packages on container build
renv::restore(prompt=FALSE)
