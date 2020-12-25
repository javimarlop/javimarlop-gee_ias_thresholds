# https://github.com/ropensci/Rclean

#library(devtools)
#install_github("ROpenSci/Rclean")
#install_github("duncantl/CodeDepends")

library(Rclean)
#library(CodeDepends)
#require(Rgraphviz)

mycode<-clean('script.R','summer.lda23.predict')
keep(mycode, file = "lda_summer.R")

