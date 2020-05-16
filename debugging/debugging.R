getwd()
install.packages("roxygen2")
library(roxygen2)

roxygenise()

library(devtools)
load_all(".")

?lme_group_plot
