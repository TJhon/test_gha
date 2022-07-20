install.packages("librarian")
librarian::shelf(
    tidyverse
    , keras
    , h2o
)
install.packages("remotes")
remotes::install_github("IRkernel/IRkernel")

IRkernel::installspec()