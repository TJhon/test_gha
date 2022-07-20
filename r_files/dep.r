install.packages("librarian")
librarian::shelf(
    tidyverse
    , keras
    , h20
)
install.packages("remotes")
devtools::install_github("IRkernel/IRkernel")

IRkernel::installspec()