library(tidyverse)
library(jsonlite)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

stims_critical <- toJSON(read.csv("ssi_normstims - critical.csv"))
stims_filler <- toJSON(read.csv("ssi_normstims - filler.csv"))
stims_exclusion <- toJSON(read.csv("ssi_normstims - exclusion.csv"))

write_file(stims_critical, "critical.json")
write_file(stims_filler, "filler.json")
write_file(stims_exclusion, "exclusion.json")
