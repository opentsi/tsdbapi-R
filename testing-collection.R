library(devtools)
library(tsdbapi)

devtools::load_all()

collection <- "ch.kof.jobtracker"
read_collection_ts(collection=collection, owner = "public") # searching for specific thing


# checkign if commits worked 
devtools::load_all()
keys <- read_dataset_keys("ch.fso.hesta")
length(keys)
all_vintages <- read_ts_history(keys) # start: 15:13

all_vintages <- read_dataset_ts_history("ch.fso.hesta")
