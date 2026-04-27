library(haven)
library(data.table)

# Read the data (approx 8 mins)
edu_data <- read_dta("replication_sample_icpsr.dta")

# Write to csv (approx 1-2 mins)
fwrite(edu_data, "edu_data.csv")

