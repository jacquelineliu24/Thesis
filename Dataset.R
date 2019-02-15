# Combine province datasets for descriptive statistics
jambi <- readRDS(file = "jambi.RDA")
head(jambi)
plot(st_geometry(jambi))

# Combine study datasets for data analysis 
