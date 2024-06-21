# Load necessary libraries
library(vegan)
library(dplyr)
library(tidyr)

# Read the ASV table
asv_table <- read.csv('C:/Users/Robin Burgold/tubCloud/LGC-16S-2022/Metalpaper/ASV.tab', sep='\t', header=TRUE, row.names=1)

# Replace commas with dots in the data for proper numeric conversion
asv_table[] <- lapply(asv_table, function(x) as.numeric(gsub(",", ".", x)))

# Transpose the ASV table to have samples as rows and ASVs as columns
asv_table_t <- t(asv_table)

# Create sample metadata
sample_metadata <- data.frame(
  SampleID = rownames(asv_table_t),
  SamplingDate = rep(c("T0", "T1", "T2", "T3", "T4", "T5"), each = 6),
  SamplingLocation = rep(c("A", "B"), each = 3, times = 6)
)

# Merge the metadata with the ASV table
asv_table_t <- as.data.frame(asv_table_t)
asv_table_t <- cbind(SampleID = rownames(asv_table_t), asv_table_t)
data_merged <- merge(asv_table_t, sample_metadata, by = "SampleID")


# Calculate richness
richness <- rowSums(data_merged[,-c(1, ncol(data_merged)-1, ncol(data_merged))] > 0)

# Add richness to the metadata
data_merged$Richness <- richness
