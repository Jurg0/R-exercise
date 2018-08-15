library(dplyr)
library(reshape2)
library(ggplot2)
library(ggbeeswarm)

reap_data <- function(filename) {
  # Read the data from csv format
  a <- read.csv(filename, header = TRUE, sep = ";")
  
  # Not really necessary but creates a small data frame with standard deviations
  b <- group_by(a, Concentration, replicate) %>%
    summarise(meanA = mean(Influenza.A), meanB = mean(Influenza.B), stdevA = sd(Influenza.A), stdevB = sd(Influenza.B))
  
  # gather or melt data for handling
  c <- melt(a, id.vars=c("Concentration", "replicate"))
  c
}

# boxplots of all concentrations and replicates, x-axis with factors
# ggplot(data= subset(c, replicate == 1)) +
#   geom_boxplot( aes(x=factor(Concentration), y=value, fill=factor(variable)), position=position_dodge(1)) +
#   facet_wrap(~ variable) +
#   theme_minimal()

# For repeatability testing take only the Influenza A value of 1 replicate at concentration of 0.5 µg/mL
# Then plot as boxplot
reap_box <- function(datas) {
  reap_dat <- subset(datas, Concentration == "0,5")
  reap_dat <- subset(reap_dat, replicate == 1)
  pdf("repeatability-box.pdf", width = 5, height = 7)
  print(
    ggplot(mutate(reap_dat, variable = factor(variable)), fill = variable) +
      geom_boxplot(aes(x = variable, y = value)) +
      #ylim(0, 800) + # setting the limits of y axis can be useful in certain scenarios
      theme(axis.title.x = element_blank()) +
      ylab("max_value") +
      ggtitle("Repeatability")
  )
  dev.off()
}

# Reapeatability testing with histogram and density plot (does not look good with this data)
reap_dens <- function(datas) {
  reap_dat <- subset(datas, Concentration == "0,5")
  reap_dat <- subset(reap_dat, replicate == 1)
  pdf("repeatability-dens.pdf", width = 5, height = 4)
  print(
    ggplot(reap_dat, aes(x=value)) + 
      geom_histogram(aes(y=..density..),
                     binwidth=30,
                     colour="black", fill="white") +
      geom_density(alpha=.2, fill="#FF6666") +
      facet_wrap(~ variable) +
      theme(axis.title.x = element_blank()) +
      ylab("max_value") +
      ggtitle("Repeatability")
  )
  dev.off()
}

# Reapeatability testing with beeswarm plot
reap_bee <- function(datas) {
  reap_dat <- subset(datas, Concentration == "0,5")
  reap_dat <- subset(reap_dat, replicate == 1)
  pdf("repeatability-bee.pdf", width = 5, height = 7)
  print(
    ggplot(mutate(reap_dat, variable = factor(variable)), fill = variable) +
      geom_quasirandom(aes(x = variable, y = value)) +
      #ylim(0, 800) +
      theme(axis.title.x = element_blank()) +
      ylab("max_value") +
      ggtitle("Repeatability")
  )
  dev.off()
}

# Reproducibility: between LFIA replicates
# data from Influenza benchmarking at Senova (influenza_bench.csv)
# with beeswarm
repr_bee <- function(datas) {
  repr_dat <- subset(datas, Concentration == "0,5")
  pdf("repr-bee.pdf", width = 7, height = 3)
  print(
    ggplot(mutate(repr_dat, variable = factor(variable))) +
      geom_quasirandom(aes(x = replicate, y = value)) +
      facet_wrap(~ variable) +
      xlab("replicate") +
      ylab("max_value") +
      ggtitle("Reproducibility: assay replicates")
  )
  dev.off()
}

# with boxplots
repr_box <- function(datas) {
  repr_dat <- subset(datas, Concentration == "0,5")
  pdf("repr-box.pdf", width = 7, height = 3)
  print(
    ggplot(mutate(repr_dat, variable = factor(variable))) +
      geom_boxplot(aes(x = factor(replicate), y = value)) +
      facet_wrap(~ variable) +
      xlab("replicate") +
      ylab("max_value") +
      ggtitle("Reproducibility: assay replicates")
  )
  dev.off()
}

# with histogram and density plot
repr_dens <- function(datas) {
  repr_dat <- subset(datas, Concentration == "0,5")
  pdf("repr-dens.pdf", width = 5, height = 3)
  print(
    ggplot(repr_dat, aes(x=value)) + 
      geom_histogram(aes(y=..density..),
                     binwidth=30,
                     colour="black", fill="white") +
      geom_density(alpha=.2, fill="#FF6666") +
      facet_wrap(~ variable, scales = "free_x") +
      theme(axis.title.x = element_blank()) +
      ylab("max_value") +
      ggtitle("Reproducibility: assay replicates")
  )
  dev.off()
}

# Reproducibility: between Readers
# data from DOA saliva testing in DOA23_saliva_-50_1.csv 
# /Production/Companywide/11_RnD/07_Quality Control/04_QC product release/02_kPlex/2017-11-20 kPlex DOA P02 saliva_presentation
repr_data <- function(filename) {
  # Read the data from csv format
  a <- read.csv(filename, header = TRUE, sep = ";")
  
  # gather or melt data for handling
  c <- melt(a, id.vars=c("Concentration", "assay", "Reader"))
  c
}

# with beeswarm
repr_bee_read <- function(datas) {
  repr_dat <- subset(datas, assay == 1)
  pdf("repr-bee-read.pdf", width = 7, height = 3)
  print(
    ggplot(mutate(repr_dat, variable = factor(variable), Reader = factor(Reader))) +
      geom_quasirandom(aes(x = Reader, y = value), groupOnX = TRUE) +
      facet_grid(~ variable) +
      xlab("Reader ID") +
      ylab("max_value") +
      ggtitle("Reproducibility: between Readers")
  )
  dev.off()
}

# with boxplots
repr_box_read <- function(datas) {
  repr_dat <- subset(datas, assay == 1)
  pdf("repr-box-read.pdf", width = 7, height = 3)
  print(
    ggplot(mutate(repr_dat, variable = factor(variable), Reader = factor(Reader))) +
      geom_boxplot(aes(x = Reader, y = value)) +
      facet_grid(~ variable) +
      xlab("Reader ID") +
      ylab("max_value") +
      ggtitle("Reproducibility: between Readers")
  )
  dev.off()
}

# with histogram and density plot
repr_dens_read <- function(datas) {
  repr_dat <- subset(datas, assay == 1)
  pdf("repr-dens-read.pdf", width = 7, height = 5)
  print(
    ggplot(repr_dat, aes(x=value)) + 
      geom_histogram(aes(y=..density..),
                     binwidth=10,
                     colour="black", fill="white") +
      geom_density(alpha=.2, fill="#FF6666") +
      facet_grid(Reader ~ variable, scales = "free_x") +
      theme(axis.title.x = element_blank()) +
      ylab("max_value") +
      ggtitle("Reproducibility: between Readers")
  )
  dev.off()
}