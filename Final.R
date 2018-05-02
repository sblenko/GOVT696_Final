## .R File: Final Project
## Stephanie Blenko

library(readr)
library(tidyverse)
## Load the Data Sets
Building_Permits_in_2012 <- read_csv("~/Desktop/Building_Permits_in_2012.csv")
Building_Permits_in_2013 <- read_csv("~/Desktop/Building_Permits_in_2013.csv")
Building_Permits_in_2014 <- read_csv("~/Desktop/Building_Permits_in_2014.csv")
Building_Permits_in_2015 <- read_csv("~/Desktop/Building_Permits_in_2015.csv")
Building_Permits_in_2016 <- read_csv("~/Desktop/Building_Permits_in_2016.csv")
Building_Permits_in_2017 <- read_csv("~/Desktop/Building_Permits_in_2017.csv")
Neighborhood_Clusters <- read_csv("~/Desktop/Neighborhood_Clusters.csv")

## Compile Observations into One Data Set
Building_Permits <- rbind(Building_Permits_in_2012, Building_Permits_in_2013)
Building_Permits <- rbind(Building_Permits, Building_Permits_in_2014)
Building_Permits <- rbind(Building_Permits, Building_Permits_in_2015)
Building_Permits <- rbind(Building_Permits, Building_Permits_in_2016)
Building_Permits <- rbind(Building_Permits, Building_Permits_in_2017)

## Rename Neighborhood Cluster Column
colnames(Neighborhood_Clusters)[colnames(Neighborhood_Clusters) == "NAME"] <- "NEIGHBORHOODCLUSTER"

## Left Join Neighborhood Clusters Data Set
Building_Permits <- left_join(Building_Permits, Neighborhood_Clusters, by = "NEIGHBORHOODCLUSTER")

## Remove Observations with NA in NEIGHBORHOODCLUSTERS and FEES_PAID
Building_Permits <- Building_Permits %>%
  drop_na(NEIGHBORHOODCLUSTER)
Building_Permits <- Building_Permits %>%
  drop_na(FEES_PAID)

## Creation and Filling of Gentrified Variable
Building_Permits$gentrified <- NA
Building_Permits[Building_Permits$NEIGHBORHOODCLUSTER == "Cluster 2", "gentrified"] <- 1
Building_Permits[Building_Permits$NEIGHBORHOODCLUSTER == "Cluster 9", "gentrified"] <- 1
Building_Permits[Building_Permits$NEIGHBORHOODCLUSTER == "Cluster 18", "gentrified"] <- 1
Building_Permits[Building_Permits$NEIGHBORHOODCLUSTER == "Cluster 21", "gentrified"] <- 1
Building_Permits[Building_Permits$NEIGHBORHOODCLUSTER == "Cluster 25", "gentrified"] <- 1
Building_Permits[Building_Permits$NEIGHBORHOODCLUSTER == "Cluster 27", "gentrified"] <- 1
Building_Permits$gentrified[is.na(Building_Permits$gentrified)] <- 0

## Data Visualization
ggplot(data = Building_Permits) +
  geom_bar(mapping = aes(x = PERMIT_TYPE_NAME))

ggplot(data = Building_Permits) +
  geom_bar(mapping = aes(x = WARD, fill = PERMIT_TYPE_NAME)) +
  labs(x = "Ward Number", y = "Count", title = "Permit Types")

ggplot(data = Building_Permits) +
  geom_bar(mapping = aes(x = WARD, fill = PERMIT_SUBTYPE_NAME)) +
  labs(x = "Ward Number", y = "Count", title = "Permit Subtypes")

Building_Permits <- within(Building_Permits, NEIGHBORHOODCLUSTER <- factor(NEIGHBORHOODCLUSTER, levels = names(sort(table(NEIGHBORHOODCLUSTER), decreasing = TRUE))))
ggplot(data = Building_Permits) +
  geom_bar(mapping = aes(x = NEIGHBORHOODCLUSTER)) +
  coord_flip() +
  geom_hline(yintercept = 10000) +
  labs(x = "Neighborhood Cluster", y = "Number of Permits", title = "Number of Permits per Neighborhood Cluster")

ggplot(data = Building_Permits, mapping = aes(x = WARD, y = FEES_PAID)) +
  geom_point(mapping = aes(color = PERMIT_TYPE_NAME))

ggplot(data = Building_Permits_Fees) +
  geom_histogram(mapping = aes(x = FEES_PAID, fill = PERMIT_TYPE_NAME), binwidth = 50) +
  facet_wrap(~ WARD, nrow = 2)

## ^ Do not run in console without running following command first 

Building_Permits_Fees <- subset(Building_Permits, !(FEES_PAID > 1000))

ggplot(data = Building_Permits) +
  geom_bar(mapping = aes(x = NEIGHBORHOODCLUSTER, fill = success)) +
  coord_flip() +
  labs(x = "Neighborhood Cluster", y = "Number of Permits", title = "Successfully Classified Clusters") 

ggplot(data = Building_Permits) +
  geom_bar(mapping = aes(x = PERMIT_TYPE_NAME)) +
  labs(x = "Permit Type", y = "Count", title = "Frequency of Permit Types")

ggplot(data = Building_Permits) +
  geom_bar(mapping = aes(x = PERMIT_SUBTYPE_NAME)) +
  coord_flip() +
  labs(x = "Permit Subtype", y = "Count", title = "Frequency of Permit Subtypes")
## Creation and Filling of Many Permits Variable
Building_Permits$many_permits <- NA
Building_Permits[Building_Permits$NEIGHBORHOODCLUSTER == "Cluster 6", "many_permits"] <- 1
Building_Permits[Building_Permits$NEIGHBORHOODCLUSTER == "Cluster 8", "many_permits"] <- 1
Building_Permits[Building_Permits$NEIGHBORHOODCLUSTER == "Cluster 25", "many_permits"] <- 1
Building_Permits[Building_Permits$NEIGHBORHOODCLUSTER == "Cluster 18", "many_permits"] <- 1
Building_Permits[Building_Permits$NEIGHBORHOODCLUSTER == "Cluster 26", "many_permits"] <- 1
Building_Permits[Building_Permits$NEIGHBORHOODCLUSTER == "Cluster 2", "many_permits"] <- 1
Building_Permits[Building_Permits$NEIGHBORHOODCLUSTER == "Cluster 21", "many_permits"] <- 1
Building_Permits$many_permits[is.na(Building_Permits$many_permits)] <- 0

## Further Visualizaiton
ggplot(data = Building_Permits, mapping = aes(x = many_permits, y = gentrified)) +
  geom_point(position = "jitter")
## ^ This is not helpful at all

## Table
table(gentrified = Building_Permits$gentrified, permits = Building_Permits$many_permits)

## Success Variable
Building_Permits$success <- ifelse((Building_Permits$gentrified == 1 & Building_Permits$many_permits == 1) | (Building_Permits$gentrified == 0 & Building_Permits$many_permits == 0), 1, 0)
mean(Building_Permits$success)

## Additional Fee Research

ggplot(data = Building_Permits_Fees) +
  geom_histogram(mapping = aes(x = FEES_PAID, fill = PERMIT_TYPE_NAME), binwidth = 50) +
  facet_wrap(~ WARD, nrow = 2)
## ^ Do not run in console without running following command first 

Building_Permits_Fees <- subset(Building_Permits, !(FEES_PAID > 1000))

Building_Permits_Big_Fees <- subset(Building_Permits, !(FEES_PAID < 1000))

ggplot(data = Building_Permits_Big_Fees) +
  geom_histogram(mapping = aes(x = FEES_PAID, fill = PERMIT_TYPE_NAME), binwidth = 10000) +
  facet_wrap(~ WARD, nrow = 2)

Building_Permits_Big_Fees <- within(Building_Permits_Big_Fees, NEIGHBORHOODCLUSTER <- factor(NEIGHBORHOODCLUSTER, levels = names(sort(table(NEIGHBORHOODCLUSTER), decreasing = TRUE))))
ggplot(data = Building_Permits_Big_Fees) +
  geom_bar(mapping = aes(x = NEIGHBORHOODCLUSTER, fill = gentrified)) +
  coord_flip() +
  labs(x = "Neighborhood Cluster", y = "Number of Permits", title = "Large Projects")

ggplot(data = Building_Permits) +
  geom_bar(mapping = aes(x = APPLICATION_STATUS_NAME)) +
  coord_flip()

