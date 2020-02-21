##Code to extract GBIF - merge, split, bend datasets-----------------
#Partial code from Marlon Cobos
# Multiple species GBIF ---------------
# loading needed package
if(!require(rgbif)){
  install.packages("rgbif")
  library(rgbif)
}

setwd("D:\\Mariannas Business\\Orthoptera_Olly\\Missing_species") # Your folder

spvector <- as.character(read.csv("species_missing.csv")[, 1])# binomial names

## Getting info species by species 
occ_count <- list() # object to save info on number of georeferenced records per species 
i=1
for (i in 1:length(spvector)) {
  sps <- try(name_lookup(query = spvector[i], rank = "species", 
                         return = "data", limit = 100), silent = TRUE) # information about the species
  
  sps_class <- class(sps)
  
  # avoiding errors from GBIF (e.g., species name not in GBIF)
  if(sps_class[1] == "try-error") {
    occ_count[[i]] <- c(Species = spvector[i], keys = 0, counts = 0) # species not in GBIF
    cat("species", spvector[i], "is not in the GBIF database\n")
    
  }else {
    keys <- sps$key # all keys returned
    counts <- vector() # object to save info on number of records per key
    
    for (j in 1:length(keys)) { # testing if keys return records
      counts[j] <- occ_count(taxonKey = keys[j], georeferenced = TRUE) 
    }
    
    if (sum(counts) == 0) { # if no info, tell the species
      occ_count[[i]] <- c(Species = spvector[i], keys = "all", counts = 0) # species not in GBIF
      cat("species", spvector[i], "has no goereferenced data\n")
      
    }else { # if it has info, use the key with more records, which is the most useful
      if (length(keys) == 1) { # if it is only one key
        key <- keys # detecting species key 
        occ_count[[i]] <- cbind(spvector[i], counts) # count how many records
        
      }else { # if its more than one key
        keysco <- cbind(keys, counts)
        keysco <- keysco[order(keysco[, 2]), ]
        key <- keysco[dim(keysco)[1], 1] # detecting species key that return information
        occ_count[[i]] <- c(Species = spvector[i], keysco[dim(keysco)[1], ])# count how many records
      }
      
      occ <- try(occ_search(taxonKey = key, return = "data", limit = 10000), silent = TRUE) # getting the data from GBIF
      occ_class <- class(occ)
      
      # avoiding errors from GBIF
      while (occ_class[1] == "try-error") {
        occ <- try(occ_search(taxonKey = key, return = "data", limit = 10000), silent = TRUE) # getting the data from GBIF
        occ_class <- class(occ)
        
        if(occ_class[1] != "try-error") {
          break()
        }
      }
      
      # following steps
      occ_g <- occ
      occ_g <- occ_g[, c(1, 2, 4, 3, 5:dim(occ_g)[2])] # reordering longitude and latitude
      
      # keeping only unique georeferenced records. IF NO FILTERING IS NEEDED, PUT A # IN FRONT OF THE NEXT 3 LINES
      occ_g <- occ_g[!is.na(occ_g$decimalLatitude) & !is.na(occ_g$decimalLongitude), ] # excluding no georeferences
      occ_g <- occ_g[!duplicated(paste(occ_g$name, occ_g$decimalLatitude, # excluding duplicates
                                       occ_g$decimalLongitude, sep = "_")), ]
      
      # writting file
      file_name <- paste(gsub(" ", "_", spvector[i]), "csv", sep = ".") # csv file name per each species
      write.csv(occ_g, file_name, row.names = FALSE) # writing inside each genus folder
      
      cat(i, "of", length(spvector), "species\n") # counting species per genus 
    }
  }
}

genus_data <- do.call(rbind, occ_count) # making the list of countings a table
genus_data <- data.frame(genus_data[, 1], genus_data[, 2], as.numeric(genus_data[, 3])) # making countings numeric
names(genus_data) <- c("Species", "Key", "N_records") # naming columns

#Standardize the csv files-------------------------------------------
##Extracting Name, Long, Lat from all csv files 
##When the list of species is too long (e.g.3000), it stops and I have to re-run the code to finish up 
setwd('D:/Mariannas Business/Orthoptera_Olly/Species_gbif') ##setting path
path <- "D:/Mariannas Business/Orthoptera_Olly/Species_gbif"
filenames <- list.files(path= path, full.names=F)
spvector <- filenames
for (i in 125:length(spvector)) {
  all <- read.csv(paste(spvector[i], sep = ","))
  myvars <- c("name", "decimalLongitude", "decimalLatitude")
  table <- all[myvars]
  write.csv(table, paste('D:/Mariannas Business/Orthoptera_Olly/sp_enm/', spvector[i], sep = ""),row.names = FALSE)
} 

#Summng all occ so we can plot on map------
path <- "D:\\Mariannas Business\\Orthoptera_Olly\\Processing Occ Data\\Orthoptera layers cropped\\Final_Species"
filenames <- list.files(path= path, full.names=TRUE)
exclude <- c("D:/Mariannas Business/Orthoptera_Olly/Orthoptera layers cropped/less than 20 loc")
spvector <- filenames[!filenames %in% exclude]

filenames <- spvector

setwd('D:\\Mariannas Business\\Orthoptera_Olly\\Orthoptera layers cropped')

x <- read.csv('D:\\Mariannas Business\\Orthoptera_Olly\\Orthoptera layers cropped\\Yersinella raymondii_thin.csv')
colnames(x) <- c('name', 'decimalLongitude', 'decimalLatitude' )
write.csv(x, 'D:\\Mariannas Business\\Orthoptera_Olly\\Orthoptera layers cropped\\Yersinella raymondii_thin.csv', row.names = F)
head(x)

All <- lapply(filenames,function(filenames){
  print(paste("Merging",filenames,sep = " "))
  read.csv(filenames)
})

head(All)
df <- do.call(rbind.data.frame, All) #rbind all files
head(df)

write.csv(df,'D:\\Mariannas Business\\Orthoptera_Olly\\Processing Occ Data\\Orthoptera layers cropped\\Final_Species\\Final_species.csv', row.names = F)

#Prepare folders------------
x <- read.csv('D:\\Mariannas Business\\Orthoptera_Olly\\Processing Occ Data\\Orthoptera layers cropped\\Final_Species\\Final_species.csv')
head(x)
sp <- unique(x$name)
n = 20 # at least 20  records 
cols = 1:3 # columns extracted from dataset - species, long, lat
setwd('D:\\Mariannas Business\\Orthoptera_Olly\\Processing Occ Data\\Orthoptera layers cropped\\Final_Species\\Species_folder')

for (i in 1:length(sp)) {
  sptable <- x[x$name == sp[i], cols]
    if (dim(sptable)[1] >= n) {
       xir.create(paste(sp[i], collapse = "_"))
    write.csv(sptable, paste(paste(sp[i], collapse = '_'), '\\',
                             paste(sp[i], collapse = '_'), ".csv", sep = ""), row.names = FALSE)
  }
  
}


#Correcting points for maxent run------------
devtools::install_github("marlonecobos/kuenm")
install.packages('kuenm')
install.packages('digest')
library(kuenm)
library(raster)

spvec <- dir()
exclude <- c("Variables", "G_variables")
spvector <- spvec[!spvec %in% exclude]
sp1 <- spvector#[1:8] # others are [27:63] and [64:78]
i=1

#Fixing points to fall inside raster layer-------------
for (i in 1:length(sp1)) {
  x <- read.csv(paste(spvector[i], "/", spvector[i], ".csv", sep=""))
  r <- raster(paste(spvector[i], "/", "M_variables", "/", "set1","/", "cveloc.asc", sep=""))
  x1 <- kuenm::kuenm_toclosest(x, longitude = "Long", latitude = "Lat", raster.layer = r, limit.distance = 50)
  x1 <- x1[,1:3]
  write.csv(x1, paste(spvector[i], "/", paste(spvector[i], "_joint.csv", sep = ""), sep = ""), row.names = F)
}

#Splitting occurences: Test, training and join------------
# 2. preparing occurrences----------------------------------------
spvec <- dir()
exclude <- c("Variables", "G_variables")
spvector <- spvec[!spvec %in% exclude]

for (i in 1:length(spvector)) {
  all <- read.csv(paste(spvector[i], "/", paste(spvector[i], ".csv", sep = ""), sep = ""))
  all <- unique(all)
  
  
  all$check <- paste(all[,2], all[,3], sep = "_")
  train <- all[sample(nrow(all), round((length(all[,1])/4 *3))), ]
  test <- all[!all[,4] %in% train[,4], ]
  
  all$check <- NULL
  train$check <- NULL
  test$check <- NULL
  
  write.csv(all, paste(spvector[i], "/", paste(spvector[i], "_joint.csv", sep = ""), sep = ""),
            row.names = FALSE)
  write.csv(train, paste(spvector[i], "/", paste(spvector[i], "_train.csv", sep = ""), sep = ""),
            row.names = FALSE)
  write.csv(test, paste(spvector[i], "/", paste(spvector[i], "_test.csv", sep = ""), sep = ""),
            row.names = FALSE)
}




#Preparing Occurences-----------------------
# 1. preparing occurrences----------------------------------------
setwd("C:\\Users\\admin\\Desktop\\Mariannas Business\\Orthoptera_Olly\\Test\\Species")
spvec <- dir()
exclude <- c("Variables", "G_variables")
spvector <- spvec[!spvec %in% exclude]
i=1
for (i in 1:length(spvector)) {
  all <- read.csv(paste(spvector[i], "/", paste(spvector[i], ".csv", sep = ""), sep = ""))
  all <- unique(all)
  
  all$check <- paste(all[,2], all[,3], sep = "_")
  train <- all[sample(nrow(all), round((length(all[,1])/4 *3))), ]
  test <- all[!all[,4] %in% train[,4], ]
  
  all$check <- NULL
  train$check <- NULL
  test$check <- NULL
  
  write.csv(all, paste(spvector[i], "/", paste(spvector[i], "_joint.csv", sep = ""), sep = ""),
            row.names = FALSE)
  write.csv(train, paste(spvector[i], "/", paste(spvector[i], "_train.csv", sep = ""), sep = ""),
            row.names = FALSE)
  write.csv(test, paste(spvector[i], "/", paste(spvector[i], "_test.csv", sep = ""), sep = ""),
            row.names = FALSE)
}

#Correlation of variables ------------------------
#####
# Evaluation of variables correlation
#####

pcakages <- c("raster", "rgdal")
req_packages <- pcakages[!(pcakages %in% installed.packages()[, "Package"])]
if (length(req_packages) > 0) {
  install.packages(req_packages, dependencies = TRUE)
}
sapply(pcakages, require, character.only = TRUE)

# defining working directory
setwd("C:\\Users\\admin\\Desktop\\Mariannas Business\\Orthoptera_Olly\\Test\\Env\\SelectVariables\\Asc") # change this to your working directory
path <- "D:\\Species_models\\G_variables\\current\\"
# reading data
variables_list <- list.files(path = path, pattern = ".asc", # vector of variables
                             full.names = TRUE)

variables <- stack(variables_list) # stack of variables

# getting data from the variables
variables_values <- na.omit(values(variables))

# sample of 10000 values if more pixels exist (optional)
if (dim(variables_values)[1] > 10000) {
  variables_values <- variables_values[sample(1:nrow(variables_values), 10000), ] 
} 

# correlation matrix calculation
correlation_matrix <- cor(variables_values)

# saving correlation matrix
write.csv(correlation_matrix, "D:\\correlationEuropeArea.csv",
          row.names = TRUE)

# detecting correlated varaibles more easily
correlation_matrix1 <- correlation_matrix # making other table with results

max_cor <- 0.8 # maximum value of correlation allowed

for (i in 1:dim(correlation_matrix1)[2]) { #correlated values will turn into 2 for easier detection
  for (j in 1:dim(correlation_matrix1)[1]) {
    correlation_matrix1[j, i] <- ifelse(correlation_matrix1[j, i] > max_cor | correlation_matrix1[j, i] < -max_cor, 
                                        2, correlation_matrix1[j, i])
  }
}

# #checking the table
View(correlation_matrix1) # selection should be done manually, 2 = correlated

# saving correlation matrix
write.csv(correlation_matrix1, "variables_correlation_matrix2.csv",
          row.names = TRUE)

# selecting variables and writing them in a new directory
names(variables) # names

#####
# Evaluation of variables correlation
#####

# data needed 
## Environmental raster layers, in this example we will use a set of ascii 
## layers located in the folder (bio) in our working directory. Our layers are 
## 15 bioclimatic varaibles. These varaibles can be in other formats as well,
## but make sure that you use the appropriate extension when reading them.

## Note: All variables must have the same projection, extent, and resolution.
## We suggest to work with Geographic projections WGS84, with no planar projection.

# loading needed packages (packages will be automatically installed if required)
pcakages <- c("raster", "rgdal")
req_packages <- pcakages[!(pcakages %in% installed.packages()[, "Package"])]
if (length(req_packages) > 0) {
  install.packages(req_packages, dependencies = TRUE)
}
sapply(pcakages, require, character.only = TRUE)

# defining working directory
setwd("D:/Marlon/Variables_processing/") # change this to your working directory
path <- "C:\\Users\\admin\\Desktop\\Crusti_New\\variables_cor\\"
# reading data
varaibles_list <- list.files(path = path, pattern = ".asc", # vector of variables
                             full.names = TRUE)

variables <- stack(varaibles_list) # stack of variables

# getting data from the variables
variables_values <- na.omit(values(variables))

# sample of 10000 values if more pixels exist (optional)
if (dim(variables_values)[1] > 10000) {
  variables_values <- variables_values[sample(1:nrow(variables_values), 10000), ] 
} 

# correlation matrix calculation
correlation_matrix <- cor(variables_values)

# saving correlation matrix
write.csv(correlation_matrix, "variables_correlation_matrix.csv",
          row.names = TRUE)

# detecting correlated varaibles more easily
correlation_matrix1 <- correlation_matrix # making other table with results

max_cor <- 0.9 # maximum value of correlation allowed

for (i in 1:dim(correlation_matrix1)[2]) { #correlated values will turn into 2 for easier detection
  for (j in 1:dim(correlation_matrix1)[1]) {
    correlation_matrix1[j, i] <- ifelse(correlation_matrix1[j, i] > max_cor | correlation_matrix1[j, i] < -max_cor, 
                                        2, correlation_matrix1[j, i])
  }
}

# #checking the table
View(correlation_matrix1) # selection should be done manually, 2 = correlated

# saving correlation matrix
setwd('C:\\Users\\admin\\Desktop\\Crusti_New\\variables_cor')
write.csv(correlation_matrix1, "variables_correlation_matrix2.csv",
          row.names = TRUE)
dir()
# selecting variables and writing them in a new directory
names(variables) # names

selected_variables <- variables[[c("bio_1", "bio_4", "bio_7", # select only non-correlated ones
                                   "bio_12", "bio_15", "bio_17")]] 

variable_names <- paste("Non_correlated_variables/", names(selected_variables), # names of variables
                        ".asc", sep = "") 

dir.create("Non_correlated_variables") # create the folder for saving these variables

for (i in 1:length(raster::unstack(selected_variables))) { # writing the selected variables
  raster::writeRaster(selected_variables[[i]], filename = variable_names[i], 
                      format = "ascii")
}selected_variables <- variables[[c("bio_1", "bio_4", "bio_7", # select only non-correlated ones
                                    "bio_12", "bio_15", "bio_17")]] 

variable_names <- paste("Non_correlated_variables/", names(selected_variables), # names of variables
                        ".asc", sep = "") 

dir.create("Non_correlated_variables") # create the folder for saving these variables

for (i in 1:length(raster::unstack(selected_variables))) { # writing the selected variables
  raster::writeRaster(selected_variables[[i]], filename = variable_names[i], 
                      format = "ascii")
}


#Load variables 
#cropping layers in batch------------------
#######################################################################################
# Masking and saving layers--------------------------
##################
setwd('WHERE YOUR LAYERS ARE')
require(rgdal)
shape <- readOGR(dsn = "D:/Processing_Env_Data",layer = "Area") #SHAPEFILE YOU CREATED FOR PROJECTION AREA
path <- "D:/alt_2-5m_bil"
varaibles_list <-list.files(path = path, pattern = ".bil", # vector of variables
                            full.names = TRUE)
variables <- stack(varaibles_list) #create a stack

#variables <- getData("worldclim", var = "bio", res = "2.5")[[c(1, 12)]]

# masking variables
var_mask <- mask(crop(variables, shape), shape)

## names for layers
rnames <- paste0("D:/alt_2-5m_bil/", names(variables), ".asc") # users select the format

## saving layers in new folder
sav <- lapply(1:nlayers(var_mask), function(x) {
  writeRaster(var_mask[[x]], filename = rnames[x], format = "ascii", overwrite=T) # change format accordingly
})

#Splitting records---------------


setwd('D:\\Species_models')
spvec <- dir()
exclude <- c("Variables", "G_variables")
spvector <- spvec[!spvec %in% exclude]
sp1 <- spvector#[1:8] # others are [27:63] and [64:78]
i=1

for (i in 1:length(sp1)) {
#n <- read.csv(paste(spvector[i], "/", paste(spvector[i], "_joint.csv", sep = ""), sep = ""))
kuenm_occsplit(paste0(sp1[i], "/", sp1[i], '.csv'),train.proportion = 0.70)
}




#Creating Ms--------------------------

devtools::install_github("marlonecobos/kuenm", force=T)
library(kuenm)
library(raster)
library(rgeos)
setwd("D:\\Species_models")
spvec <- dir()
exclude <- c("Variables", "G_variables")
spvector <- spvec[!spvec %in% exclude]

scenarios <- c("current")

var_list <- list(stack(list.files("Variables/set1/current", pattern = ".asc", full.names = TRUE)), 
                 stack(list.files("Variables/set2/current", pattern = ".asc", full.names = TRUE)), 
                 stack(list.files("Variables/set3/current", pattern = ".asc", full.names = TRUE)),
                 stack(list.files("Variables/set4/current", pattern = ".asc", full.names = TRUE)))

var_names <- list(list.files("Variables/set1/current", pattern = ".asc", full.names = FALSE),
                  list.files("Variables/set2/current", pattern = ".asc", full.names = FALSE), 
                  list.files("Variables/set3/current", pattern = ".asc", full.names = FALSE), 
                  list.files("Variables/set4/current", pattern = ".asc", full.names = FALSE))

sets <- c("set1", "set2","set3", "set4")


for (i in 1:length(spvector)) {
  # reading species data
  sp <- read.csv(paste(spvector[i], "/", paste(spvector[i],"_joint.csv", sep = ""), sep = ""))
  sp_nodup <- unique(sp)
  
  # create buffer
  WGS84 <- sp::CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  occ_pr <- sp::SpatialPointsDataFrame(coords = sp_nodup[, 2:3], data = sp_nodup,
                                       proj4string = WGS84)
  
  buff_area <- rgeos::gBuffer(occ_pr, width = 0.5)
  #buff_areap <- raster::disaggregate(buff_area)
  
  # folder for Ms
  dir.create(paste(spvector[i], "M_variables", sep = "/"))
  
  for (j in 1:length(var_list)) {
    set <- var_list[[j]]
    
    # folder for sets
    infolder <- paste(spvector[i], "M_variables", sets[j], sep = "/")
    dir.create(infolder)
    
    # mask variables
    masked <- mask(crop(set, buff_area), buff_area)
    
    # write variables
    for (k in 1:length(unstack(masked))) {
      writeRaster(masked[[k]], filename = paste(infolder, var_names[[j]][k], sep = "/"),
                  format = "ascii")
    }
    cat("   Set", j, "of", length(var_list), "finished\n")
  }
  
  cat("Species", i, "of", length(spvector), "finished\n")
}




#Thining 
install.packages('spThin')
library(spThin)
setwd('D:\\Mariannas Business\\Orthoptera_Olly\\Models2')
spvec <- dir()
exclude <- c("Variables", "G_variables")
spvector <- spvec[!spvec %in% exclude]

final_occ <- list()

for (i in 1:length(spvector)) {
  # reading species data
  sp <- read.csv(paste(spvector[i], "/", paste(spvector[i], ".csv", sep = ""), sep = ""))
  sp_nodup <- unique(sp)
  
  if (dim(sp_nodup)[1] > 1000) {
    sp_nodup <- sp_nodup[sample(1:nrow(sp_nodup), 1000), ]
  }
  
  # thinning
  thinnedcat_a <- thin( loc.data = sp_nodup, lat.col = "decimalLatitude", 
                        long.col = "decimalLongitude", 
                        spec.col = "scientificName", thin.par = 10, reps = 5, 
                        locs.thinned.list.return = TRUE, write.files = FALSE, 
                        max.files = 1, out.dir = spvector[i], 
                        out.base = "cat_a_thined", write.log.file = FALSE )
  
  thin <- cbind(as.character(sp_nodup[1, 1]), thinnedcat_a[[5]])
  colnames(thin) <- colnames(sp_nodup)
  
  write.csv(thin, paste0(spvector[i], "/", paste0(spvector[i], "_thin.csv")),
            row.names = FALSE)
  
  # training and testing split
  thin$check <- paste(thin[,2], thin[,3], sep = "_") # column to make the split
  train <- thin[sample(nrow(thin), round((length(thin[,1]) / 4 * 3))), ] # training data
  test <- thin[!thin[,4] %in% train[,4],] # testing data
  
  thin$check <- NULL # deleting column to make splits
  train$check <- NULL
  test$check <- NULL
  
  write.csv(thin, paste0(spvector[i], "/", paste0(spvector[i], "_joint.csv")), row.names = FALSE) # writing files (change names as needed)
  write.csv(train, paste0(spvector[i], "/", paste0(spvector[i], "_train.csv")), row.names = FALSE)
  write.csv(test, paste0(spvector[i], "/", paste0(spvector[i], "_test.csv")), row.names = FALSE)
  
  # number of final records per species
  final_occ[[i]] <- data.frame(species = spvector[i], n_records = dim(thin)[1])
  
  cat("\nSpecies", i, "of", length(spvector), "finished\n")
}

# writing summary of species number of records
final_n <- do.call(rbind, final_occ)
write.csv(final_n, "summary_occ_n.csv", row.names = FALSE)




#Prepaqre folders------------
path <- "D:/Mariannas Business/Orthoptera_Olly/sp_enm"
filenames <- list.files(path= path, full.names=F)
sp <- unique(x$name)

x <- read.csv('D:/Mariannas Business/Orthoptera_Olly/All_orthoptera_ENM.csv')
head(x)
n = 20 # at least 20  records 
cols = 1:3 # columns extracted from dataset - species, long, lat
setwd('D:/Mariannas Business/Orthoptera_Olly/Models2')
i=1

for (i in 1:length(sp)) {
  
  sptable <- x[x$name == sp[i], cols]
  
  if (dim(sptable)[1] >= n) {
    
    dir.create(paste(sp[i], collapse = "_"))
    write.csv(sptable, paste(paste(sp[i], collapse = '_'), '\\',
                             paste(sp[i], collapse = '_'), ".csv", sep = ""), row.names = FALSE)
  }
  
}

library(raster)

# 3. Correcting the occourence-rasters match-------------------------

r <- raster("D:\\Variables_EU\\bbc45\\bio_1.asc")
i=1
for (i in 1:length(spvector)) {
  x <- read.csv(paste(spvector[i], "/", spvector[i], ".csv", sep=""))
  x1 <- kuenm_toclosest(x,  'decimalLatitude', 'decimalLongitude',r, limit.distance = 10) #distanmce is in Km
  write.csv(x1, paste("D:/Species_models", spvector[i], paste(spvector[i], "_joint.csv", sep = ""), sep = "/"), 	row.names = FALSE)
  
}

head(x)

#4. install Rtools- add his program to the PATH of the computer-----------------------
#4. runing calibration and final models in loop----------
# remember to first install Rtools addid this program to the PATH of the computer
#remeber to create a folder of G_variables in the Ku-enm folder

install.packages('devtools')
devtools::install_github("marlonecobos/kuenm") 
install.packages('kuenm')
library(kuenm)
install.packages('raster')

setwd('D:/Species_models')
spvec <- dir()
exclude <- c("Variables", "G_variables")
spvector <- spvec[!spvec %in% exclude]
sp1 <- spvector

# occurrences
occ_joint <- paste(sp1, "/", paste(sp1, "_joint.csv", sep = ""), sep = "")
occ_tra <- paste(sp1, "/", paste(sp1, "_train.csv", sep = ""), sep = "")
occ_test <- paste(sp1, "/", paste(sp1, "_test.csv", sep = ""), sep = "")

# variables
M_var_dir <- paste(sp1, "/", "M_variables", sep = "")# m for each species
G_var_dir <- "G_variables" #g for all species

# other arguments
batch_cal <- paste(sp1, "/", "Candidate_models1", sep = "")
out_dir <- paste(sp1, "/", "Candidate_Models1", sep = "")
reg_mult <- c(seq(0.5, 1, 0.5), seq(2, 2, 1))
f_clas <- "no.t.h"
background <- 10000
maxent_path <- "C:\\Maxent"
wait <- TRUE
run <- TRUE
out_eval <- paste(sp1, "/", "Calibration_results1", sep = "")
threshold <- 5
rand_percent <- 50
iterations <- 500
kept <- FALSE
selection <- "OR_AICc"
paral_proc <- FALSE
batch_fin <- paste(sp1, "/", "Final_models", sep = "")
mod_dir <- paste(sp1, "/", "Final_Models", sep = "")
rep_n <- 10
rep_type <- "Bootstrap"
jackknife <- FALSE
out_format <- "logistic"
project <- TRUE
ext_type <- "ext_clam"
write_mess <- TRUE
write_clamp <- FALSE
args <- NULL
i=1
# runing calibration and final models in loop
for (i in 18:length(occ_joint)) {
  # create candidate models
 # kuenm_cal(occ.joint = occ_joint[i], occ.tra = occ_tra[i], M.var.dir = M_var_dir[i], 
  #          batch = batch_cal[i], out.dir = out_dir[i], reg.mult = reg_mult, f.clas = f_clas, 
  #          maxent.path = maxent_path, wait = wait, run = run)
  # evaluate all candidate models and select the best based on pROC, Omission rate and complexity AICc
  #kuenm_ceval(path = out_dir[i], occ.joint = occ_joint[i], occ.tra = occ_tra[i], occ.test = occ_test[i],
  #           batch = batch_cal[i], out.eval = out_eval[i], threshold = threshold,
  #            rand.percent = rand_percent, iterations = iterations, kept = kept,
   #           selection = selection, parallel.proc = paral_proc)
  # create final models using the parameterizations selected before
  kuenm_mod(occ.joint = occ_joint[i], M.var.dir = M_var_dir[i], out.eval = out_eval[i], batch = batch_fin[i],
            rep.n = rep_n, rep.type = rep_type, jackknife = jackknife, out.dir = mod_dir[i],
            out.format = out_format, project = project, G.var.dir = G_var_dir, ext.type = ext_type,
           write.mess = write_mess, write.clamp = write_clamp, maxent.path = maxent_path,
            args = args, wait = wait, run = run)
  cat("\n\nAnalyses for species", i, "of", length(occ_joint), "finished\n")
}


#Selecting best models based on delta AIC and ROC----------------------

setwd("D:/Species_models")
spvec <- dir()
exclude <- c("Variables", "G_variables")
spvector <- spvec[!spvec %in% exclude]
sp1 <- spvector
j=17
for (j in 17:length(spvector)) {
    best <- list.files(path = paste(spvector[j], "Calibration_results1", sep = "/"),
                       pattern = "best", full.names = TRUE)
    bestr <- read.csv(best)
    
    file.rename(best, paste(spvector[j], "Calibration_results1", "tseb_candidate_models_OR_AICc.csv", sep = "/"))
    
    bestr <- bestr[bestr[, 6] == 0, ]
    
    if (dim(bestr)[2] > 1) {
      models <- bestr[, 1]
      
      sn <- ".*set2"
      stn <- gregexpr(sn, models)
      stan <- regmatches(models, stn)
      statn <- unlist(stan)
      
      if (length(statn) > 0) {
        bestb <- bestr[bestr[, 1] == statn, ][1, ]
      }else {
        bestb <- bestr[1, ]
      }
    }else {
      bestb <- bestr
    }
    
    write.csv(bestb, file = best, row.names = FALSE)
    cat("   species", j, "of", length(spvector),'\n')
}
#Rename Final Models #"Replacing names on incorrect files=---------------------------------------------------
setwd('D:\\Species_models\\Yersinella raymondii\\Final_Models\\M_0.5_F_qp_set3_EC')

f <- as.data.frame(list.files('D:\\Species_models\\Yersinella raymondii\\Final_Models\\M_0.5_F_qp_set3_EC'), header=FALSE)
colnames(f) <- 'files.old'
f$files.new <- sapply(f$files.old,function(x) gsub("__Yersin,_1860_", "", x))
file.rename(as.vector(f$files.old), as.vector(f$files.new))


  
#5. Model threshold--------------------
library(kuenm)

setwd("D:/Species_models")
dir()

spvec <- dir()
exclude <- c("Variables", "G_variables")
spvector <- spvec[!spvec %in% exclude]
sp1 <- spvector # others are [27:63] and [64:78]
sp2 <- gsub(" ", "_", sp1)

sp_name <- sp2
fmod_dir <- paste(sp1, "Final_Models", sep = '/')
format <- "asc"
project <- TRUE
stats <- c("med", "range")
rep <- TRUE
scenarios <- c("current", "bbc45", "bbc85", "ccm45", "ccm85", 'ces45', 'ces85', 'csi45', 'csi85', 'mir45', 'mir85', 'moh45', 'moh85')
ext_type <- c("EC") # you can select only one type of extrapolation if needed
out_dir <- paste(sp1, "Final_Model_Stats", sep = '/')

# other arguments were defined before
occ <- paste(sp1, paste(sp1, "joint.csv", sep = '_'), sep = "/")
thres <- 5
curr <- "current"
emi_scenarios <- c("45", "85")
t_periods <- c("2050")
c_mods <- c("bbc", "ccm", 'ces', 'csi', 'mir', 'moh')
out_dir1 <-  paste(sp1, "Projection_Changes", sep = '/')
split <- 200 #partitioning the stack of models of different scenarios
out_dir2 <- paste(sp1, "Variation_from_sources", sep = '/')
i=20
for (i in 1:length(sp1)) {
 # if(i != 1)    {
 #   kuenm_modstats(sp.name = sp_name[i], fmod.dir = fmod_dir[i], format = format, project = project, 
 #                  statistics = stats, replicated = rep, proj.scenarios = scenarios, 
 #                  ext.type = ext_type, out.dir = out_dir[i]) 
 # }
}
i=1
for (i in 21:length(sp1)) {
  kuenm_projchanges(occ = occ[i], fmod.stats = out_dir[i], threshold = thres, 
                    current = curr, emi.scenarios = emi_scenarios, 
                    clim.models = c_mods, ext.type = ext_type, out.dir = out_dir1[i])
}
  #kuenm_modvar(sp.name = sp_name[i], fmod.dir = fmod_dir[i], replicated = rep, format = format,  
   #            project = project, current = curr, emi.scenarios = emi_scenarios, 
    #            ext.type = ext_type, split.length = split, out.dir = out_dir2[i])
  
 # cat("\n\nAnalyses for species", i, "of", length(sp1), "finished\n")
#}






#MOP analysis-----------------------------------
setwd("D:/Species_models") # this directory has ***only the group nw_shallow***
install.packages('devtools')
devtools::install_github("marlonecobos/kuenm", force=T)
install.packages('raster')
install.packages('rgeos')
library(raster)
library(rgeos)
library(kuenm)

setwd("D:/Species_models")
spvec <- dir()
exclude <- c("Variables", "G_variables")
spvector <- spvec[!spvec %in% exclude]


percent <- 3 # percent for loop
paral <- FALSE # no paralel
comp_each <- 3000 # reduce this or incrrease it depending on how your memory is used, increasing this number makes it go faster

#for (i in 1) {
#  spvec <- dir(path = deep[i], full.names = TRUE)
#  exclude <- paste(deep[i], c("Variables", "G_variables"), sep = "/")
# spvector <- spvec[!spvec %in% exclude]

G_var_dir <- paste('D:/Species_models', "G_variables", sep = "/")

for (j in 1:length(spvector)) {
  sets <- list.files(path = paste(spvector[j], "Calibration_results1", sep = "/"),
                     pattern = "best", full.names = TRUE)
  sets_var <- as.character(read.csv(sets)[1, 1])
  
  sets_var <- gsub("^M.*_", "", sets_var)
  M_var_dir <- paste(spvector[j], "M_variables", sep = "/")
  out_mop <- paste(spvector[j], "MOP_results", sep = "/")
  
  kuenm_mmop(G.var.dir = G_var_dir, M.var.dir = M_var_dir, sets.var = sets_var, out.mop = out_mop,
             percent = percent, parallel = paral, comp.each = comp_each)
  
  cat("   species", j, "of", length(spvector), "==========\n")
}

cat("group", i, "of", length(deep), "======================\n")
}

#MOP Processing and Percentage of area -------------------

setwd('D:/Species_models')
dir()
##MOPs
mops <- "MOP_\\d\\S*.tif$"
set <- "set\\d"

files <- list.files(pattern = mops, full.names = T, recursive = T) #list MOPS
files

new_files <- gsub("MOP_results", "MOP_results_bin", files)
new_files <- gsub(".tif$", "_bin.tif", new_files)
new_files

SET <- gsub(mops, "", new_files)
MAIN <- gsub(paste0(set, "/", mops), "", new_files)  #Mop binary names

##Binary
setwd('D:/Species_models')
sps <- dir()
exclude <- c("Variables", "G_variables")  #exclude g_variables and variables
sps <- sps[!sps %in% exclude]

bins <- "binary.*if$"

bin_files <- list.files(pattern = bins, full.names = T, recursive = T)
bin_files

bins_exc <- "binary_c.*on.tif$"
bin_files_exc <- list.files(pattern = bins_exc, full.names = T, recursive = T)
bin_files <- bin_files[!bin_files %in% bin_files_exc]

ex <- seq(2, 360, 2) 
ne <- seq(8, 360, 8)

exs <- ex[!ex %in% ne]
bin_files <- bin_files[-exs]

new_bins <- gsub("Pro.*", 'Reduced_binary/', bin_files)
new_bins <- gsub(".tif$", "_bin.tif", new_bins)
new_bins

pat <- '.*%_'
name <- gsub(pat, '', new_files)
new_bins <- paste0(new_bins, name)

MAIN1 <- paste0(sps, "/", "Reduced_binary")
SCEN <- paste0(MAIN1, rep(c("/rcp_26", "/rcp_85"), length(MAIN1)))

results <- list()

for (i in 106:length(new_files)) {
  setwd("D:\\MOPS_Ready\\arctic_shallow")
  mop <- raster(files[i])
  setwd('D:\\MODELS_Ready\\arctic_shallow')
  bin <- raster(bin_files[i])
  
  mop <- mop > 0 # strict extrapolation
  #dir.create(MAIN[i]) #creating folder to save binary version of MOPS - MOP_results_bin
  #dir.create(SET[i])
  writeRaster(mop, filename = new_files[i], format = "GTiff", overwrite=TRUE)
  
  
  bin1 <- bin * mop
  #creating folder to save reduced version of MOPs - MOP - BINARY PREDIC
  dir.create(MAIN1[i])
  #dir.create(SCEN[i])
  writeRaster(bin1, filename = new_bins[i], format = "GTiff", overwrite=TRUE)
  
  
  # before
  vals <- na.omit(values(bin))
  uval <- sort(unique(vals))
  
  ## processing
  all <- length(vals)
  percents <- sapply(1:length(uval), function(x) {
    per <- round(sum(vals == uval[x]) * 100 / all, 2)
    return(per)
  })
  names(percents) <- uval
  
  # after
  vals <- na.omit(values(bin1))
  uval <- sort(unique(vals))
  
  ## processing
  all <- length(vals)
  percents1 <- sapply(1:length(uval), function(x) {
    per <- round(sum(vals == uval[x]) * 100 / all, 2)
    return(per)
  })
  names(percents1) <- uval
  
  results[[i]] <- list(before = percents, after = percents1)
  
  cat(i, "of", length(new_files), "\n")
}

