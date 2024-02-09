### negate the function
`%notin%` <- Negate("%in%")

# This function creates clean labels for the different indicators
clean_labels <- function (indicator_names) {
  indicator_names <- str_replace(indicator_names, "^ar", "Availability ratio")
  indicator_names <- str_replace(indicator_names, "^pr", "Preference ratio")
  indicator_names <- str_replace(indicator_names, "^sr", "Sex ratio")
  indicator_names <- str_replace(indicator_names, "_near", " (near)")
  indicator_names <- str_replace(indicator_names, "_.+km", "")
  return(indicator_names)
}


### Create directory structure ---------------------------------

gen_folder <- function(foldername = "Raw"){
  if(file.exists(foldername)){
    cat("The", foldername, "folder exists already! \n")
  }else{
    dir.create(foldername)
  }
}

# Create the folders
folders <- c("code", "raw", "functions", "data", "results", "figures")
lapply(folders, gen_folder)

### File names --------------------------------------------------

# Set the paths
input <- "D:/ready-made/"

# Create a data frame with all the paths and names
datasets <- data.frame(
  paths = paste0(input, c("FOLK_perus_8800a/folk_19872000_tua_perus21tot_1.csv",
                          "FOLK_perus_0110a/folk_20012010_tua_perus21tot_1.csv", 
                          "FOLK_perus_11a/folk_20112020_tua_perus22tot_1.csv")),
  names = c("raw/folk_1987_2000.Rda", "raw/folk_2001_2010.Rda", "raw/folk_2011_2020.Rda"),
  clean = c("data/cleaned_folk_1987_2000.Rda", "data/cleaned_folk_2001_2010.Rda", "data/cleaned_folk_2011_2020.Rda"))


# Function to create a local copy of the data --------------------

load_data <- function(dataset = 1){
  
  if(dataset %notin% 1:3) stop("Dataset has to be a number betweeen 1 and 3!")
  
  if(file.exists(datasets[dataset, ]$names)){
    cat("File has been saved already!")
  }else{
    # Load the basic data
    d <- fread(datasets[dataset, ]$paths)
    
    # Save a copy of the data
    save(d, file = datasets[dataset, ]$names)
    
    # Delete the file
    rm(d)
  }
  
  # Load the dataset
  load(datasets[dataset, ]$names)
  
  return(d)
}

### Data cleaning --------------------------------

clean_data <- function(...){
  
  d <- (...)
  
  # Select the important variables
  vars <- c("vuosi", "ika", "syntyv", "kuolv", "kturaha_k", "sukup", "ptoim1", "shnro", "syntyp2",
            "kunta", "sivs", "sose", "lkm_k", "skunta", "ututku_aste", "svaltio_k", "maka")
  
  
  # Select the variables
  d <- d[, ..vars]
  
  
  # Year: Rename the original variable
  d <- d[ , .(year = vuosi,
              id = shnro,
              age = ika,
              yob = syntyv,
              yod = kuolv,
              bpl = skunta,
              res = kunta,
              Res = kunta,
              mar = sivs,
              inc = kturaha_k,
              nch = lkm_k,
              set = maka,
              sukup,
              ptoim1,
              svaltio_k,
              ori = syntyp2,
              ututku_aste,
              sose)]
  
  
  
  # sex: Create a variable for sex
  d <- d[ , sex := (ifelse(sukup == "1", "Male", "Female"))] 
  
  # Create a variable for being born in Finland
  d <- d[ , bif := (ifelse(svaltio_k == "246", 1, 0))] 
  
  # Create socio-economic status
  d <- d[ , ses := (ifelse(sose != "99" | sose != "94", sose, NA))]
  
  # Create the settlement
  d <- d[, urban := (ifelse(set %in% c("K1", "K2"), "1", "0"))]
  
  # Create activity status
  d <- d[ , act := fcase(ptoim1 == "11", "employed",
                         ptoim1 == "12", "unemployed",
                         ptoim1 == "22", "student",
                         ptoim1 %in% c("24", "25", "29", "99"), "others",
                         ptoim1 %notin% c("11", "12", "22", "24", "25", "29", "99"), NA_character_)]
  
  
  # Create education variable
  d <- d[ , edu := factor(fcase(is.na(ututku_aste) | ututku_aste < 2, "basic",
                                ututku_aste %in% c(3, 4), "medium",
                                ututku_aste %in% c(5, 6, 7, 8), "high"))]
  

  ### Keep the variables
  d <- d[ , .(year, id, age, sex, edu, yob, bpl, yod, res, mar, ses, nch, inc, act, urban, ori)]
  
  return(d)
}



#### Data.Table Complete ----------------------------------

completeDT <- function(DT, cols, defs = NULL){
  mDT <- do.call(CJ, c(DT[, ..cols], list(unique = T)))
  res <- DT[mDT, on = names(mDT)]
  if (length(defs))
    res[, names(defs) :=  Map(replace, .SD, lapply(.SD, is.na), defs), .SDcols = names(defs)]
  res[]
}

#### Basic Graph settings ================================= 

# Load the packages
library(tidyverse)


# set theme
theme_set(theme_test(base_size = 16, base_family = "serif"))
theme_update(plot.margin = margin(0.2, 0.6, 0.2, 0.2, "cm"),
             panel.grid.major.y =  element_line(colour = "grey80"),
             panel.grid.major.x =  element_blank(),
             panel.grid.minor.y =  element_blank(),
             panel.grid.minor.x =  element_blank(),
             legend.background = element_rect(fill = "white", colour =  "white"),
             legend.title =  element_text(face =  "bold"),
             axis.title.y =  element_text(size =  14, face =  "bold"),
             axis.title.x =  element_text(size =  14, face =  "bold"),
             plot.title.position = "plot",
             # legend.box = element_blank(),
             title = element_text(face = "bold"),
             legend.position = "bottom",
             strip.background = element_blank(),
             strip.text = element_text(size = 14, face = "bold")
)


# Cols
MPIDRpurple <- rgb(red = 62, green = 44, blue = 81, maxColorValue = 255)
MPIDRgreen <- "#066E6E"
MPIDRyellow <- "#FAAF3B"
MPIDRred <- rgb(142, green = 32, blue = 59, maxColorValue = 255)
MPIDRorange <- rgb(239, green = 125, blue = 0, maxColorValue = 255)
MPIDRgrey <- rgb(148, 145, 151, maxColorValue = 255)


# Shades
MPIDR1 <- rgb(148, 145, 151, maxColorValue = 255)
MPIDR2 <- rgb(181, 145, 154, maxColorValue = 255)
MPIDR3 <- rgb(192, 112, 124, maxColorValue = 255)
MPIDR4 <- rgb(180, 55, 70, maxColorValue = 255)
MPIDR5 <- rgb(142, 32, 159, maxColorValue = 255)


# MPIDRpalette
MPIDRpalette <- c(MPIDRpurple, MPIDRorange, MPIDRgreen, MPIDRyellow, MPIDRgrey, MPIDRred)
MPIDRgradient <- c(MPIDR1, MPIDR2, MPIDR3, MPIDR4, MPIDR5)


#### Write a function that counts observations -------------------

# Write a function
cases <- function(data){
  
  # Transform to data.table
  data <- as.data.table(data)
  
  # Count overall observations
  obs <- nrow(data)
  
  # Individuals
  inds <- length(unique(data$id))
  
  # Print the result
  print(c("Individuals" = inds, "Observations" = obs))
  
}


#### Plotting funtions ---------------------------------------------

# Make a histogram
histo <- function(data, variable, title = "a variable", bins = 20){
  plot <- ggplot(data, aes(x = {{variable}})) + 
    geom_histogram(fill = "navyblue", colour = "black", bins = bins) + 
    scale_y_continuous(expand = c(0, 0)) + 
    ggtitle(str_to_title(title))
  return(plot)
}


# Write a function to plot childlessness
plot_childless <- function(data = basic) {
  tmp <- data
  tmp <- tmp[order(id, year), ]
  tmp <- tmp[, spell := seq_along(.N), by = id]
  tmp <- tmp[spell == 1, ]
  plot_res <- basic[, .(childlessness = mean(pc)), by = yob] %>%
    ggplot(aes(yob, childlessness)) + 
    geom_line()
  return(plot_res)
}


#### Tabulate funciton ---------------------------------------------


tab <- function(...){
  tmp <- table(..., useNA = "always")
  return(tmp)
}

### Set the colours ----------------------------

# 
ColLow <- "#ca0020"
ColMlo <- "#f4a582"
ColMid <- "#f7f7f7"
ColMhi <- "#92c5de"
ColHig <- "#0571b0"
