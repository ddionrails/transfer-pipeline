#############################################################################
# Erzeugung Aggregierte Tabellen für SOEP-Transfer Projekt
#############################################################################

### Was muss definiert werden:
# Persönlicher Pfad
if (Sys.info()[["user"]] == "Stefan") {
  datapath <- "C:/Users/Stefan/DIW/transfer/generierung/"
  metapath <- "C:/git/platform-datasets/scripts/"
  exportpath <- "C:/git/platform-datasets/testdaten/"
}

# Definition von Objekten
dataset <- "plattform_data"  # Aus welchem Datensatz sollen Werte genommen werden
cell.min <- "30" # Maximal erlaubte Zellgröße
year <- "syear" # Erhebungsjahr muss definiert sein
weight <- "phrf" # Gewicht muss definiert sein
#############################################################################

## load packages
loadpackage(c("foreign", "dplyr", "tidyverse", "readstata13", "spatstat",
              "gsubfn")) 

## load dataset
data.file.num <- read.dta13(paste0(datapath,dataset,".dta"), 
                            convert.factors = FALSE, encoding = "UTF-8")

# Gewichte mit 0 machen Probleme bei der Mittelwertberechnung
data.file.num <- data.file.num %>%
  filter(phrf > 0) 

data.file.fac <- read.dta13(paste0(datapath,dataset,".dta"), 
                            convert.factors = TRUE, 
                            nonint.factors = TRUE, encoding = "UTF-8")

data.file.fac <- data.file.fac %>%
  filter(phrf > 0) 

meta <- read.csv(paste0(metapath, "variables.csv") , header = TRUE)

################################################################################
################################################################################
### Code zum Erzeugen:

meta_demo <- meta %>%
  filter(meantable == "demo") 

meta_demo <- subset(data.file.num,
                    select=meta_demo$variable)

difflist <- c("",combn(names(meta_demo),1,simplify=FALSE), 
  combn(names(meta_demo),2,simplify=FALSE))

diffcountlist <- difflist
diffcountlist[[1]] <- 0
diffcountlist[2:(1+length(combn(names(meta_demo),1,simplify=FALSE)))] <- 1
diffcountlist[(2+length(combn(names(meta_demo),1,simplify=FALSE))):length(diffcountlist)] <- 2


for (var in 1:length(meta$variable)){
  
  if (meta$meantable[var] == "Yes" | meta$proptable[var] == "Yes") {
      variable <- meta$variable[var] 
      
    for(i in seq_along(difflist)){
      diffcount <- diffcountlist[[i]]
      diffvars <- difflist[[i]]
  
      if (!is.na(diffvars[1])) {
        diffvar1 <- diffvars[1]
      } else {
        diffvar1 <- ""
      }
  
      if (!is.na(diffvars[2]))  {
        diffvar2 <- diffvars[2]
      } else {
        diffvar2 <- ""
      }
  
      if (!is.na(diffvars[3]))  {
        diffvar3 <- diffvars[3]
      } else {
        diffvar3 <- ""
      }

      if (meta$meantable[var] == "Yes") {
      data <- get_data(datasetnum =  data.file.num, 
                       datasetfac = data.file.fac,
                       variable = variable, 
                       year = year, 
                       weight = weight,
                       diffcount = diffcount,
                       diffvars = diffvars,
                       vallabel = FALSE)
      

      table.values <- get_mean_values(dataset = data, 
                                      year = "year", 
                                      diffcount = diffcount,
                                      diffvar1 = diffvar1,
                                      diffvar2 = diffvar2,
                                      diffvar3 = diffvar3)
      

      table.values <- create_table_lables(table = table.values)
      
      protected.table <- get_protected_values(dataset = table.values, cell.size = cell.min)
      
      data.csv <- get_table_export(table = protected.table, variable = variable, 
                                   metadatapath = paste0(metapath, "variables.csv"),
                                   exportpath = exportpath, diffcount = diffcount, 
                                   tabletype = "mean")
      
      json_output <- rjson::toJSON(
        x = list(
          "title" = meta$label_de[var],
          "variable" = meta$variable[var],
          "statistics" = c("mean", "median", "n", "upper_ci", "lower_ci"),
          "dimensions" = list(
            list("variable" = "year", "label" = "Erhebungsjahr"),
            list("variable" = meta$variable[meta$variable == "alter_gr"], "label" = meta$label_de[meta$variable == "alter_gr"]),
            list("variable" = meta$variable[meta$variable == "sex"], "label" = meta$label_de[meta$variable == "sex"]),
            list("variable" = meta$variable[meta$variable == "bula"], "label" = meta$label_de[meta$variable == "bula"]),
            list("variable" = meta$variable[meta$variable == "Bildungsniveau"], "label" = meta$label_de[meta$variable == "Bildungsniveau"]),
            list("variable" = meta$variable[meta$variable == "sampreg"], "label" = meta$label_de[meta$variable == "sampreg"]),
            list("variable" = meta$variable[meta$variable == "pgcasmin"], "label" = meta$label_de[meta$variable == "pgcasmin"])
          ),
          "start_year" = unique(data.csv$year)[1],
          "end_year" = unique(data.csv$year)[length(unique(data.csv$year))],
          "types" = list("numeric")
        )
      )
      
      file_handler <- file("meta.json")
      writeLines(json_output, paste0(exportpath, variable, "/",variable, ".json"))
      close(file_handler)
      
      
      print(paste("Die Variable", variable, "wird verarbeitet mit Differenzierung", paste(difflist[[i]],collapse=","), "als Mittelwert-Tabelle"))
      }
      
      if (meta$proptable[var] == "Yes") {
        data <- get_data(datasetnum =  data.file.num, 
                         datasetfac = data.file.fac,
                         variable = variable, 
                         year = year, 
                         weight = weight,
                         diffcount = diffcount,
                         diffvars = diffvars,
                         vallabel = TRUE)
        
        if (diffvars=="") {
          columns <- c("usedvariable", "year")
        } else {
          columns <- c("usedvariable", "year", diffvars)
        }
        
        prop.data <- get_prop_values(dataset = data, groupvars = columns, alpha = 0.05)
        
        protected.table <- get_protected_values(dataset = prop.data, cell.size = 30)
        
        data.csv <- get_table_export(table = protected.table, variable = variable, 
                                     metadatapath = paste0(metapath, "variables.csv"),
                                     exportpath = exportpath, diffcount = diffcount,
                                     tabletype = "prop")
        
        json_output <- rjson::toJSON(
          x = list(
            "title" = meta$label_de[var],
            "usedvariable" = meta$variable[var],
            "statistics" = c("percent", "n", "upper_ci", "lower_ci"),
            "dimensions" = list(
              list("variable" = "year", "label" = "Erhebungsjahr"),
              list("variable" = meta$variable[meta$variable == "alter_gr"], "label" = meta$label_de[meta$variable == "alter_gr"]),
              list("variable" = meta$variable[meta$variable == "sex"], "label" = meta$label_de[meta$variable == "sex"]),
              list("variable" = meta$variable[meta$variable == "bula"], "label" = meta$label_de[meta$variable == "bula"]),
              list("variable" = meta$variable[meta$variable == "Bildungsniveau"], "label" = meta$label_de[meta$variable == "Bildungsniveau"]),
              list("variable" = meta$variable[meta$variable == "sampreg"], "label" = meta$label_de[meta$variable == "sampreg"]),
              list("variable" = meta$variable[meta$variable == "pgcasmin"], "label" = meta$label_de[meta$variable == "pgcasmin"])
            ),
            "start_year" = unique(data.csv$year)[1],
            "end_year" = unique(data.csv$year)[length(unique(data.csv$year))],
            "types" = list("numeric")
          )
        )
        
        file_handler <- file("meta.json")
        writeLines(json_output, paste0(exportpath, variable, "/",variable, ".json"))
        close(file_handler)
        
        
        print(paste("Die Variable", variable, "wird verarbeitet mit Differenzierung", paste(difflist[[i]],collapse=","), "als Prozentwert-Tabelle"))
        }
    } 
  }
}  






