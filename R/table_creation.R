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

# install and load packages
loadpackage <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
    }
    #  Load package (after installing)
    library( i , character.only = TRUE )
  }
}

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

meta <- read.csv(paste0(metapath, "varnames.csv") , header = TRUE)

###########################################################################################################################
### Funktionen:

get_data <- function(dataset, variable, year, weight, diffcount, diffvars){
  
  # Funktion get_data soll Ausgangs-Datensatz auf bestimmte Variablen beschränken 
  # (variable = Analysevariable, period = Erhebungsjahr, weight = Gewichte, 
  # diffvars = Maximal 3 Differenzierungsvariablen)
  # 
  # Args:
  # dataset= data.frame (plattform_data)
  # variable = Name Analysevariable als string (z.B. "pglabnet" )
  # period = Name Erhebungsjahrvariable als string (z.B. "syear")
  # diffvar= Vector mit Differenzierungsvariablen (z.B. c("alter_gr", "sex", "Bildungsniveau"))
  #
  # Returns:
  # variable.values.valid = Teildatensatz mit validen Werten der Analysevariable 
  # und allen zusätzlich definierten Variablen
  
  if (diffcount > 0) {
    variable.values <- subset(dataset,
                              select=c(variable, year, weight, diffvars)) 
    
    names(variable.values) <- c("usedvariable", "year", "weight", diffvars)
  }
  
  if (diffcount == 0) {
    variable.values <- subset(dataset,
                              select=c(variable, year, weight)) 
    
    names(variable.values) <- c("usedvariable", "year", "weight")
  }

  if (any(variable.values$usedvariable >= 0)) { 
    variable.values.valid <- subset(variable.values,usedvariable>= 0) 
    variable.values.valid <- variable.values.valid[order(variable.values.valid$usedvariable),]
  }
  return(variable.values.valid)
}

##############################################################################################
get_mean_values <- function(dataset, year, diffcount,
                            diffvar1, diffvar2, diffvar3) {
  
  # Funktion get_mean_values soll gewichtete Mittelwert und Mediantabellen
  # erzeugen mit den Informationen n = größe der Subgruppe, weighted_mean = gewichteter
  # Mittelwert, weighted_median = gewichteter Median, lower = unteres Konfidenzintervall 95%,
  # upper = oberes Konfidenzintervall 95%
  # 
  # Args:
  # dataset = produzierter data.frame aus get_data (z.B variable.values.valid)
  # usedvariable = Name Analysevariable (usedvariable)
  # period = Name Erhebungsjahrvariable (period)
  # weight = Name der Gewichtungsvariable (weight)
  # diffcount= Anzahl an Differenzierungen als numeric (0-3 eraubt)
  # diffvar1 = Differenzierungsvariable 1
  # diffvar2 = Differenzierungsvariable 2
  # diffvar3 = DIfferenzierungsvariable 3
  
  # Returns:
  # mean.values = Datensatz mi n, weighted_mean, weighted_median, Konfidenzintervalle
  
  if (diffcount == 0) {
    columns = year
  }
  
  if (diffcount == 1) {
    columns = c(year, diffvar1)
  }
  
  if (diffcount == 2) {
    columns = c(year, diffvar1, diffvar2)
  }
  
  if (diffcount == 3) {
    columns = c(year, diffvar1, diffvar2, diffvar3)
  }
  mean.values <- dataset[complete.cases(dataset), ] %>%
    group_by_at(vars(one_of(columns))) %>%
    mutate(weighted_mean = round(weighted.mean(usedvariable, weight),2))  %>%
    mutate(weighted_median = round(weighted.median(usedvariable, weight),2))  %>%
    add_count(year, wt = NULL)  %>%
    mutate(sd = sd(usedvariable/sqrt(n))) %>%
    mutate(lower = weighted_mean - qt(1 - (0.05 / 2), as.numeric(n) - 1) * sd,
           upper = weighted_mean + qt(1 - (0.05 / 2), as.numeric(n) - 1) * sd) %>%
    mutate(lower = round((lower),2))  %>%
    mutate(upper = round((upper),2))  %>%
    distinct(weighted_mean, .keep_all = TRUE)
  
  selected.values <- c(columns, "weighted_mean", "lower", 
                       "upper", "weighted_median", "n")
  
  mean.values <- mean.values[,(names(mean.values) %in% selected.values)]
  mean.values <-  mean.values %>% 
    arrange(desc(year), .by_group = TRUE)
  
  return(mean.values)
}

###########################################################################################################################

# data_protection
get_protected_values <- function(dataset, cell.size) {
  
  # get_protected_values überprüft Tabelle aus get_mean_values auf ihre Zellengröße
  # 
  # Args:
  # dataset = produzierter data.frame aus get_mean_values (z.B mean.values)
  # cell.size = maximal zulässige Zellengröße als numeric (z.B. 30)
  
  # Returns:
  # protected.data = Datensatz mit n, weighted_mean, weighted_median, Konfidenzintervalle
  # ausschließlich mit Zellen >= cell.size
  
  save.data <- as.data.frame(apply(dataset[c("weighted_mean", "weighted_median", "n",
                                             "lower", "upper")], 2, 
                                   function(x) ifelse(dataset["n"] < 30, NA, x)))
  data <- dataset
  dataset[c("weighted_mean", "weighted_median", "n",
            "lower", "upper")] <- NULL
  protected.data <- cbind(dataset, save.data)
  return(protected.data)
}

create_table_lables <- function(table) {
  data_with_label <- table
  
  if("sex" %in% colnames(data_with_label)){
    data_with_label$sex <- gsubfn(".", list("1" = "Männlich", "2" = "Weiblich"), data_with_label$sex)
  }
  
  if("bula" %in% colnames(data_with_label)){
    data_with_label$bula <-data_with_label$bula %>%
      gsub("10", "Saarland", .) %>%
      gsub("11", "Berlin", .) %>%
      gsub("12", "Brandenburg", .) %>%
      gsub("13", "Mecklenburg-Vorpommern", .) %>%
      gsub("14", "Sachsen", .) %>%
      gsub("15", "Sachsen-Anhalt", .) %>%
      gsub("16", "Thueringen", .) %>% 
      gsub("1", "Schleswig-Holstein", .) %>%
      gsub("2", "Hamburg", .) %>%
      gsub("3", "Niedersachsen", .) %>%
      gsub("4", "Bremen", .) %>%
      gsub("5", "Nordrhein-Westfalen", .) %>%
      gsub("6", "Hessen", .) %>%
      gsub("7", "Rheinland-Pfalz", .) %>% 
      gsub("8", "Baden-Wuerttemberg", .) %>% 
      gsub("9", "Bayern", .)
  }
  
  if("sampreg" %in% colnames(data_with_label)){
    data_with_label$sampreg <- gsubfn(".", list("1" = "Westdeutschland, alte Bundeslaender", 
                                      "2"  = "Ostdeutschland, neue Bundeslaender"), data_with_label$sampreg)
  }
  
  if("pgcasmin" %in% colnames(data_with_label)){
    data_with_label$pgcasmin <- gsubfn(".", list("0" = "(0) in school", "1"  = "(1a) inadequately completed", 
                                       "2" = "(1b) general elementary school", "3"  = "(1c) basic vocational qualification", 
                                       "4" = "(2b) intermediate general qualification", "5"  = "(2a) intermediate vocational", 
                                       "6" = "(2c_gen) general maturity certificate", "7"  = "(2c_voc) vocational maturity certificate",
                                       "8" = "(3a) lower tertiary education", "9" = "(3b) higher tertiary education"), data_with_label$pgcasmin)
  }
  
  if("alter_gr" %in% colnames(data_with_label)){
    data_with_label$alter_gr <- gsubfn(".", list("1"  = "16-34 Jahre alt", 
                                       "2" = "35-65 Jahre alt", "3"  = "66 und älter"), data_with_label$alter_gr)
  }
  
  if("Bildungsniveau" %in% colnames(data_with_label)){
    data_with_label$Bildungsniveau <- gsubfn(".", list("1"  = "(noch) kein Abschluss", 
                                             "2" = "Hauptschulabschluss", "3"  = "Realschulabschluss",
                                             "4" = "(Fach-)Abitur", "5"  = "AkademikerInnen"), data_with_label$Bildungsniveau)
  }
  
  return(data_with_label)
}


# export_table
get_table_export <- function(table, variable, metadatapath, exportpath, diffcount) {
  
  # get_table_export exportiert Datensatz aus get_protected_values als csv
  # 
  # Args:
  # table = produzierter data.frame aus get_protected_values (z.B protected.data)
  # variable = Name Analysevariable aus den Rohdaten als string ("pglabnet")
  # metadatapath = Pfad zu den Metadaten mit Variablenumbenennungen als string
  # exportpath = Exportordner als string
  # diffcount = Anzahl an Differenzierungen als numeric (0-3 eraubt)
  
  # Returns:
  # table = exportierte Tabelle als csv
  
  metadata <- read.csv(metadatapath , header = TRUE)
  variable <- variable
  path <- file.path(exportpath, variable)
  dir.create(path, showWarnings = FALSE)
#  varname <- metadata$varname[metadata$variable==variable]
  folder <- paste0(variable, "/")
  
  diffvars <- 1+diffcount
  filenames  <- names(table)[2:diffvars]
  
  if (diffcount == 3) {
    filename <- paste0(variable, "_", "year", "_", metadata$variable[metadata$variable==filenames[1]], "_",
                       metadata$variable[metadata$variable==filenames[2]], "_", 
                       metadata$variable[metadata$variable==filenames[3]])
  }
  
  if (diffcount == 2) {
    filename <- paste0(variable, "_", "year", "_", metadata$variable[metadata$variable==filenames[1]], "_",
                       metadata$variable[metadata$variable==filenames[2]])
  }
  
  if (diffcount == 1) {
    filename <- paste0(variable, "_", "year", "_", metadata$variable[metadata$variable==filenames[1]])
  }
  
  if (diffcount == 0) {
    filename <- paste0(variable, "_", "year")
  }
  
  export <- paste0(exportpath, folder, filename, ".csv")
  
  data.csv <- sapply(table, as.character)
  data.csv[is.na(data.csv)] <- ""
  data.csv <- as.data.frame(data.csv)
  
  data.csv <- create_table_lables(table = data.csv)
  
  write.csv(data.csv, export, row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8")
  return(data.csv)
}


###########################################################################################################################
###############################################################################################
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
  
  if (meta$meantable[var] == "Yes") {
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
      data <- get_data(dataset = data.file.num, 
                       variable = variable, 
                       year = year, 
                       weight = weight,
                       diffcount = diffcount,
                       diffvars = diffvars)
  
      table.values <- get_mean_values(dataset = data, 
                                      year = "year", 
                                      diffcount = diffcount,
                                      diffvar1 = diffvar1,
                                      diffvar2 = diffvar2,
                                      diffvar3 = diffvar3)
  
      protected.table <- get_protected_values(dataset = table.values, cell.size = cell.min)
  
      data.csv <- get_table_export(table = protected.table, variable = variable, 
                                   metadatapath = paste0(metapath, "varnames.csv"),
                                   exportpath = exportpath, diffcount = diffcount)
  
      print(paste("Die Variable", variable, "wird verarbeitet mit Differenzierung", paste(difflist[[i]],collapse=",")))
    } 
  }
}  


