
################################################################################
# Funktionen #
################################################################################

#' @title get_data erzeugt Teilmenge eines Datensatzes
#'
#' @description get_data soll Ausgangs-Datensatz im long Format 
#' auf bestimmte Variablen beschraenken (variable, year, weight, diffvars) und
#' ausschliesslich valide Werte enthalten.
#'
#' @param datasetnum data.frame mit numeric Variablen (z.B. plattform_data)
#' @param datasetfac data.frame mit factor Variablen (z.B. plattform_data)
#' @param variable Name Analysevariable als string (z.B. "pglabnet" )
#' @param year Name Erhebungsjahrvariable als string (z.B. "syear")
#' @param weight Name Gewichtsvariable als string (z.B. "phrf")
#' @param diffcount Anzahl gewuenschter Differenzierungen (z.B. 2) (range 0-3)
#' @param diffvars Vector mit Differenzierungsvariablen (z.B. c("alter_gr", "sex", "Bildungsniveau")) (maximal 3 Variablen)
#' @param vallabel Valuelabel sollen verwendet werden (z.B: vallabel = TRUE) (TRUE/FALSE)
#' 
#' @return variable.values.valid ist ein Datensatz mit validen Werten der 
#' Variablen (variable, year, weight, diffvars)
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords get_data
#'  
#' @examples
#'       get_data(datasetnum =  data.file.num, 
#'       datasetfac = data.file.fac,
#'       variable = "pglabnet", 
#'       year = "syear", 
#'       weight = "phrf",
#'       diffcount = diffcount,
#'       diffvars = c("alter_gr", "sex", "Bildungsniveau"),
#'       vallabel = TRUE)

get_data <- function(datasetnum, datasetfac, variable, year, weight, diffcount, diffvars, vallabel){
  
  if (diffcount > 0) {
    
    if (vallabel == FALSE) {
      variable.values <- subset(datasetnum,
                                select=c(variable, year, weight, diffvars)) 
      names(variable.values) <- c("usedvariablenum", "year", "weight", diffvars)
    }
    if (vallabel == TRUE) {
      variable.values <- subset(datasetnum, select=variable) 
      factorvar <- subset(datasetfac, select=c(variable, year, weight, diffvars))
      variable.values <- cbind(variable.values, factorvar)
      names(variable.values) <- c("usedvariablenum", "usedvariable", "year", "weight", diffvars)
    }
  }
  
  if (diffcount == 0) {
    if (vallabel == FALSE) {
      variable.values <- subset(datasetnum,
                                select=c(variable, year, weight)) 
      names(variable.values) <- c("usedvariablenum", "year", "weight")
    }
    if (vallabel == TRUE) {
      variable.values <- subset(datasetnum, select=variable) 
      factorvar <- subset(datasetfac, select=c(variable, year, weight))
      variable.values <- cbind(variable.values, factorvar)
      names(variable.values) <- c("usedvariablenum", "usedvariable", "year", "weight")
    }
  }
  
  if (any(variable.values$usedvariablenum >= 0)) { 
    if (vallabel == FALSE) {
      variable.values.valid <- subset(variable.values, usedvariablenum>= 0) 
      variable.values.valid <- variable.values.valid[order(variable.values.valid$usedvariablenum),]
      names(variable.values.valid)[names(variable.values.valid) == "usedvariablenum"] <- "usedvariable"
    } 
    if (vallabel == TRUE) {
      variable.values.valid <- subset(variable.values, usedvariablenum>= 0) 
      variable.values.valid <- variable.values.valid[order(variable.values.valid$usedvariablenum),]
      variable.values.valid <- variable.values.valid[2:length(variable.values.valid)]
    }
  }
  
  return(variable.values.valid)
}

################################################################################

#' @title loadpackage laedt benoetigte R-Pakete
#'
#' @description loadpackage soll benoetigte Pakete, wenn noetig installieren und in library laden
#'
#' @param x Namen der benoetigten R-Packages als Vector (z.B. c("foreign", "dplyr", "tidyverse", "readstata13"))
#' 
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords loadpackage
#'  
#' @examples
#'       loadpackage(c("foreign", "dplyr", "tidyverse", "readstata13")) 

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

################################################################################

#' @title get_mean_values erstellt Mittelwert/Mediantabellen mit Mittelwert-Konfidenzinterval
#' 
#' @description get_mean_values soll gewichtete Mittelwert/Mediantabellen mit Mittelwert-Konfidenzinterval 
#' erzeugen mit den Informationen n = Groesse der Subgruppe, mean = gewichteter
#' Mittelwert, median = gewichteter Median, lower_ci = unteres Konfidenzintervall 95%,
#' upper_ci = oberes Konfidenzintervall 95%
#'
#' @param dataset data.frame aus get_data (z.B. plattform_data)
#' @param year Jahresvariable als string (z.B. "year")
#' @param diffcount Anzahl gewuenschter Differenzierungen (z.B. 2) (range 0-3)
#' @param diffvar1 Name Differenzierungsvariable 1 als string ("" moeglich)
#' @param diffvar2 Name Differenzierungsvariable 2 als string ("" moeglich)
#' @param diffvar3 Anzahl Differenzierungsvariable 3 als string ("" moeglich)
#' 
#' @return mean.values = Datensatz mit n, mean, median, Konfidenzintervalle
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords get_mean_values
#'  
#' @examples
#'       get_mean_values(dataset = data, 
#'                       year = "year", 
#'                       diffcount = 2,
#'                       diffvar1 = "sex",
#'                       diffvar2 = "alter_gr",
#'                       diffvar3 = "")

get_mean_values <- function(dataset, year, diffcount,
                            diffvar1, diffvar2, diffvar3) {
  
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
    mutate(mean = round(weighted.mean(usedvariable, weight),2))  %>%
    mutate(median = round(weighted.median(usedvariable, weight),2))  %>%
    add_count(year, wt = NULL)  %>%
    mutate(sd = sd(usedvariable/sqrt(n))) %>%
    mutate(lower = mean - qt(1 - (0.05 / 2), as.numeric(n) - 1) * sd,
           upper = mean + qt(1 - (0.05 / 2), as.numeric(n) - 1) * sd) %>%
    mutate(lower_ci = round((lower),2))  %>%
    mutate(upper_ci = round((upper),2))  %>%
    distinct(mean, .keep_all = TRUE)
  
  selected.values <- c(columns, "mean", "lower_ci", 
                       "upper_ci", "median", "n")
  
  mean.values <- mean.values[,(names(mean.values) %in% selected.values)]
  mean.values <-  mean.values %>% 
    arrange(desc(year), .by_group = TRUE)
  
  return(mean.values)
}

################################################################################
#' @title Funktion get_prop_values soll gewichtete Anteilswert-Tabellen mit Konfidenzintervallen erstellen
#'
#' @description get_mean_values soll gewichtete Anteilswert-Tabellen mit Konfidenzintervallen erstellen
#' erzeugen mit den Informationen n = Groesse der Subgruppe, percent = gewichteter
#' Anteilswert, lower_ci = unteres Konfidenzintervall, upper_ci = oberes Konfidenzintervall 95%
#' 
#' @param dataset data.frame aus get_data (z.B. plattform_data)
#' @param groupvars Vector mit allen Variablen im Datensatz (z.B. c("usedvariable", "year", "sex"))
#' @param alpha Alpha fuer die Festlegung der Konfidenzintervalles (z.B. 0.05)
#' 
#' @return data_prop_complete_ci = Datensatz mit n, percent, lower_ci, upper_ci
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords get_prop_values
#'  
#' @examples
#'       get_prop_values(dataset = data, 
#'                       groupvars = c("usedvariable", "year", "sex"), 
#'                       alpha = 0.05)


get_prop_values <- function(dataset, groupvars, alpha) {
  
  data_prop <-  dataset[complete.cases(dataset), ] %>%
    group_by_at(vars(one_of(groupvars))) %>%
    summarise(count_w = sum(weight), .groups="drop_last") %>%
    #   mutate(percent_groups = round((count_w/sum(count_w)),3) )  %>%
    group_by(year) %>%
    mutate(sum_count_w = sum(count_w)) %>%
    mutate(percent = count_w/sum_count_w,)
  
  data_prop2 <- dataset[complete.cases(dataset), ] %>%   
    group_by_at(vars(one_of(groupvars))) %>%
    summarise(n = n(), .groups="drop_last") 
  
  data_prop <- data_prop[order(data_prop$year),]
  data_prop <- cbind(data_prop, data_prop2["n"])
  
  data_prop_year_total <- setNames(aggregate(data_prop$n, 
                                             by=list(year=data_prop$year), 
                                             FUN=sum), c("year", "year_total"))
  
  data_prop_complete <- left_join(data_prop, data_prop_year_total, by="year")
  
  n_total <- data_prop_complete$year_total
  n_total2 <- data_prop_complete$sum_count_w
  p_hat <- data_prop_complete$percent
  alpha <- alpha
  
  margin1 <- qnorm(1-alpha/2)*sqrt(p_hat*(1-p_hat)/n_total)
  margin2 <- qnorm(1-alpha/2)*sqrt(p_hat*(1-p_hat)/n_total2)
  
  # Compute the CI
  lower_ci1 <- p_hat - margin1
  upper_ci1 <- p_hat + margin1
  
  # Compute the CI
  lower_ci2 <- p_hat - margin2
  upper_ci2 <- p_hat + margin2
  
  data_prop_complete_ci <- cbind(data_prop_complete, 
                                 lower_ci=lower_ci1, 
                                 upper_ci=upper_ci1
  )
  
  data_prop_complete_ci <- subset(data_prop_complete_ci, select=c(groupvars, "n", "percent", 
                                                                  "lower_ci", "upper_ci")) 
  
  return(data_prop_complete_ci)
}

################################################################################
#' @title get_protected_values soll bestimmte Zelleninhalte entfernen
#'
#' @description get_protected_values soll Zelleninhalte von gewichteten Anteilswert-Tabellen oder Mittelwert-Tabelle 
#' loeschen wenn eine Minimalbesetzung unterschritten wird.
#' 
#' @param dataset data.frame aus get_prop_values oder get_mean_table 
#' @param cell.size maximal zulaessige Zellengroesse (z.B. 30)
#' 
#' @return protected.data (Datensatz mit n, mean/percent, median, n, Konfidenzintervalle ausschlie?lich mit Zellen >= cell.size)
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords get_prop_values
#'  
#' @examples
#'       get_prop_values(dataset = data, alpha = 0.05)

get_protected_values <- function(dataset, cell.size) {
  
  if(("mean" %in% colnames(dataset))==TRUE){
    
    save.data <- as.data.frame(apply(dataset[c("mean", "median", "n",
                                               "lower_ci", "upper_ci")], 2, 
                                     function(x) ifelse(dataset["n"] < 30, NA, x)))
    data <- dataset
    dataset[c("mean", "median", "n",
              "lower_ci", "upper_ci")] <- NULL
    
  }
  
  if(("percent" %in% colnames(dataset))==TRUE){
    
    save.data <- as.data.frame(apply(dataset[c("n","percent", 
                                               "lower_ci", "upper_ci")], 2, 
                                     function(x) ifelse(dataset["n"] < 30, NA, x)))
    data <- dataset
    dataset[c("n","percent", 
              "lower_ci", "upper_ci")] <- NULL
    
  }
  protected.data <- cbind(dataset, save.data)
  return(protected.data)
}

################################################################################
#' @title create_table_lables Datensatzes mit Vauluelabel ausstatten
#'
#' @description create_table_lables soll spezifische Variablen eines Datensatzes mit Vauluelabel ausstatten
#' 
#' @param table data.frame aus get_mean_data 
#' 
#' @return data_with_label = Datensatz mit Valuelabels
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords data_with_label
#'  
#' @examples
#'       create_table_lables(table = data)

create_table_lables <- function(table) {
  data_with_label <- table
  
  if("sex" %in% colnames(data_with_label)){
    data_with_label$sex <- gsubfn(".", list("1" = "Männlich", "2" = "Weiblich"), as.character(data_with_label$sex))
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
                                                "2"  = "Ostdeutschland, neue Bundeslaender"), as.character(data_with_label$sampreg))
  }
  
  if("pgcasmin" %in% colnames(data_with_label)){
    data_with_label$pgcasmin <- gsubfn(".", list("0" = "(0) in school", "1"  = "(1a) inadequately completed", 
                                                 "2" = "(1b) general elementary school", "3"  = "(1c) basic vocational qualification", 
                                                 "4" = "(2b) intermediate general qualification", "5"  = "(2a) intermediate vocational", 
                                                 "6" = "(2c_gen) general maturity certificate", "7"  = "(2c_voc) vocational maturity certificate",
                                                 "8" = "(3a) lower tertiary education", "9" = "(3b) higher tertiary education"), as.character(data_with_label$pgcasmin))
  }
  
  if("alter_gr" %in% colnames(data_with_label)){
    data_with_label$alter_gr <- gsubfn(".", list("1"  = "16-34 Jahre alt", 
                                                 "2" = "35-65 Jahre alt", "3"  = "66 und ?lter"), as.character(data_with_label$alter_gr))
  }
  
  if("Bildungsniveau" %in% colnames(data_with_label)){
    data_with_label$Bildungsniveau <- gsubfn(".", list("1"  = "(noch) kein Abschluss", 
                                                       "2" = "Hauptschulabschluss", "3"  = "Realschulabschluss",
                                                       "4" = "(Fach-)Abitur", "5"  = "AkademikerInnen"), as.character(data_with_label$Bildungsniveau))
  }
  
  return(data_with_label)
}

################################################################################
#' @title get_table_export Export von Mittelwert-oder Anteilswertswert-Tabellen
#'
#' @description get_table_export soll erzeugte Mittelwert-oder Anteilswertswert-Tabellen als csv Exportieren
#' 
#' @param table produzierter data.frame aus get_protected_values (z.B. plattform_data)
#' @param variable Name Analysevariable aus den Rohdaten als string ("pglabnet")
#' @param metadatapath Pfad zu den Metadaten mit Variablennamen und Tabellentyp im Datensatz als string
#' @param exportpath Exportordner als string
#' @param diffcount Anzahl an Differenzierungen als numeric (0-3 eraubt)
#' @param tabletype Art der zu verarbeitenden Tabelle ("mean" oder "prop")
#' 
#' @return data.csv = exportierte Tabelle als csv
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords data.csv
#'  
#' @examples
#'        get_table_export(table = protected.table, variable = "usedvariable", 
#'                         metadatapath = paste0(metapath, "varnames.csv"),
#'                         exportpath = exportpath, diffcount = 2, 
#'                         tabletype = "mean")

get_table_export <- function(table, variable, metadatapath, exportpath, diffcount, tabletype) {
  
  metadata <- read.csv(metadatapath , header = TRUE)
  variable <- variable
  path <- file.path(exportpath, variable)
  dir.create(path, showWarnings = FALSE)

  folder <- paste0(variable, "/")
  
  diffvars <- 1+diffcount
  filenames  <- names(table)[2:diffvars]
  
  if(tabletype=="mean"){
    diffvars <- 1+diffcount
    filenames  <- names(table)[2:diffvars]
  }
  
  if(tabletype=="prop"){
    diffvars <- 2+diffcount
    filenames  <- names(table)[3:diffvars]
  }
  
  if (diffcount == 3) {
    filename <- paste0(variable, "_", "year", "_", metadata$variable[metadata$variable==filenames[1]], "_",
                       metadata$variable[metadata$variable==filenames[2]], "_", 
                       metadata$variable[metadata$variable==filenames[3]])
  }
  
  if (diffcount == 2 ) {
    filename <- paste0(variable, "_", "year", "_", metadata$variable[metadata$variable==filenames[1]], "_",
                       metadata$variable[metadata$variable==filenames[2]])
  }
  
  if (diffcount == 1) {
    filename <- paste0(variable, "_", "year", "_", metadata$variable[metadata$variable==filenames[1]])
  }
  
  if (diffcount == 0) {
    filename <- paste0(variable, "_", "year")
  }
  
  if(tabletype=="mean"){
    export <- paste0(exportpath, folder, "mean_", filename, ".csv")
  }
  
  if(tabletype=="prop"){
    export <- paste0(exportpath, folder, "prop_", filename, ".csv")
  }
  
  data.csv <- sapply(table, as.character)
  data.csv[is.na(data.csv)] <- ""
  data.csv <- as.data.frame(data.csv)
  
  write.csv(data.csv, export, row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8")
  return(data.csv)
}



