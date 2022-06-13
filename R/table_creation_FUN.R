################################################################################
# Functions #
################################################################################

`%>%` <- dplyr::`%>%`

#' @title loadpackage loads required R-packages
#'
#' @description loadpackage should install needed packages if necessary and load them into library
#'
#' @param x Names of the required R-packages as vector (e.g. c("foreign", "dplyr", "tidyverse", "readstata13"))
#' 
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords loadpackage
#'  
#' @examples
#'       loadpackage(c("foreign", "dplyr", "tidyverse", "readstata13")) 

# TODO: Remove this. This is bad practice for using libraries.
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


#' @title get_data creates subset of a data set
#'
#' @description get_data to get the output dataset in long format 
#' limited to certain variables (variable, year, weight, diffvars) and
#' contain only valid values.
#'
#' @param datasetnum data.frame with numeric variables (e.g. platform_data)
#' @param datasetfac data.frame with factor variables (e.g. platform_data)
#' @param variable name analysis variable as string (e.g. "pglabnet" )
#' @param year Name survey year variable as string (e.g. "syear" )
#' @param weight Name weight variable as string (e.g. "phrf")
#' @param diffcount Number of desired differentiations (e.g. 2) (range 0-3)
#' @param diffvars Vector with differentiation variables (e.g. c("age_gr", "sex", "education level")) (maximum 3 variables)
#' @param vallabel Valuelabel should be used (e.g.: vallabel = TRUE) (TRUE/FALSE)
#' 
#' @return variable.values.valid is a data set with valid values of the 
#' variables (variable, year, weight, diffvars)
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
# TODO: Too many arguments. Arguments are badly named. Possibly too many if statements.
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

#' @title get_mean_values creates mean/median tables with mean, median, n, percentiles, confidence interval
#' 
#' @description get_mean_values creates weighted mean/median tables with
#'              n, percentiles, confidence interval
#'
#' @param dataset data.frame from get_data (e.g. platform_data)
#' @param year year variable as string (e.g. "year")
#' @param diffcount number of desired differentiations (e.g. 2) (range 0-3)
#' @param diffvar1 name differentiation variable 1 as string ("" possible)
#' @param diffvar2 name differentiation variable 2 as string ("" possible)
#' @param diffvar3 Number of differentiation variable 3 as string ("" possible)
#' 
#' @return data = dataset with mean, median, n, percentiles, confidence interval
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
# TODO: Some arguments are badly named. diffvar1-3 could be one data structure.
# TODO: Too many if statements
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
    dplyr::group_by_at(vars(one_of(columns))) %>%
    dplyr::mutate(mean = round(weighted.mean(usedvariable, weight),2))  %>%
    dplyr::mutate(median = round(weighted.median(usedvariable, weight),2))  %>%
    dplyr::add_count(year, wt = NULL)  %>%
    dplyr::mutate(sd = sd(usedvariable/sqrt(n))) %>%
    dplyr::mutate(lower = mean - qt(1 - (0.05 / 2), as.numeric(n) - 1) * sd,
           upper = mean + qt(1 - (0.05 / 2), as.numeric(n) - 1) * sd) %>%
    dplyr::mutate(lowerci_mean = round((lower),2))  %>%
    dplyr::mutate(upperci_mean = round((upper),2))  %>%
    dplyr::mutate(max = round(max(usedvariable, na.rm = T), 2),
           min = round(min(usedvariable, na.rm = T), 2))   %>%
    dplyr::distinct(mean, .keep_all = TRUE)

  
  percentile.values <-  dataset[complete.cases(dataset), ] %>%
    dplyr::group_by_at(vars(one_of(columns)))  %>% 
    dplyr::summarise(ptile10 = round(Hmisc::wtd.quantile(usedvariable, weights = weight, 
                                     probs = .1, na.rm = TRUE),2),
              ptile25 = round(Hmisc::wtd.quantile(usedvariable, weights = weight, 
                                     probs = .25, na.rm = TRUE),2),
              ptile75 = round(Hmisc::wtd.quantile(usedvariable, weights = weight, 
                                     probs = .75, na.rm = TRUE),2),
              ptile90 = round(Hmisc::wtd.quantile(usedvariable, weights = weight, 
                                     probs = .90, na.rm = TRUE),2), 
              ptile99 = round(Hmisc::wtd.quantile(usedvariable, weights = weight, 
                                           probs = .99, na.rm = TRUE),2), .groups = 'drop')

  # Median confidence interval calculation
  median_data <- dataset %>% 
    dplyr::group_by_at(vars(one_of(columns))) %>% 
    dplyr::filter(dplyr::n() > 8)
  
  medianci.value<- median_data[complete.cases(median_data), ] %>% 
    tidyr::nest(data = -columns) %>%
    dplyr::mutate(ci = purrr::map(data, ~ 
                                    DescTools::MedianCI(.x$usedvariable, 
                                                        method = "exact")[2:3])) %>% 
    tidyr::unnest_wider(ci)
  
  medianci.value$data <- NULL
  colnames(medianci.value) <- c(columns, "lowerci_median", "upperci_median")
  
  data <- merge(mean.values,percentile.values,by=columns)
  
  data <- dplyr::left_join(data,medianci.value,by=columns)
  
  selected.values <- c(columns, "mean", "lowerci_mean", "upperci_mean", 
                       "median", "lowerci_median", "upperci_median", 
                       "ptile10", "ptile25", "ptile75", "ptile90", "ptile99", "n", "min", "max")
  
  data <- data[,(names(data) %in% selected.values)]
  data <-  data %>% 
    dplyr::arrange(desc(year), .by_group = TRUE)
  
  return(data)
}

################################################################################
#' @title function get_prop_values shall create weighted proportion tables with confidence intervals
#'
#' @description get_mean_values shall create weighted proportion value tables with confidence intervals
#' create with the information n = size of the subgroup, percent = weighted
#' proportion value, lower_confidence = lower confidence interval, upper_confidence = upper 95% confidence interval
#' 
#' @param dataset data.frame from get_data (e.g. platform_data)
#' @param groupvars vector with all variables in the dataset (e.g. c("usedvariable", "year", "sex"))
#' @param alpha Alpha for setting the confidence interval (e.g. 0.05)
#' 
#' @return data_prop_complete_ci = data set with n, percent, lower_confidence, upper_confidence
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords get_prop_values
#'  
#' @examples
#'       get_prop_values(dataset = data, 
#'                       groupvars = c("usedvariable", "year", "sex"), 
#'                       alpha = 0.05)

get_prop_values <- function(dataset, groupvars, alpha) {
  
  data_prop1 <-  dataset[complete.cases(dataset), ] %>%
    dplyr::group_by_at(vars(one_of(groupvars))) %>%
    dplyr::summarise(count_w = sum(weight), .groups="drop_last")
  
  data_prop2 <-  data_prop1[complete.cases(data_prop1), ] %>% 
    dplyr::group_by(eval(parse(text = groupvars[2])), 
                    eval(parse(text = groupvars[3])),
                    eval(parse(text = groupvars[4]))) %>%
    dplyr::mutate(sum_count_w = sum(count_w)) 
  
  data_prop3 <- dataset[complete.cases(dataset), ] %>% 
    dplyr::group_by_at(vars(one_of(groupvars))) %>%
    dplyr::summarise(n = dplyr::n(), .groups="drop_last") 
  
  data_prop4 <- data_prop3[complete.cases(data_prop3), ] %>% 
    dplyr::group_by(eval(parse(text = groupvars[2])), eval(parse(text = groupvars[3])),
             eval(parse(text = groupvars[4]))) %>%
    dplyr::mutate(n_total = sum(n)) 
  
  data_prop <- cbind(data_prop1, data_prop2["sum_count_w"], 
                     data_prop3["n"], 
                     data_prop4["n_total"])
  data_prop <- data_prop[order(data_prop$year),]
  
  data_prop_complete <-  data_prop[complete.cases(data_prop1), ] %>% 
    dplyr::mutate(percent = count_w/sum_count_w,)
  
  n_total <- data_prop_complete$n_total
  p_hat <- data_prop_complete$percent
  alpha <- alpha
  
  margin1 <- qnorm(1-alpha/2)*sqrt(p_hat*(1-p_hat)/n_total)
  
  # Compute the CI
  lower_confidence1 <- p_hat - margin1
  upper_confidence1 <- p_hat + margin1
  
  data_prop_complete_ci <- cbind(data_prop_complete, 
                                 lower_confidence=lower_confidence1, 
                                 upper_confidence=upper_confidence1)
  
  data_prop_complete_ci <- subset(data_prop_complete_ci, 
                                  select=c(groupvars, "n", "percent", 
                                           "lower_confidence", "upper_confidence")) 
  
  return(data_prop_complete_ci)
}

################################################################################
#' @title get_protected_values should remove certain cell contents
#'
#' @description get_protected_values should remove cell contents of weighted proportion tables or mean value table 
#' delete if a minimum population is not reached.
#' 
#' @param dataset data.frame from get_prop_values or get_mean_table 
#' @param cell.size maximum allowed cell size (e.g. 30)
#' 
#' @return protected.data (dataset with n, mean/percent, median, n, confidence intervals only with cells >= cell.size)
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords get_prop_values
#'  
#' @examples
#'       get_prop_values(dataset = data, alpha = 0.05)

get_protected_values <- function(dataset, cell.size) {
  
  if(("mean" %in% colnames(dataset))==TRUE){
    
    save.data <- as.data.frame(apply(dataset[c("mean",  "median", "n",  
                                               "ptile10", "ptile25", "ptile75", "ptile90", "ptile99", 
                                               "lowerci_mean", "upperci_mean", "min", "max",
                                               "lowerci_median", "upperci_median")], 2, 
                                     function(x) ifelse(dataset["n"] < cell.size, NA, x)))
    data <- dataset
    dataset[c("mean",  "median", "n",  
              "ptile10", "ptile25", "ptile75", "ptile90", "ptile99",  
              "lowerci_mean", "upperci_mean", "min", "max",
              "lowerci_median", "upperci_median")] <- NULL
    
  }
  
  if(("percent" %in% colnames(dataset))==TRUE){
    
    save.data <- as.data.frame(apply(dataset[c("percent", 
                                               "lower_confidence", "upper_confidence")], 2, 
                                     function(x) ifelse(dataset["n"] < cell.size, NA, x)))
    data <- dataset
    dataset[c("percent", 
              "lower_confidence", "upper_confidence")] <- NULL
    
  }
  protected.data <- cbind(dataset, save.data)
  return(protected.data)
}

################################################################################
#' @title create_table_lables data set with vauluelabel
#'
#' @description create_table_lables is to provide specific variables of a dataset with vauluelabel
#' 
#' @param table data.frame from get_mean_data 
#' 
#' @return data_with_label = data set with value labels
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords data_with_label
#'  
#' @examples
#'       create_table_lables(table = data)

create_table_lables <- function(table) {
  data_with_label <- table
  
  if("sex" %in% colnames(data_with_label)){
    data_with_label$sex <- gsubfn::gsubfn(".", 
                                          list("1" = "Male", "2" = "Female"), 
                                          as.character(data_with_label$sex))
  }
  
  if("bula_h" %in% colnames(data_with_label)){
    data_with_label$bula_h <-data_with_label$bula_h %>%
      gsub("11", "Berlin", .) %>%
      gsub("12", "Brandenburg", .) %>%
      gsub("13", "Mecklenburg-Western Pomerania", .) %>%
      gsub("14", "Saxony", .) %>%
      gsub("15", "Saxony-Anhalt", .) %>%
      gsub("16", "Thuringia", .) %>% 
      gsub("1", "Schleswig-Holstein", .) %>%
      gsub("2", "Hamburg", .) %>%
      gsub("3", "Lower Saxony", .) %>%
      gsub("4", "Bremen", .) %>%
      gsub("5", "North Rhine-Westphalia", .) %>%
      gsub("6", "Hesse", .) %>%
      gsub("7", "Rhineland-Palatinate,Saarland", .) %>% 
      gsub("8", "Baden-Württemberg", .) %>% 
      gsub("9", "Bavaria", .)
  }
  
  if("sampreg" %in% colnames(data_with_label)){
    data_with_label$sampreg <- gsubfn::gsubfn(".", list("1" = "West Germany", 
                                                        "2"  = "East Germany"), 
                                              as.character(data_with_label$sampreg))
  }
  
  if("pgcasmin" %in% colnames(data_with_label)){
    data_with_label$pgcasmin <- gsubfn::gsubfn(".", list("0" = "(0) in school", "1"  = "(1a) inadequately completed", 
                                                 "2" = "(1b) general elementary school", "3"  = "(1c) basic vocational qualification", 
                                                 "4" = "(2b) intermediate general qualification", "5"  = "(2a) intermediate vocational", 
                                                 "6" = "(2c_gen) general maturity certificate", "7"  = "(2c_voc) vocational maturity certificate",
                                                 "8" = "(3a) lower tertiary education", "9" = "(3b) higher tertiary education"), as.character(data_with_label$pgcasmin))
  }
  
  if("pgisced97" %in% colnames(data_with_label)){
    data_with_label$pgisced97 <- gsubfn::gsubfn(".", list("0" = "in school", "1"  = "inadequately", 
                                                 "2" = "general elemantary", "3"  = "middle vocational", 
                                                 "4" = "vocational + Abi", "5"  = "higher vocational", 
                                                 "6" = "higher education"), as.character(data_with_label$pgisced97))
  }
  
  if("age_gr" %in% colnames(data_with_label)){
    data_with_label$age_gr <- gsubfn::gsubfn(".", list("1"  = "16-34 y.", 
                                               "2" = "35-65 y.", "3"  = "66 and older"), as.character(data_with_label$age_gr))
  }
  
  if("education" %in% colnames(data_with_label)){
    data_with_label$education <- gsubfn::gsubfn(".", list("1"  = "lower secondary degree", 
                                                  "2" = "secondary school degree", "4"  = "college entrance qualification",
                                                  "5" = "Other degree", "6"  = "no degree/no degree yet"), 
                                        as.character(data_with_label$education))
  }
  
  if("migback" %in% colnames(data_with_label)){
    data_with_label$migback <- gsubfn::gsubfn(".", list("1"  = "no migration background", 
                                                "2" = "direct migration background", 
                                                "3"  = "indirect migration background"), 
                                      as.character(data_with_label$migback))
  }
  
  if("e11102" %in% colnames(data_with_label)){
    data_with_label$e11102 <- gsubfn::gsubfn(".", list("0"  = "Not Employed", 
                                                "1" = "Employed"), 
                                      as.character(data_with_label$e11102))
  }
  
  if("e11103" %in% colnames(data_with_label)){
    data_with_label$e11103 <- gsubfn::gsubfn(".", list("1"  = "Full Time", 
                                                "2" = "Part Time",
                                                "3"  = "Not Working"), 
                                      as.character(data_with_label$e11103))
  }
  
  if("regtyp" %in% colnames(data_with_label)){
    data_with_label$regtyp <- gsubfn::gsubfn(".", list("1" = "Urban Area", "2" = "Rural Area"), 
                                     as.character(data_with_label$regtyp))
  }
  
  if("hgtyp1hh" %in% colnames(data_with_label)){
    data_with_label$hgtyp1hh <- gsubfn::gsubfn(".", list("1"  = "1-pers.-HH", 
                                                "2" = "(Married) couple without C.",
                                                "3"  = "Single parent",
                                                "4" = "Couple + C. LE 16",
                                                "5"  = "Couple + C. GT 16 ",
                                                "6" = "Couple + C. LE and GT 16"), 
                                      as.character(data_with_label$hgtyp1hh))
  }
  
  return(data_with_label)
}


################################################################################
#' @title get_table_export Export of mean value or proportion value tables
#'
#' @description get_table_export export created mean or proportion tables as csv
#' 
#' @param table produced data.frame from get_protected_values (e.g. platform_data)
#' @param variable name analysis variable from raw data as string ("pglabnet")
#' @param metadatapath Path to the metadata with variable name and table type in the dataset as string
#' @param exportpath export folder as string
#' @param diffcount number of differentiations as numeric (0-3 robbed)
#' @param tabletype Type of table to be processed ("mean" or "prop")
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
  
  if(tabletype=="mean"){
    path <- file.path(exportpath, "numerical", variable, "/")
    diffvars <- 1+diffcount
    filenames  <- names(table)[2:diffvars]
  }
  
  if(tabletype=="prop"){
    path <- file.path(exportpath, "categorical", variable, "/")
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
  
  dir.create(path, showWarnings = FALSE)
  data.csv <- sapply(table, as.character)
  data.csv[is.na(data.csv)] <- ""
  data.csv <- as.data.frame(data.csv)
  
  data.csv <- as.data.frame(apply(data.csv,2,
                                  function(x)gsub('^\\[[0-9]*]', '',x)))
  
  data.csv <- as.data.frame(apply(data.csv,2,
                                  function(x)gsub('^\\s+', '',x)))
  
  if(tabletype=="mean"){
    export <- paste0(path, filename, ".csv")
  }
  
  if(tabletype=="prop"){
    export <- paste0(path, filename, ".csv")
    colnames(data.csv)[1] <- variable
  }

  write.csv(data.csv, export, row.names = FALSE, quote = TRUE, fileEncoding = "UTF-8")
  return(data.csv)
}

################################################################################
################################################################################
# Creation of blank lines when information is empty in a year

#' @title expand_table 
#'
#' @description expand_table add empty rows if variable in year is empty. 
#'
#' @table data.frame to be filled with empty cells
#' @diffvar1 differentiation dimension as character
#' @diffvar2 Differentiation dimension as character
#' @diffvar3 differentiation dimension as character
#' @diffcount number of differentiations as numeric
#' @tabletype Table type ("prop" or "mean")
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#'  
#' @examples expand_table(table = protected.table, diffvar1 = diffvar1, 
#'                        diffvar2 = diffvar2, diffvar3 = diffvar3,
#'                        diffcount = diffcount, tabletype = "prop")

expand_table <- function(table, diffvar1, diffvar2, diffvar3, diffcount, tabletype) {
  
  start_year <- as.numeric(unique(table$year)[1])
  end_year <- as.numeric(unique(table$year)[length(unique(table$year))])
  
  if (tabletype == "mean") {
    if(diffcount == 0){
      expand.table <- expand.grid(year=seq(start_year, end_year))
    }
    if(diffcount == 1){
      expand.table <- expand.grid(year=seq(start_year, end_year), 
                                  diffvar1=unique(dplyr::pull(table, diffvar1)))
      names(expand.table) <- c("year", diffvar1)
    }
    
    if(diffcount == 2){
      expand.table <- expand.grid(year=seq(start_year, end_year), 
                                  diffvar1=unique(dplyr::pull(table, diffvar1)),
                                  diffvar2=unique(dplyr::pull(table, diffvar2)))
      names(expand.table) <- c("year", diffvar1, diffvar2)
    }
    
    if(diffcount == 3){
      expand.table <- expand.grid(year=seq(start_year, end_year), 
                                  diffvar1=unique(dplyr::pull(table, diffvar1)),
                                  diffvar2=unique(dplyr::pull(table, diffvar2)),
                                  diffvar3=unique(dplyr::pull(table, diffvar3)))
      names(expand.table) <- c("year", diffvar1, diffvar2, diffvar3)
    }
  }
  if (tabletype == "prop") {
    if(diffcount == 0){
      expand.table <- expand.grid(year=seq(start_year, end_year),
                                  usedvariable=unique(dplyr::pull(table, usedvariable)))
    }
    if(diffcount == 1){
      expand.table <- expand.grid(year=seq(start_year, end_year), 
                                  usedvariable=unique(dplyr::pull(table, usedvariable)),
                                  diffvar1=unique(dplyr::pull(table, diffvar1)))
      names(expand.table) <- c("year", "usedvariable", diffvar1)
    }
    
    if(diffcount == 2){
      expand.table <- expand.grid(year=seq(start_year, end_year), 
                                  usedvariable=unique(dplyr::pull(table, usedvariable)),
                                  diffvar1=unique(dplyr::pull(table, diffvar1)),
                                  diffvar2=unique(dplyr::pull(table, diffvar2)))
      names(expand.table) <- c("year", "usedvariable", diffvar1, diffvar2)
    }
    
    if(diffcount == 3){
      expand.table <- expand.grid(year=seq(start_year, end_year), 
                                  usedvariable=unique(dplyr::pull(table, usedvariable)),
                                  diffvar1=unique(dplyr::pull(table, diffvar1)),
                                  diffvar2=unique(dplyr::pull(table, diffvar2)),
                                  diffvar3=unique(dplyr::pull(table, diffvar3)))
      names(expand.table) <- c("year", "usedvariable", diffvar1, diffvar2, diffvar3)
    }
  }
  final <- merge(table, expand.table, all.y = TRUE)
  final <- final[with(final, order(year)), ]
  return(final)
}  

################################################################################
# creation of json metadata

#' @title json_create_lite 
#'
#' @description json_create_lite creates json metadata
#'
#' @variable variable names as character
#' @varlabel variable label as character
#' @startyear start year of the information as numeric
#' @endyear end of year information as numeric
#' @tabletype table type (e.g. "mean", "prop", "both")
#' @exportpath path where json file will be stored
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords column_count_check Spaltenanzahl
#'  
#' @examples column_count_check(data = data, columns = columns)

json_create_lite <- function(variable, varlabel, startyear, endyear, tabletype, exportpath) {
  if (tabletype == "mean") {
    json_output <- jsonlite::toJSON(
      x = list(
        "title" = varlabel,
        "variable" = variable,
        "statistics" = c("mean", "lowerci_mean", "upperci_mean", "min", "max",
                         "median", "lowerci_median", "upperci_median", 
                         "ptile10", "ptile25", "ptile75", "ptile90", "ptile99"),
        "dimensions" = list(
          list("variable" = meta$variable[meta$variable == "age_gr"], 
               "label" = meta$label_de[meta$variable == "age_gr"],
               "values" = list("16-34 y.", "35-65 y.",
                               "66 and older")),
          list("variable" = meta$variable[meta$variable == "sex"], 
               "label" = meta$label_de[meta$variable == "sex"],
               "values" = list("Male", "Female")),
          list("variable" = meta$variable[meta$variable == "bula_h"], 
               "label" = meta$label_de[meta$variable == "bula_h"],
               "values" = list("Schleswig-Holstei", "Hamburg",
                               "Lower Saxony", "Bremen", "North Rhine-Westphalia",
                               "Hesse", "Rhineland-Palatinate,Saarland", "Baden-Württemberg", 
                               "Bavaria", "Berlin", "Brandenburg", "Mecklenburg-Western Pomerania", "Saxony",
                               "Saxony-Anhalt", "Thuringia")),
          list("variable" = meta$variable[meta$variable == "education"], 
               "label" = meta$label_de[meta$variable == "education"],
               "values" = list("lower secondary degree", "secondary school degree",
                               "college entrance qualification", "Other degree", 
                               "no degree/no degree yet")),
          list("variable" = meta$variable[meta$variable == "pgcasmin"], 
               "label" = meta$label_de[meta$variable == "pgcasmin"],
               "values" = list("(0) in school", "(1a) inadequately completed", 
                               "(1b) general elementary school", "(1c) basic vocational qualification", 
                               "(2b) intermediate general qualification", "(2a) intermediate vocational", 
                               "(2c_gen) general maturity certificate", "(2c_voc) vocational maturity certificate",
                               "(3a) lower tertiary education", "(3b) higher tertiary education")),
          list("variable" = meta$variable[meta$variable == "pgisced97"], 
               "label" = meta$label_de[meta$variable == "pgisced97"],
               "values" = list("in school","inadequately", 
                               "general elemantary", "middle vocational", 
                               "vocational + Abi", "higher vocational", 
                               "higher education")),
          list("variable" = meta$variable[meta$variable == "sampreg"], 
               "label" = meta$label_de[meta$variable == "sampreg"],
               "values" = list("West Germany", 
                               "East Germany")),
          list("variable" = meta$variable[meta$variable == "e11102"], 
               "label" = meta$label_de[meta$variable == "e11102"],
               "values" = list("Not Employed", 
                               "Employed")),
          list("variable" = meta$variable[meta$variable == "e11103"], 
               "label" = meta$label_de[meta$variable == "e11103"],
               "values" = list("Full Time", 
                               "Part Time",
                               "Not Working")),
          list("variable" = meta$variable[meta$variable == "hgtyp1hh"], 
               "label" = meta$label_de[meta$variable == "hgtyp1hh"],
               "values" = list("1-pers.-HH",
                               "(Married) couple without C.", 
                               "Single parent",
                               "Couple + C. LE 16",
                               "Couple + C. GT 16",
                               "Couple + C. LE and GT 16")),
          list("variable" = meta$variable[meta$variable == "migback"], 
               "label" = meta$label_de[meta$variable == "migback"],
               "values" = list("no migration background", 
                               "direct migration background",
                               "indirect migration background")),
          list("variable" = meta$variable[meta$variable == "regtyp"], 
               "label" = meta$label_de[meta$variable == "regtyp"],
               "values" = list(as.list(str_trim(gsub("[[0-9]+]", "", levels(factor(data.file.fac$regtyp)))))))

        ),
        "description_de" = meta$description[meta$variable==variable],
        "start_year" = startyear,
        "end_year" = endyear,
        "types" = list("numerical")
      ), encoding = "UTF-8", pretty = TRUE, auto_unbox=TRUE
    )
    
    file_handler <- file("meta.json")
    writeLines(json_output, exportpath, useBytes=TRUE)
    close(file_handler)
  }
  
  if (tabletype == "prop") {
    json_output <- jsonlite::toJSON(
      x = list(
        "title" = varlabel,
        "variable" = variable,
        "statistics" = c("percent", "lower_confidence" ,"upper_confidence"),
        "dimensions" = list(
          list("variable" = meta$variable[meta$variable == "age_gr"], 
               "label" = meta$label_de[meta$variable == "age_gr"],
               "values" = list("16-34 y.", "35-65 y.",
                               "66 and older")),
          list("variable" = meta$variable[meta$variable == "sex"], 
               "label" = meta$label_de[meta$variable == "sex"],
               "values" = list("Male", "Female")),
          list("variable" = meta$variable[meta$variable == "bula_h"], 
               "label" = meta$label_de[meta$variable == "bula_h"],
               "values" = list("Schleswig-Holstei", "Hamburg",
                               "Lower Saxony", "Bremen", "North Rhine-Westphalia",
                               "Hesse", "Rhineland-Palatinate,Saarland", "Baden-Württemberg", 
                               "Bavaria", "Berlin", "Brandenburg", "Mecklenburg-Western Pomerania", "Saxony",
                               "Saxony-Anhalt", "Thuringia")),
          list("variable" = meta$variable[meta$variable == "education"], 
               "label" = meta$label_de[meta$variable == "education"],
               "values" = list("lower secondary degree", "secondary school degree",
                               "college entrance qualification", "Other degree", 
                               "no degree/no degree yet")),
          list("variable" = meta$variable[meta$variable == "pgcasmin"], 
               "label" = meta$label_de[meta$variable == "pgcasmin"],
               "values" = list("(0) in school", "(1a) inadequately completed", 
                               "(1b) general elementary school", "(1c) basic vocational qualification", 
                               "(2b) intermediate general qualification", "(2a) intermediate vocational", 
                               "(2c_gen) general maturity certificate", "(2c_voc) vocational maturity certificate",
                               "(3a) lower tertiary education", "(3b) higher tertiary education")),
          list("variable" = meta$variable[meta$variable == "pgisced97"], 
               "label" = meta$label_de[meta$variable == "pgisced97"],
               "values" = list("in school","inadequately", 
                               "general elemantary", "middle vocational", 
                               "vocational + Abi", "higher vocational", 
                               "higher education")),
          list("variable" = meta$variable[meta$variable == "sampreg"], 
               "label" = meta$label_de[meta$variable == "sampreg"],
               "values" = list("West Germany", 
                               "East Germany")),
          list("variable" = meta$variable[meta$variable == "e11102"], 
               "label" = meta$label_de[meta$variable == "e11102"],
               "values" = list("Not Employed", 
                               "Employed")),
          list("variable" = meta$variable[meta$variable == "e11103"], 
               "label" = meta$label_de[meta$variable == "e11103"],
               "values" = list("Full Time", 
                               "Part Time",
                               "Not Working")),
          list("variable" = meta$variable[meta$variable == "hgtyp1hh"], 
               "label" = meta$label_de[meta$variable == "hgtyp1hh"],
               "values" = list("1-pers.-HH",
                               "(Married) couple without C.", 
                               "Single parent",
                               "Couple + C. LE 16",
                               "Couple + C. GT 16",
                               "Couple + C. LE and GT 16")),
          list("variable" = meta$variable[meta$variable == "migback"], 
               "label" = meta$label_de[meta$variable == "migback"],
               "values" = list("no migration background", 
                               "direct migration background",
                               "indirect migration background")),
          list("variable" = meta$variable[meta$variable == "regtyp"], 
               "label" = meta$label_de[meta$variable == "regtyp"],
               "values" = list(as.list(str_trim(gsub("[[0-9]+]", "", levels(factor(data.file.fac$regtyp)))))))
        ),
        "description_de" = meta$description[meta$variable==variable],
        "start_year" = startyear,
        "end_year" = endyear,
        "types" = "categorical"
      ), encoding = "UTF-8", pretty = TRUE, auto_unbox=TRUE
    )
    
    file_handler <- file("meta.json")
    writeLines(json_output, exportpath, useBytes=TRUE)
    close(file_handler)
  }
  
  if (tabletype == "both") {
    json_output <- jsonlite::toJSON(
      x = list(
        "title" = varlabel,
        "variable" = variable,
        "statistics" = c("mean", "lowerci_mean", "upperci_mean", 
                         "median", "lowerci_median", "upperci_median", 
                         "ptile10", "ptile25", "ptile75", "ptile90", "n",
                         "percent", "lower_confidence" ,"upper_confidence"),
        "dimensions" = list(
          list("variable" = meta$variable[meta$variable == "age_gr"], 
               "label" = meta$label_de[meta$variable == "age_gr"],
               "values" = list("16-34 y.", "35-65 y.",
                               "66 and older")),
          list("variable" = meta$variable[meta$variable == "sex"], 
               "label" = meta$label_de[meta$variable == "sex"],
               "values" = list("Male", "Female")),
          list("variable" = meta$variable[meta$variable == "bula_h"], 
               "label" = meta$label_de[meta$variable == "bula_h"],
               "values" = list("Schleswig-Holstei", "Hamburg",
                               "Lower Saxony", "Bremen", "North Rhine-Westphalia",
                               "Hesse", "Rhineland-Palatinate,Saarland", "Baden-Württemberg", 
                               "Bavaria", "Berlin", "Brandenburg", "Mecklenburg-Western Pomerania", "Saxony",
                               "Saxony-Anhalt", "Thuringia")),
          list("variable" = meta$variable[meta$variable == "education"], 
               "label" = meta$label_de[meta$variable == "education"],
               "values" = list("lower secondary degree", "secondary school degree",
                               "college entrance qualification", "Other degree", 
                               "no degree/no degree yet")),
          list("variable" = meta$variable[meta$variable == "pgcasmin"], 
               "label" = meta$label_de[meta$variable == "pgcasmin"],
               "values" = list("(0) in school", "(1a) inadequately completed", 
                               "(1b) general elementary school", "(1c) basic vocational qualification", 
                               "(2b) intermediate general qualification", "(2a) intermediate vocational", 
                               "(2c_gen) general maturity certificate", "(2c_voc) vocational maturity certificate",
                               "(3a) lower tertiary education", "(3b) higher tertiary education")),
          list("variable" = meta$variable[meta$variable == "pgisced97"], 
               "label" = meta$label_de[meta$variable == "pgisced97"],
               "values" = list("in school","inadequately", 
                               "general elemantary", "middle vocational", 
                               "vocational + Abi", "higher vocational", 
                               "higher education")),
          list("variable" = meta$variable[meta$variable == "sampreg"], 
               "label" = meta$label_de[meta$variable == "sampreg"],
               "values" = list("West Germany", 
                               "East Germany")),
          list("variable" = meta$variable[meta$variable == "e11102"], 
               "label" = meta$label_de[meta$variable == "e11102"],
               "values" = list("Not Employed", 
                               "Employed")),
          list("variable" = meta$variable[meta$variable == "e11103"], 
               "label" = meta$label_de[meta$variable == "e11103"],
               "values" = list("Full Time", 
                               "Part Time",
                               "Not Working")),
          list("variable" = meta$variable[meta$variable == "hgtyp1hh"], 
               "label" = meta$label_de[meta$variable == "hgtyp1hh"],
               "values" = list("1-pers.-HH",
                               "(Married) couple without C.", 
                               "Single parent",
                               "Couple + C. LE 16",
                               "Couple + C. GT 16",
                               "Couple + C. LE and GT 16")),
          list("variable" = meta$variable[meta$variable == "migback"], 
               "label" = meta$label_de[meta$variable == "migback"],
               "values" = list("no migration background", 
                               "direct migration background",
                               "indirect migration background")),
          list("variable" = meta$variable[meta$variable == "regtyp"], 
               "label" = meta$label_de[meta$variable == "regtyp"],
               "values" = list(as.list(str_trim(gsub("[[0-9]+]", "", levels(factor(data.file.fac$regtyp)))))))
        ),
        "description_de" = meta$description[meta$variable==variable],
        "start_year" = startyear,
        "end_year" = endyear,
        "types" = list("numerical", "categorical")
      ), encoding = "UTF-8", pretty = TRUE, auto_unbox=TRUE
    )
    
    file_handler <- file("meta.json")
    writeLines(json_output, exportpath, useBytes=TRUE)
    close(file_handler)
  }
}


