#############################################################################
# Create aggregated data tables for SOEP transfer project
#############################################################################
#' @include cli.R

### Path definition
# main <- function() {
# Problem:
# if (Sys.info()[["user"]] == "szimmermann") {
#  dataset_path <- "H:/data/"
# }
#
# Clutters the code, is highly dependent on a static environment.
# Publishes information about the private path structure and usernames.
# Instead use command line arguments and/or a config file
# arguments <- parse_arguments()

# Definition of objects
# dataset <- arguments$input # From which data set should values be taken
# cell_minimum <- arguments$minimal_group_size # Maximum allowed cell size
# year <- "syear" # Survey year must be defined
# weight_variable <- arguments$weight_column # Weight must be defined
# version <- "v37"

dataset <- "h_statistics"
version <- "v37"
cell_minimum <- 30 # Maximum allowed cell size
year <- "syear" # Survey year must be defined
weight_variable <- "hhrf" # Weight must be defined
alpha <- 0.05 # for confidence interval

dataset_path <- "H:/data/"
metadata_path <- paste0("https://git.soep.de/kwenzig/publicecoredoku/raw/master/datasets/",
                        dataset, "/", version, "/")
export_path <- "H:/Clone/soep-transfer/"

numeric_statistics_column_names <- c(
  "mean",
  "lower_confidence_mean",
  "upper_confidence_mean",
  "median",
  "lower_confidence_median",
  "upper_confidence_median",
  "percentile_10",
  "percentile_25",
  "percentile_75",
  "percentile_90",
  "percentile_99",
  "n",
  "minimum",
  "maximum"
)

categorical_statistics_column_names <- c(
  "percent",
  "lower_confidence_percent", 
  "upper_confidence_percent"
)

# Wo sollte man das starten und sollte man das beenden?
# Ausspielen wie viele Cores
# Cores wieder freilassen
# F?r alle Variablen mal ausprobieren
doParallel::registerDoParallel(8)
options(dplyr.summarise.inform = FALSE)
################################################################################

  metadaten_variables <-
    read.csv(
      paste0(metadata_path, "variables.csv"),
      header = TRUE,
      colClasses = "character",
      encoding = "UTF-8"
    )

  metadaten_variable_categories <-
    read.csv(
      paste0(metadata_path, "variable_categories.csv"),
      header = TRUE,
      encoding = "UTF-8"
    )

  ## load data without labels
  datafile_without_labels <- readstata13::read.dta13(
    paste0(
      dataset_path,
      dataset, ".dta"
    ),
    convert.factors = FALSE,
    encoding = "UTF-8"
  )

  # Delete cases with no weighting
  datafile_without_labels <- dplyr::filter(
    datafile_without_labels,
    weight_variable > 0
  )

  # Rename weighting variable to weight and syear to year
  lookup <- c(year = year, 
              weight = weight_variable)
  datafile_without_labels <- dplyr::rename(datafile_without_labels, 
                                           all_of(lookup))

    ## load data with labels
  datafile_with_labels <- readstata13::read.dta13(
    paste0(
      dataset_path,
      dataset, ".dta"
    ),
    convert.factors = TRUE,
    nonint.factors = TRUE,
    encoding = "UTF-8"
  )

  # Weights with 0 cause problems
  datafile_with_labels <- dplyr::filter(
    datafile_with_labels,
    weight_variable > 0
  )

  # Rename weighting variable to weight and syear to year
  datafile_with_labels <- dplyr::rename(datafile_with_labels, 
                                           all_of(lookup))

  # keep valid value labels
  metadaten_variable_categories <- dplyr::filter(
    metadaten_variable_categories,
    value >= 0
  )

  ##############################################################################
  ##############################################################################
  ### Code to create the aggregated tables in variables.csv
  metadaten_variables <-
    metadaten_variables[metadaten_variables$dataset == dataset, ]

  dimension_variables <- dplyr::filter(
    metadaten_variables,
    statistical_type == "dimension" 
  )
  
  dimension_variables <- subset(datafile_without_labels,
    select = dimension_variables$variable
  )

  # Generate a list that represents all the grouping possibilities of the users
  grouping_variables_list <- get_grouping_variables_list(
    dimension_variables = dimension_variables
  )

  # Generate a list that represents ne number of differentiations for each
  # possibility
  grouping_count_list <- get_grouping_count_list(
    dimension_variables = dimension_variables
  )

  ##############################################################################
  # Create aggregated data tables
 start <- Sys.time()
  for (var in 1:length(metadaten_variables$variable)) {
    # create tables for variables on numeric, categorical or ordinal level
    if (any(is.element(c('numerical', 'categorical', 'ordinal'), 
                   metadaten_variables$statistical_type[var]))) {
      variable <- metadaten_variables$variable[var]

    # create aggregated tables for all possible dimension combinations  
      for (i in seq_along(grouping_variables_list)) {
        
        grouping_variables <- grouping_variables_list[[i]]
        start <- Sys.time()
        if (metadaten_variables$statistical_type[var] == "numerical") {
          print(
            paste(
              "The variable",
              variable,
              "is processed with grouping year,",
              paste(grouping_variables_list[[i]], collapse = ","),
              "as numerical table"
            )
          )
        print_numeric_statistics()
        }
        
        # statistical type == categorical
        if (metadaten_variables$statistical_type[var] == "categorical") {

          print(
            paste(
              "The variable",
              variable,
              "is processed with grouping year",
              paste(grouping_variables_list[[i]], collapse = ","),
              "as a categorical percentage table"
            )
          )
        print_categorical_statistics()
        }
    }
  }
}
end <- Sys.time()
time <- end - start
