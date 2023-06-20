#############################################################################
# Create aggregated data tables for SOEP transfer project
#############################################################################
# Set global variables

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

alpha <- 0.05
metadata_path <- paste0(arguments$metadata, arguments$dataset_name, "/", 
                        arguments$version, "/")

doParallel::registerDoParallel(8)
options(dplyr.summarise.inform = FALSE)
################################################################################
# load data 

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
      arguments$input,
      arguments$dataset_name, ".dta"
    ),
    convert.factors = FALSE,
    encoding = "UTF-8"
  )

  # Delete cases with no weighting
  datafile_without_labels <- dplyr::filter(
    datafile_without_labels,
    arguments$weight_column > 0
  )

  # Rename weighting variable to weight and syear to year
  lookup <- c(year = arguments$survey_year_column, 
              weight = arguments$weight_column)
  datafile_without_labels <- dplyr::rename(datafile_without_labels, 
                                           all_of(lookup))

    ## load data with labels
  datafile_with_labels <- readstata13::read.dta13(
    paste0(
      arguments$input,
      arguments$dataset_name, ".dta"
    ),
    convert.factors = TRUE,
    nonint.factors = TRUE,
    encoding = "UTF-8"
  )

  # Weights with 0 cause problems
  datafile_with_labels <- dplyr::filter(
    datafile_with_labels,
    arguments$weight_column > 0
  )

  # Rename weighting variable to weight and syear to year
  datafile_with_labels <- dplyr::rename(datafile_with_labels, 
                                           all_of(lookup))

  # keep valid value labels
  metadaten_variable_categories <- dplyr::filter(
    metadaten_variable_categories,
    value >= 0
  )

  ### Code to create the aggregated tables in variables.csv
  metadaten_variables <-
    metadaten_variables[metadaten_variables$dataset == arguments$dataset_name, ]

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

  ##############################################################################
  # Create aggregated data tables
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

