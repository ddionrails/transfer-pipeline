#############################################################################
# Create aggregated data tables for SOEP transfer project
#############################################################################
### Path definition
if (Sys.info()[["user"]] == "szimmermann") {
  dataset_path <- "H:/data/"
  metadata_path <- "H:/Clone/soep-transfer/metadata/"
  export_path <- "C:/git/soep-transfer/"
}

# Definition of objects
dataset <- "p_statistics" # From which data set should values be taken
cell_minimum <- 30 # Maximum allowed cell size
year <- "syear" # Survey year must be defined
weight_variable <- "phrf" # Weight must be defined
#############################################################################

## load packages
## load data without labels
datafile_without_labels <- readstata13::read.dta13(paste0(dataset_path, 
                                                          dataset, ".dta"),
  convert.factors = FALSE, encoding = "UTF-8"
)

# Delete cases with no weighting
datafile_without_labels <- dplyr::filter(datafile_without_labels, 
                                         weight_variable > 0)

## load data without labels
datafile_with_labels <- readstata13::read.dta13(paste0(dataset_path, 
                                                dataset, ".dta"),
  convert.factors = TRUE,
  nonint.factors = TRUE, encoding = "UTF-8"
)

# Weights with 0 cause problems
datafile_without_labels <- dplyr::filter(datafile_without_labels, 
                                         weight_variable > 0)

# read metainformation
metadaten_variables <- read.csv(paste0(metadata_path, "variables.csv"),
  header = TRUE,
  colClasses = "character"
)

################################################################################
################################################################################
### Code to create the aggregated tables in variables.csv
metadaten_variables <- metadaten_variables[metadaten_variables$dataset==dataset, ]

metadaten_variables_demo <- dplyr::filter(metadaten_variables, 
                                          meantable == "demo")

metadaten_variables_demo <- subset(datafile_without_labels,
  select = metadaten_variables_demo$variable
)

# Generate a list that represents all the grouping possibilities of the users
# TODO: Hard to read. Should be encapsulated by a function and the function chaining
# TODO: should be broken up into seperate statements:
# TODO: sort(names(metadaten_variables_demo)) is duplicated here.
grouping_variables_list <- get_grouping_variables_list(
  metadaten_variables_demo = metadaten_variables_demo
  )

# Generate a list that represents ne number of differentiations for each possibility
# TODO: names(metadaten_variables_demo) seems to be used quite frequently.
# TODO: Could be better to store it in an extra variable.
# TODO: This might belong to the 'function' above.
# TODO: Purpose of renaming and changes are not clear.

grouping_count_list <- get_grouping_count_list(
  metadaten_variables_demo = metadaten_variables_demo
  )

##############################################################################################
# Create aggregated data tables
# TODO: Loop Body is way to long.
# TODO: Too many if statements. Too many if statements whith unclear purpose.
# TODO: Should be split into seperate functions.
for (var in 1:length(metadaten_variables$variable)) {
  if (metadaten_variables$meantable[var] == "Yes" | metadaten_variables$probtable[var] == "Yes") {
    variable <- metadaten_variables$variable[var]

    for (i in seq_along(grouping_variables_list)) {
      grouping_count <- grouping_count_list[[i]]
      grouping_variables <- grouping_variables_list[[i]]

      if (!is.na(grouping_variables[1])) {
        grouping_variable_one <- grouping_variables[1]
      } else {
        grouping_variable_one <- ""
      }

      if (!is.na(grouping_variables[2])) {
        grouping_variable_two <- grouping_variables[2]
      } else {
        grouping_variable_two <- ""
      }

      if (!is.na(grouping_variables[3])) {
        grouping_variable_three <- grouping_variables[3]
      } else {
        grouping_variable_three <- ""
      }

      if (metadaten_variables$meantable[var] == "Yes") {
        data <- get_data(
          datafile_without_labels = datafile_without_labels,
          datafile_with_labels = datafile_with_labels,
          variable = variable,
          year = year,
          weight = weight_variable,
          grouping_count = grouping_count,
          grouping_variables = grouping_variables,
          value_label = FALSE
        )


        table_numeric <- get_mean_values(
          dataset = data,
          year = "year",
          grouping_count = grouping_count,
          grouping_variable_one = grouping_variable_one,
          grouping_variable_two = grouping_variable_two,
          grouping_variable_three = grouping_variable_three
        )


        table_numeric <- create_table_lables(table = table_numeric)

        protected_table <- get_protected_values(dataset = table_numeric, 
                                                cell.size = cell_minimum)


        protected_table <- expand_table(
          table = protected_table, 
          grouping_variable_one = grouping_variable_one,
          grouping_variable_two = grouping_variable_two, 
          grouping_variable_three = grouping_variable_three,
          grouping_count = grouping_count, table_type = "mean"
        )

        data_csv <- get_table_export(
          table = protected_table, variable = variable,
          metadata_path = paste0(metadata_path, "variables.csv"),
          export_path = export_path, grouping_count = grouping_count,
          table_type = "mean"
        )

        json_create_lite(
          variable = variable,
          variable_label = datafile_without_labels$label_de[datafile_without_labels$variable == variable],
          start_year = as.numeric(unique(data_csv$year)[1]),
          end_year = as.numeric(unique(data_csv$year)[length(unique(data_csv$year))]),
          table_type = "mean",
          export_path = paste0(export_path, "/numerical/", variable, "/meta.json")
        )

        print(paste(
          "The variable", variable, "is processed with grouping year,",
          paste(grouping_variables_list[[i]], collapse = ","), "as numeric mean table"
        ))
      }

      if (datafile_without_labels$probtable[var] == "Yes") {
        data <- get_data(
          datafile_without_labels = datafile_without_labels,
          datafile_with_labels = datafile_with_labels,
          variable = variable,
          year = year,
          weight = weight_variable,
          grouping_count = grouping_count,
          grouping_variables = grouping_variables,
          value_label = TRUE
        )

        if (grouping_variables == "") {
          columns <- c("usedvariable", "year")
        } else {
          columns <- c("usedvariable", "year", grouping_variables)
        }

        prop.data <- get_prop_values(dataset = data, groupvars = columns, alpha = 0.05)

        protected_table <- get_protected_values(dataset = prop.data, cell.size = cell_minimum)

        protected_table <- expand_table(
          table = protected_table, grouping_variable_one = grouping_variable_one,
          grouping_variable_two = grouping_variable_two, grouping_variable_three = grouping_variable_three,
          grouping_count = grouping_count, table_type = "prop"
        )

        data_csv <- get_table_export(
          table = protected_table, variable = variable,
          metadata_path = paste0(metadata_path, "variables.csv"),
          export_path = export_path, grouping_count = grouping_count,
          table_type = "prop"
        )

        json_create_lite(
          variable = variable,
          variable_label = datafile_without_labels$label_de[datafile_without_labels$variable == variable],
          start_year = as.numeric(unique(data_csv$year)[1]),
          end_year = as.numeric(unique(data_csv$year)[length(unique(data_csv$year))]),
          table_type = "prop",
          export_path = paste0(export_path, "/categorical/", variable, "/meta.json")
        )

        print(paste(
          "The variable", variable, "is processed with grouping year",
          paste(grouping_variables_list[[i]], collapse = ","), "as a categorical percentage table"
        ))
      }
    }
  }
}