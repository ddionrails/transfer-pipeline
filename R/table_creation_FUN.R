################################################################################
# Functions #
################################################################################

`%>%` <- dplyr::`%>%`

#' @title get_data creates subset of a data set
#'
#' @description get_data to get the output dataset in long format
#' limited to certain variables (variable, year, weight, grouping_variables) and
#' contain only valid values.
#'
#' @param variable name analysis variable as string (e.g. "pglabnet" )
#' @param grouping_variables Vector with differentiation variables
#' (e.g. c("age_gr", "sex", "education level")) (maximum 3 variables)
#' @param value_label Valuelabel should be used (e.g.: value_label = TRUE)
#' (TRUE/FALSE)
#'
#' @return variable.values.valid is a data set with valid values of the
#' variables (variable, year, weight, grouping_variables)
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords get_data
#'
# TODO: Too many arguments. Arguments are badly named. Possibly too many if
# statements.
#'
get_data <-
  function(variable,
           grouping_variables,
           value_label) {
    columns <- c(
      variable, "syear", "weight",
      grouping_variables
    )

    columns <- columns[columns != ""]
    renamed_columns <- columns[columns != variable]
    renamed_columns <- dplyr::recode(renamed_columns, syear = "year")

    if (value_label == FALSE) {
      variable.values <- subset(datafile_without_labels,
        select = columns
      )
      names(variable.values) <-
        c(
          "usedvariablenum",
          renamed_columns
        )
    }
    if (value_label == TRUE) {
      variable.values <-
        subset(datafile_without_labels, select = variable)
      factorvar <-
        subset(datafile_with_labels,
          select = columns
        )
      variable.values <- cbind(variable.values, factorvar)
      names(variable.values) <-
        c(
          "usedvariablenum",
          "usedvariable",
          renamed_columns
        )
    }

    if (any(variable.values$usedvariablenum >= 0)) {
      if (value_label == FALSE) {
        variable.values.valid <-
          subset(variable.values, usedvariablenum >= 0)
        variable.values.valid <-
          variable.values.valid[order(variable.values.valid$usedvariablenum), ]
        names(variable.values.valid)[names(variable.values.valid) ==
          "usedvariablenum"] <- "usedvariable"
      }
      if (value_label == TRUE) {
        variable.values.valid <-
          subset(variable.values, usedvariablenum >= 0)
        variable.values.valid <-
          variable.values.valid[order(variable.values.valid$usedvariablenum), ]
        variable.values.valid <-
          variable.values.valid[2:length(variable.values.valid)]
      }
    }

    return(variable.values.valid)
  }

################################################################################

#' @title calculate_numeric_statistics creates mean/median tables with mean, median, n,
#' percentiles, confidence interval
#'
#' @description calculate_numeric_statistics creates weighted mean/median tables with
#'              n, percentiles, confidence interval
#'
#' @param dataset data.frame from get_data (e.g. platform_data)
#' @param grouping_variables Vector with differentiation variables
#' (e.g. c("age_gr", "sex", "education level")) (maximum 3 variables)
#' ("" possible)
#'
#' @return data = dataset with mean, median, n, percentiles, confidence interval
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords calculate_numeric_statistics
#'
# TODO: Some arguments are badly named. grouping_variable_one-3 could be one
# data structure.
# TODO: Too many if statements
#'
calculate_numeric_statistics <- function(dataset,
                            grouping_variables) {
  columns <- c("year", grouping_variables)
  columns <- columns[columns != ""]

  mean.values <- dataset[complete.cases(dataset), ] %>%
    dplyr::group_by_at(dplyr::vars(one_of(columns))) %>%
    dplyr::mutate(mean = round(weighted.mean(usedvariable, weight), 2)) %>%
    dplyr::mutate(median = round(spatstat.geom::weighted.median(usedvariable, weight), 2)) %>%
    dplyr::add_count(year, wt = NULL) %>%
    dplyr::mutate(sd = sd(usedvariable / sqrt(n))) %>%
    dplyr::mutate(
      lower = mean - qt(1 - (0.05 / 2), as.numeric(n) - 1) * sd,
      upper = mean + qt(1 - (0.05 / 2), as.numeric(n) - 1) * sd
    ) %>%
    dplyr::mutate(lower_confidence_mean = round((lower), 2)) %>%
    dplyr::mutate(upper_confidence_mean = round((upper), 2)) %>%
    dplyr::mutate(
      maximum = round(max(usedvariable, na.rm = T), 2),
      minimum = round(min(usedvariable, na.rm = T), 2)
    ) %>%
    dplyr::distinct(mean, .keep_all = TRUE)


  percentile.values <- dataset[complete.cases(dataset), ] %>%
    dplyr::group_by_at(dplyr::vars(one_of(columns))) %>%
    dplyr::summarise(
      percentile_10 = round(
        Hmisc::wtd.quantile(
          usedvariable,
          weights = weight,
          probs = .1,
          na.rm = TRUE
        ),
        2
      ),
      percentile_25 = round(
        Hmisc::wtd.quantile(
          usedvariable,
          weights = weight,
          probs = .25,
          na.rm = TRUE
        ),
        2
      ),
      percentile_75 = round(
        Hmisc::wtd.quantile(
          usedvariable,
          weights = weight,
          probs = .75,
          na.rm = TRUE
        ),
        2
      ),
      percentile_90 = round(
        Hmisc::wtd.quantile(
          usedvariable,
          weights = weight,
          probs = .90,
          na.rm = TRUE
        ),
        2
      ),
      percentile_99 = round(
        Hmisc::wtd.quantile(
          usedvariable,
          weights = weight,
          probs = .99,
          na.rm = TRUE
        ),
        2
      ),
      .groups = "drop"
    )

  # Median confidence interval calculation
  median_data <- dataset %>%
    dplyr::group_by_at(dplyr::vars(one_of(columns))) %>%
    dplyr::filter(dplyr::n() > 8)

  medianci.value <- median_data[complete.cases(median_data), ] %>%
    tidyr::nest(data = -columns) %>%
    dplyr::mutate(ci = purrr::map(
      data,
      ~
        DescTools::MedianCI(.x$usedvariable,
          method = "exact"
        )[2:3]
    )) %>%
    tidyr::unnest_wider(ci)

  medianci.value$data <- NULL
  colnames(medianci.value) <-
    c(columns, "lower_confidence_median", "upper_confidence_median")

  data <- merge(mean.values, percentile.values, by = columns)
  data <- dplyr::left_join(data, medianci.value, by = columns)

  selected.values <- c(
    columns,
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

  data <- data[, (names(data) %in% selected.values)]
  data <- data %>%
    dplyr::arrange(desc(year), .by_group = TRUE)

  return(data)
}

################################################################################
#' @title function calculate_categorical_statistics shall create weighted proportion tables with
#' confidence intervals
#'
#' @description calculate_categorical_statistics shall create weighted proportion value tables
#' with confidence intervals
#' create with the information n = size of the subgroup, percent = weighted
#' proportion value, lower_confidence = lower confidence interval,
#' upper_confidence = upper 95% confidence interval
#'
#' @param dataset data.frame from get_data (e.g. platform_data)
#' @param groupvars vector with all variables in the dataset
#' (e.g. c("usedvariable", "year", "sex"))
#' @param alpha Alpha for setting the confidence interval (e.g. 0.05)
#'
#' @return data_prop_complete_ci = data set with n, percent, lower_confidence,
#' upper_confidence
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords calculate_categorical_statistics
#'
#' @examples
#' calculate_categorical_statistics(
#'   dataset = data,
#'   groupvars = c("usedvariable", "year", "sex"),
#'   alpha = 0.05
#' )
#'
calculate_categorical_statistics <- function(dataset, groupvars, alpha) {
  data_prop1 <- dataset[complete.cases(dataset), ] %>%
    dplyr::group_by_at(dplyr::vars(one_of(groupvars))) %>%
    dplyr::summarise(count_w = sum(weight), .groups = "drop_last")

  data_prop2 <- data_prop1[complete.cases(data_prop1), ] %>%
    dplyr::group_by(
      eval(parse(text = groupvars[2])),
      eval(parse(text = groupvars[3])),
      eval(parse(text = groupvars[4]))
    ) %>%
    dplyr::mutate(sum_count_w = sum(count_w))

  data_prop3 <- dataset[complete.cases(dataset), ] %>%
    dplyr::group_by_at(dplyr::vars(one_of(groupvars))) %>%
    dplyr::summarise(n = dplyr::n(), .groups = "drop_last")

  data_prop4 <- data_prop3[complete.cases(data_prop3), ] %>%
    dplyr::group_by(
      eval(parse(text = groupvars[2])),
      eval(parse(text = groupvars[3])),
      eval(parse(text = groupvars[4]))
    ) %>%
    dplyr::mutate(n_total = sum(n))

  data_prop <- cbind(
    data_prop1, data_prop2["sum_count_w"],
    data_prop3["n"],
    data_prop4["n_total"]
  )
  data_prop <- data_prop[order(data_prop$year), ]

  data_prop_complete <- data_prop[complete.cases(data_prop1), ] %>%
    dplyr::mutate(percent = count_w / sum_count_w, )

  n_total <- data_prop_complete$n_total
  p_hat <- data_prop_complete$percent
  alpha <- alpha

  margin1 <-
    qnorm(1 - alpha / 2) * sqrt(p_hat * (1 - p_hat) / n_total)

  # Compute the CI
  lower_confidence1 <- p_hat - margin1
  upper_confidence1 <- p_hat + margin1

  data_prop_complete_ci <- cbind(data_prop_complete,
    lower_confidence_percent = lower_confidence1,
    upper_confidence_percent = upper_confidence1
  )

  data_prop_complete_ci <- subset(
    data_prop_complete_ci,
    select = c(
      groupvars,
      "n",
      "percent",
      "lower_confidence_percent",
      "upper_confidence_percent"
    )
  )

  return(data_prop_complete_ci)
}

################################################################################
#' @title get_protected_values should remove certain cell contents
#'
#' @description get_protected_values should remove cell contents of weighted
#' proportion tables or mean value table
#' delete if a minimum population is not reached.
#'
#' @param dataset data.frame from calculate_categorical_statistics or get_mean_table
#' @param cell_size maximum allowed cell size (e.g. 30)
#'
#' @return protected.data (dataset with n, mean/percent, median, n, confidence
#' intervals only with cells >= cell_size)
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords get_protected_values
#'
#'
get_protected_values <- function(dataset, cell_size) {
  if (("mean" %in% colnames(dataset)) == TRUE) {
    save.data <- as.data.frame(apply(
      dataset[c(
        "mean",
        "median",
        "n",
        "percentile_10",
        "percentile_25",
        "percentile_75",
        "percentile_90",
        "percentile_99",
        "lower_confidence_mean",
        "upper_confidence_mean",
        "minimum",
        "maximum",
        "lower_confidence_median",
        "upper_confidence_median"
      )], 2,
      function(x) {
        ifelse(dataset["n"] < cell_size, NA, x)
      }
    ))
    data <- dataset
    data[c(
      "mean",
      "median",
      "n",
      "percentile_10",
      "percentile_25",
      "percentile_75",
      "percentile_90",
      "percentile_99",
      "lower_confidence_mean",
      "upper_confidence_mean",
      "minimum",
      "maximum",
      "lower_confidence_median",
      "upper_confidence_median"
    )] <- NULL
  }

  if (("percent" %in% colnames(dataset)) == TRUE) {
    save.data <- as.data.frame(apply(
      dataset[c(
        "percent",
        "lower_confidence_percent", "upper_confidence_percent"
      )], 2,
      function(x) {
        ifelse(dataset["n"] < cell_size, NA, x)
      }
    ))
    data <- dataset
    data[c(
      "percent",
      "lower_confidence_percent", "upper_confidence_percent"
    )] <- NULL
  }
  protected.data <- cbind(data, save.data)
  return(protected.data)
}

################################################################################
#' @title create_table_lables data set with vauluelabel
#'
#' @description create_table_lables is to provide specific variables of a
#' dataset with vauluelabel
#'
#' @param table data.frame from get_mean_data
#' @param grouping_variables Vector with differentiation variables
#' (e.g. c("age_gr", "sex", "education level")) (maximum 3 variables)
#' ("" possible)
#'
#' @return data_with_label = data set with value labels
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords data_with_label
#'
#' @examples
#' create_table_lables(table = data)
#'
create_table_lables <- function(table, grouping_variables) {
  if (all(nzchar(grouping_variables))) {
    for (groupingvar in grouping_variables) {
      variable_categories_subset <-
        subset(metadaten_variable_categories, variable %in% groupingvar)

      valuelabel_list <- split(
        variable_categories_subset$label_de,
        variable_categories_subset$value
      )

      table[, groupingvar] <- gsubfn::gsubfn(
        ".", valuelabel_list,
        as.character(table[, groupingvar])
      )
    }
  }
  return(table)
}

################################################################################
#' @title table_create Export of mean value or proportion value tables
#'
#' @description table_create export created mean or proportion tables as csv
#'
#' @param table produced data.frame from get_protected_values
#' (e.g. platform_data)
#' @param variable name analysis variable from raw data as string ("pglabnet")
#' @param table_type Type of table to be processed ("mean" or "prop")
#'
#' @return data_csv = exportierte Tabelle als csv
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords data_csv
#'
#'
table_create <-
  function(table,
           variable,
           table_type) {
    if (table_type == "numeric") {
      path <- file.path(export_path, "numerical", variable, "/")
    }

    if (table_type == "categorical") {
      path <- file.path(export_path, "categorical", variable, "/")
    }

    filename_elements <- c(variable, "year", grouping_variables)
    filename_elements <- filename_elements[filename_elements != ""]

    filename <- paste0(
      filename_elements,
      collapse = "_"
    )

    dir.create(path, showWarnings = FALSE)
    data_csv <- sapply(table, as.character)
    data_csv[is.na(data_csv)] <- ""
    data_csv <- as.data.frame(data_csv)

    data_csv <- as.data.frame(apply(
      data_csv, 2,
      function(x) {
        gsub("^\\[[0-9]*]", "", x)
      }
    ))

    data_csv <- as.data.frame(apply(
      data_csv, 2,
      function(x) {
        gsub("^\\s+", "", x)
      }
    ))

    if (table_type == "numeric") {
      export <- paste0(path, filename, ".csv")
    }

    if (table_type == "categorical") {
      export <- paste0(path, filename, ".csv")
      colnames(data_csv)[1] <- variable
    }

    write.csv(
      data_csv,
      export,
      row.names = FALSE,
      quote = TRUE,
      fileEncoding = "UTF-8"
    )
    # return weglassen
    return(data_csv)
  }

################################################################################
################################################################################
# Creation of blank lines when information is empty in a year

#' @title expand_table
#'
#' @description expand_table add empty rows if variable in year is empty.
#'
#' @param table data.frame to be filled with empty cells
#' @param table_type Table type ("prop" or "mean")
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#'
#'
expand_table <-
  function(table,
           table_type) {

    start_year <- max(table$year)
    end_year <- min(table$year)
    
    columns <- c("year", grouping_variables)
    columns <- c(columns, rep("", 5))
    
    variable_categories_subset <-
      subset(metadaten_variable_categories, variable %in% grouping_variables)
    
    value_label_grouping1 <- variable_categories_subset$label_de[which(
      variable_categories_subset$variable == columns[[2]]
    )]
    
    value_label_grouping2 <- variable_categories_subset$label_de[which(
      variable_categories_subset$variable == columns[[3]]
    )]
    
    if (identical(value_label_grouping1, character(0))) {
      value_label_grouping1 <- ""
    }
    if (identical(value_label_grouping2, character(0))) {
      value_label_grouping2 <- ""
    }
    
    if (table_type == "numeric") {
      expand.table <- expand.grid(
        year = seq(start_year, end_year),
        grouping_variable_one = value_label_grouping1,
        grouping_variable_two = value_label_grouping2
      )
      columns <- c("year", grouping_variables)
    }
    
    if (table_type == "categorical") {
      expand.table <- expand.grid(
        usedvariable = unique(dplyr::pull(table, usedvariable)),
        year = seq(start_year, end_year),
        grouping_variable_one = value_label_grouping1,
        grouping_variable_two = value_label_grouping2
      )
      columns <-c("usedvariable", "year", grouping_variables)
    } 
    
    columns <- columns[columns != ""]
    names(expand.table) <- columns
    expand.table <- expand.table %>% 
      purrr::discard(~all(is.na(.) | . ==""))
    final <- merge(table, expand.table, all.y = TRUE)
    final <- final[with(final, order(year)), ] 
    return(final)
  }

################################################################################
# creation of json metadata

#' @title json_create
#'
#' @description json_create creates json metadata
#'
#' @param variable variable names as character
#' @param variable_label variable label as character
#' @param start_year start year of the information as numeric
#' @param end_year end of year information as numeric
#' @param table_type table type (e.g. "mean", "prop", "both")
#' @param export_path path where json file will be stored
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords
#'
#' @examples
#' #
#'
json_create <-
  function(table,
           variable,
           table_type) {
    all_grouping_variables <- metadaten_variables$variable[
      metadaten_variables$meantable == "demo" &
        metadaten_variables$variable != "syear"
    ]

    if (table_type == "numeric") {
      statistics <- c(
        "mean", "lower_confidence_mean", "upper_confidence_mean", 
        "minimum", "maximum",
        "median", "lower_confidence_median", "lower_confidence_median",
        "percentile_10", "percentile_25", "percentile_75", 
        "percentile_90", "percentile_99"
      )
      level <- "numerical"
      exportfile <- paste0(export_path, "/numerical/", variable, "/meta.json")
    }

    if (table_type == "categorical") {
      statistics <- c("percent", "lower_confidence_percent", 
                      "upper_confidence_percent")
      level <- "categorical"
      exportfile <- paste0(export_path, "/categorical/", variable, "/meta.json")
    }

    grouping_information <- list(NULL)
    i <- 0
    for (groupingvar in all_grouping_variables) {
      i <- i + 1
      grouping_information[[i]] <-
        list(
          "variable" = metadaten_variables$variable[
            metadaten_variables$variable == groupingvar
          ],
          "label" = metadaten_variables$label_de[
            metadaten_variables$variable == groupingvar
          ],
          "values" = list(as.list(stringr::str_trim(
            gsub("[[0-9]+]", "", levels(
              factor(datafile_with_labels[[groupingvar]])
            ))
          )))
        )
    }
    json_output <- jsonlite::toJSON(
      x = list(
        "title" = metadaten_variables$label_de[
          metadaten_variables$variable == variable
        ],
        "variable" = metadaten_variables$variable[
          metadaten_variables$variable == variable
        ],
        "statistics" = statistics,
        "dimensions" = grouping_information,
        "description_de" = metadaten_variables$description_de[
          metadaten_variables$variable == variable
        ],
        "start_year" = min(table$year),
        "end_year" = max(table$year),
        "types" = level
      ),
      encoding = "UTF-8",
      pretty = TRUE,
      auto_unbox = TRUE
    )
    file_handler <- file("meta.json")
    writeLines(json_output, exportfile, useBytes = TRUE)
    close(file_handler)
  }

################################################################################
get_grouping_variables_list <- function(dimension_variables) {
  # TODO: Hard to read. Should be encapsulated by a function and the
  # function chaining
  # TODO: should be broken up into seperate statements:
  # TODO: sort(names(dimension_variables)) is duplicated here.

  dimension_variables_sorted <-
    sort(names(dimension_variables))

  single_grouping_combinations <-
    combn(dimension_variables_sorted,
      1,
      simplify = FALSE,
      FUN = sort
    )

  double_grouping_combinations <-
    combn(dimension_variables_sorted, 2,
      simplify = FALSE
    )

  grouping_variables_list <- c(
    "",
    single_grouping_combinations,
    double_grouping_combinations
  )

  return(grouping_variables_list)
}

################################################################################
get_grouping_count_list <- function(dimension_variables) {
  # TODO: names(dimension_variables) seems to be used quite frequently.
  # TODO: Could be better to store it in an extra variable.
  # TODO: This might belong to the 'function' above.
  # TODO: Purpose of renaming and changes are not clear.

  # number of single groupings
  single_length <- length(dimension_variables)
  # number of double groupings
  double_length <- length(combn(names(dimension_variables), 2,
    simplify = FALSE
  ))
  # empty list
  grouping_count_list <- list()

  # First list element is 0
  grouping_count_list[[1]] <- 0

  # Single grouping combinatons always count = 1
  single_grouping_combinations <- rep(list(1), single_length)

  # Double grouping combinatons always count = 2
  double_grouping_combinations <- rep(list(2), double_length)

  # Append single_grouping_combinations
  grouping_count_list <- append(grouping_count_list,
    single_grouping_combinations,
    after = 1
  )

  # Append double grouping combinations
  grouping_count_list <- append(grouping_count_list,
    double_grouping_combinations,
    after = length(grouping_count_list)
  )

  return(grouping_count_list)
}

################################################################################
#' @title get_numeric_statistics
#'
#' @description global function to create aggregated data tables for numeric 
#' variables
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}

get_numeric_statistics <- function() {
  
  data <- get_data(
    variable = variable,
    grouping_variables = grouping_variables,
    value_label = FALSE
  )
  
  table_numeric <- calculate_numeric_statistics(
    dataset = data,
    grouping_variables =
      grouping_variables
  )
  
  table_numeric <-
    create_table_lables(
      table = table_numeric,
      grouping_variables = grouping_variables
    )
  
  protected_table <- get_protected_values(
    dataset = table_numeric,
    cell_size = cell_minimum
  )
  
  protected_table <- expand_table(
    table = protected_table,
    table_type = "numeric"
  )
  
  table_create(
    table = protected_table,
    variable = variable,
    table_type = "numeric"
  )
  
  json_create(
    table = protected_table,
    variable = variable,
    table_type = "numeric"
  )
}

################################################################################
#' @title get_categorical_statistics
#'
#' @description global function to create aggregated data tables for categorical 
#' variables
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords
#'
#'     

get_categorical_statistics <- function() {

data <- get_data(
  variable = variable,
  grouping_variables = grouping_variables,
  value_label = TRUE
)

if ("" %in% grouping_variables) {
  columns <- c("usedvariable", "year")
} else {
  columns <- c("usedvariable", "year", grouping_variables)
}

prop.data <-
  calculate_categorical_statistics(
    dataset = data,
    groupvars = columns,
    alpha = 0.05
  )

protected_table <-
  get_protected_values(dataset = prop.data, 
                       cell_size = cell_minimum)

protected_table <- expand_table(
  table = protected_table,
  table_type = "categorical"
)

data_csv <- table_create(
  table = protected_table,
  variable = variable,
  table_type = "categorical"
)

json_create(
  table = protected_table,
  variable = variable,
  table_type = "categorical"
)
}
