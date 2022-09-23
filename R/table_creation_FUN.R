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
#' @param datafile_without_labels data.frame with numeric variables
#' (e.g. platform_data)
#' @param datafile_with_labels data.frame with factor variables
#' (e.g. platform_data)
#' @param variable name analysis variable as string (e.g. "pglabnet" )
#' @param year Name survey year variable as string (e.g. "syear" )
#' @param weight Name weight variable as string (e.g. "phrf")
#' @param grouping_count Number of desired differentiations (e.g. 2)
#' (range 0-3)
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
#' @examples
#' get_data(
#'   datafile_without_labels = datafile_without_labels,
#'   datafile_with_labels = datafile_with_labels,
#'   variable = "pglabnet",
#'   year = "syear",
#'   weight = "phrf",
#'   grouping_count = grouping_count,
#'   grouping_variables = c("alter_gr", "sex", "Bildungsniveau"),
#'   value_label = TRUE
#' )
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

#' @title get_mean_values creates mean/median tables with mean, median, n,
#' percentiles, confidence interval
#'
#' @description get_mean_values creates weighted mean/median tables with
#'              n, percentiles, confidence interval
#'
#' @param dataset data.frame from get_data (e.g. platform_data)
#' @param year year variable as string (e.g. "year")
#' @param grouping_count number of desired differentiations (e.g. 2) (range 0-3)
#' @param grouping_variable_one name differentiation variable 1 as string or ""
#' @param grouping_variable_two name differentiation variable 2 as string or ""
#' @param grouping_variable_three Number of differentiation variable 3 as string
#' ("" possible)
#'
#' @return data = dataset with mean, median, n, percentiles, confidence interval
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords get_mean_values
#'
#' @examples
#' get_mean_values(
#'   dataset = data,
#'   year = "year",
#'   grouping_count = 2,
#'   grouping_variable_one = "sex",
#'   grouping_variable_two = "alter_gr",
#'   grouping_variable_three = ""
#' )
# TODO: Some arguments are badly named. grouping_variable_one-3 could be one
# data structure.
# TODO: Too many if statements
#'
get_mean_values <- function(dataset,
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
    dplyr::mutate(lowerci_mean = round((lower), 2)) %>%
    dplyr::mutate(upperci_mean = round((upper), 2)) %>%
    dplyr::mutate(
      max = round(max(usedvariable, na.rm = T), 2),
      min = round(min(usedvariable, na.rm = T), 2)
    ) %>%
    dplyr::distinct(mean, .keep_all = TRUE)


  percentile.values <- dataset[complete.cases(dataset), ] %>%
    dplyr::group_by_at(dplyr::vars(one_of(columns))) %>%
    dplyr::summarise(
      ptile10 = round(
        Hmisc::wtd.quantile(
          usedvariable,
          weights = weight,
          probs = .1,
          na.rm = TRUE
        ),
        2
      ),
      ptile25 = round(
        Hmisc::wtd.quantile(
          usedvariable,
          weights = weight,
          probs = .25,
          na.rm = TRUE
        ),
        2
      ),
      ptile75 = round(
        Hmisc::wtd.quantile(
          usedvariable,
          weights = weight,
          probs = .75,
          na.rm = TRUE
        ),
        2
      ),
      ptile90 = round(
        Hmisc::wtd.quantile(
          usedvariable,
          weights = weight,
          probs = .90,
          na.rm = TRUE
        ),
        2
      ),
      ptile99 = round(
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
    c(columns, "lowerci_median", "upperci_median")

  data <- merge(mean.values, percentile.values, by = columns)

  data <- dplyr::left_join(data, medianci.value, by = columns)

  selected.values <- c(
    columns,
    "mean",
    "lowerci_mean",
    "upperci_mean",
    "median",
    "lowerci_median",
    "upperci_median",
    "ptile10",
    "ptile25",
    "ptile75",
    "ptile90",
    "ptile99",
    "n",
    "min",
    "max"
  )

  data <- data[, (names(data) %in% selected.values)]
  data <- data %>%
    dplyr::arrange(desc(year), .by_group = TRUE)

  return(data)
}

################################################################################
#' @title function get_prop_values shall create weighted proportion tables with
#' confidence intervals
#'
#' @description get_mean_values shall create weighted proportion value tables
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
#' @keywords get_prop_values
#'
#' @examples
#' get_prop_values(
#'   dataset = data,
#'   groupvars = c("usedvariable", "year", "sex"),
#'   alpha = 0.05
#' )
#'
get_prop_values <- function(dataset, groupvars, alpha) {
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
    lower_confidence = lower_confidence1,
    upper_confidence = upper_confidence1
  )

  data_prop_complete_ci <- subset(
    data_prop_complete_ci,
    select = c(
      groupvars,
      "n",
      "percent",
      "lower_confidence",
      "upper_confidence"
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
#' @param dataset data.frame from get_prop_values or get_mean_table
#' @param cell.size maximum allowed cell size (e.g. 30)
#'
#' @return protected.data (dataset with n, mean/percent, median, n, confidence
#' intervals only with cells >= cell.size)
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords get_prop_values
#'
#' @examples
#' get_prop_values(dataset = data, alpha = 0.05)
#'
get_protected_values <- function(dataset, cell.size) {
  if (("mean" %in% colnames(dataset)) == TRUE) {
    save.data <- as.data.frame(apply(
      dataset[c(
        "mean",
        "median",
        "n",
        "ptile10",
        "ptile25",
        "ptile75",
        "ptile90",
        "ptile99",
        "lowerci_mean",
        "upperci_mean",
        "min",
        "max",
        "lowerci_median",
        "upperci_median"
      )], 2,
      function(x) {
        ifelse(dataset["n"] < cell.size, NA, x)
      }
    ))
    data <- dataset
    dataset[c(
      "mean",
      "median",
      "n",
      "ptile10",
      "ptile25",
      "ptile75",
      "ptile90",
      "ptile99",
      "lowerci_mean",
      "upperci_mean",
      "min",
      "max",
      "lowerci_median",
      "upperci_median"
    )] <- NULL
  }

  if (("percent" %in% colnames(dataset)) == TRUE) {
    save.data <- as.data.frame(apply(
      dataset[c(
        "percent",
        "lower_confidence", "upper_confidence"
      )], 2,
      function(x) {
        ifelse(dataset["n"] < cell.size, NA, x)
      }
    ))
    data <- dataset
    dataset[c(
      "percent",
      "lower_confidence", "upper_confidence"
    )] <- NULL
  }
  protected.data <- cbind(dataset, save.data)
  return(protected.data)
}

################################################################################
#' @title create_table_lables data set with vauluelabel
#'
#' @description create_table_lables is to provide specific variables of a
#' dataset with vauluelabel
#'
#' @param table data.frame from get_mean_data
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
#' @title get_table_export Export of mean value or proportion value tables
#'
#' @description get_table_export export created mean or proportion tables as csv
#'
#' @param table produced data.frame from get_protected_values
#' (e.g. platform_data)
#' @param variable name analysis variable from raw data as string ("pglabnet")
#' @param metadata_path Path to the metadata with variable name and table type
#' in the dataset as string
#' @param export_path export folder as string
#' @param grouping_count number of differentiations as numeric (0-3 robbed)
#' @param table_type Type of table to be processed ("mean" or "prop")
#'
#' @return data_csv = exportierte Tabelle als csv
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords data_csv
#'
#' @examples
#' get_table_export(
#'   table = protected_table, variable = "usedvariable",
#'   metadata_path = paste0(metadata_path, "varnames.csv"),
#'   export_path = export_path, grouping_count = 2,
#'   table_type = "mean"
#' )
#'
get_table_export <-
  function(table,
           variable,
           table_type) {
    if (table_type == "mean") {
      path <- file.path(export_path, "numerical", variable, "/")
    }

    if (table_type == "prop") {
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

    if (table_type == "mean") {
      export <- paste0(path, filename, ".csv")
    }

    if (table_type == "prop") {
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
    return(data_csv)
  }

################################################################################
################################################################################
# Creation of blank lines when information is empty in a year

#' @title expand_table
#'
#' @description expand_table add empty rows if variable in year is empty.
#'
#' @table data.frame to be filled with empty cells
#' @grouping_variable_one differentiation dimension as character
#' @grouping_variable_two Differentiation dimension as character
#' @grouping_variable_three differentiation dimension as character
#' @grouping_count number of differentiations as numeric
#' @table_type Table type ("prop" or "mean")
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#'
#' @examples expand_table(
#'   table = protected_table, grouping_variable_one = grouping_variable_one,
#'   grouping_variable_two = grouping_variable_two,
#'   grouping_variable_three = grouping_variable_three,
#'   grouping_count = grouping_count, table_type = "prop"
#' )
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

    if (table_type == "mean") {
      expand.table <- expand.grid(
        year = seq(start_year, end_year),
        grouping_variable_one = value_label_grouping1,
        grouping_variable_two = value_label_grouping2
      )
      columns <- c("year", grouping_variables)
    }

    if (table_type == "prop") {
      expand.table <- expand.grid(
        year = seq(start_year, end_year),
        usedvariable = unique(dplyr::pull(table, usedvariable)),
        grouping_variable_one = value_label_grouping1,
        grouping_variable_two = value_label_grouping2
      )
      columns <-c("year", "usedvariable", grouping_variables)
    }

    columns <- columns[columns != ""]
    names(expand.table) <- columns
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
#' @variable variable names as character
#' @variable_label variable label as character
#' @start_year start year of the information as numeric
#' @end_year end of year information as numeric
#' @table_type table type (e.g. "mean", "prop", "both")
#' @export_path path where json file will be stored
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

    if (table_type == "mean") {
      statistics <- c(
        "mean", "lowerci_mean", "upperci_mean", "min", "max",
        "median", "lowerci_median", "upperci_median",
        "ptile10", "ptile25", "ptile75", "ptile90", "ptile99"
      )
      level <- "numerical"
      exportfile <- paste0(export_path, "/numerical/", variable, "/meta.json")
    }

    if (table_type == "prop") {
      statistics <- c("percent", "lower_confidence", "upper_confidence")
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
get_grouping_variables_list <- function(metadaten_variables_demo) {
  # TODO: Hard to read. Should be encapsulated by a function and the
  # function chaining
  # TODO: should be broken up into seperate statements:
  # TODO: sort(names(metadaten_variables_demo)) is duplicated here.

  metadaten_variables_demo_sorted <-
    sort(names(metadaten_variables_demo))

  single_grouping_combinations <-
    combn(metadaten_variables_demo_sorted,
      1,
      simplify = FALSE,
      FUN = sort
    )

  double_grouping_combinations <-
    combn(metadaten_variables_demo_sorted, 2,
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
get_grouping_count_list <- function(metadaten_variables_demo) {
  # TODO: names(metadaten_variables_demo) seems to be used quite frequently.
  # TODO: Could be better to store it in an extra variable.
  # TODO: This might belong to the 'function' above.
  # TODO: Purpose of renaming and changes are not clear.

  # number of single groupings
  single_length <- length(metadaten_variables_demo)
  # number of double groupings
  double_length <- length(combn(names(metadaten_variables_demo), 2,
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
