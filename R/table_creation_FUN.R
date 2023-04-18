################################################################################
# Functions #
################################################################################

`%>%` <- dplyr::`%>%`
#' @title subset_data creates subset of a data set
#'
#' @description subset_data to get the output dataset in long format
#' limited to certain variables (variable, year, weight, grouping_variables) and
#' contain only valid values.
#'
#' @param variable name analysis variable as string (e.g. "pglabnet" )
#' @param grouping_variables Vector with differentiation variables
#' (e.g. c("age_gr", "sex", "education level")) (maximum 3 variables)
#' @param use_value_labels Valuelabel should be used (e.g.: use_value_labels = TRUE)
#' (TRUE/FALSE)
#'
#' @return variable.values.valid is a data set with valid values of the
#' variables (variable, year, weight, grouping_variables)
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords subset_data
#'
# TODO: Too many arguments. Arguments are badly named. Possibly too many if
# statements.
#'
subset_data <-
  function(variable,
           grouping_variables,
           use_value_labels) {
    
    columns <- c(variable, "year", "weight",
      grouping_variables
    )

    columns <- columns[columns != ""]
    renamed_columns <- columns[columns != variable]

    if (use_value_labels == FALSE) {
      variable.values <- subset(datafile_without_labels,
        select = columns
      )
      names(variable.values) <-
        c("usedvariablenum",
          renamed_columns
        )
    }
    
    if (use_value_labels == TRUE) {
      variable.values <-
        subset(datafile_without_labels, select = variable)
      factorvar <-
        subset(datafile_with_labels,
          select = columns
        )
      variable.values <- cbind(variable.values, factorvar)
      names(variable.values) <-
        c("usedvariablenum",
          "usedvariable",
          renamed_columns
        )
    }
    variable.values.valid <-
      subset(variable.values, usedvariablenum >= 0)
    
    if (use_value_labels == TRUE) {  
      variable.values.valid$usedvariablenum <- NULL
    }
    if (use_value_labels == FALSE) {  
      variable.values.valid <- dplyr::rename(variable.values.valid, 
                                             usedvariable = usedvariablenum)
    }
    
    variable.values.valid <- variable.values.valid[
      complete.cases(variable.values.valid), ]
    return(variable.values.valid)
  }

################################################################################
################################################################################
# Reihenfolge der columns  noch irgendwie wichtig???

# Gruppierung des Datensatzes am Anfang und nicht in jeder funktion einzeln

#' @title calculate_numeric_statistics
#'
#' @description Main funtction calculate_numeric_statistics creates aggregated 
#' tables for numeric variables with weighted median, weighted mean, n, 
#' minimum, maximum, percentiles, confidence intervals by groups
#'
#' @param dataset data.frame from subset_data function
#' @param grouping_variables Vector with dimension or grouping variables
#' (e.g. c("age_gr", "sex", "education level")) (maximum 3 variables)
#' ("" possible)
#'
#' @return datatable_numeric = dataset with mean, median, n, percentiles, 
#' confidence interval
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords calculate_numeric_statistics
#'
#'
calculate_numeric_statistics <- function(dataset,
                                         grouping_variables) {
  
  columns <- c("year", grouping_variables)
  columns <- columns[columns != ""]
  
  # Calculate weighted mean
  dataset_mean <- calculate_weighted_mean(dataset = dataset,
                                          grouping_variables = columns)
  
  # Calculate weighted median
  dataset_median <- calculate_weighted_median(dataset = dataset, 
                                              grouping_variables = columns)
  # Calculate number of observations n
  dataset_n <- calculate_n(dataset = dataset, 
                           grouping_variables = columns)
  
  # Calculate sd of mean
  dataset_sd <- calculate_sd(dataset = dataset_n, 
                             grouping_variables = columns)
  
  # Calculate minimum and maximum
  dataset_min_max <- calculate_min_max(dataset = dataset, 
                                       grouping_variables = columns)
  
  # Calculate confideence interval mean with weighted mean n and sd
  dataset_confidence_interval_mean <- calculate_confidence_interval_mean(
    dataset_n = dataset_n, dataset_sd = dataset_sd, dataset_mean = dataset_mean)
  
  # Calculate percentiles 
  dataset_percentile_values <- 
    calculate_percentiles(dataset = dataset, 
                          grouping_variables = columns)
  
  # Calculate confidence interval median
  dataset_confidence_interval_median <- calculate_confidence_interval_median(
    dataset = dataset, 
    grouping_variables = columns)
  
  datatable_numeric <- 
    combine_numeric_statistics(
      grouping_variables = columns, 
      dataset_confidence_interval_mean = dataset_confidence_interval_mean,
      dataset_median = dataset_median, 
      dataset_min_max = dataset_min_max, 
      dataset_percentile_values = dataset_percentile_values, 
      dataset_confidence_interval_median = dataset_confidence_interval_median)
  
  return(datatable_numeric)
}

################################################################################
################################################################################
#' @title calculate_weighted_mean
#'
#' @description calculate_weighted_mean calculates weighted mean by groups
#'
#' @param dataset data.frame from subset_data function
#' @param grouping_variables Vector with dimension or grouping variables
#' (e.g. c("age_gr", "sex", "education level")) (maximum 3 variables)
#' ("" possible)
#'
#' @return dataset_mean = dataset with weighted mean by group
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords calculate_numeric_statistics
#'
#'
calculate_weighted_mean <- function(dataset, grouping_variables) {
  
  dataset <- dataset[complete.cases(dataset), ]
  dataset_grouped <- dplyr::group_by_at(dataset, 
                                        dplyr::vars(one_of(grouping_variables)))
  dataset_mean <- dplyr::mutate(dataset_grouped, 
                                mean = round(stats::weighted.mean(usedvariable, 
                                                                  weight), 2))
  return(dataset_mean)
}

################################################################################
################################################################################
#' @title calculate_weighted_median
#'
#' @description calculate_weighted_median calculates weighted median by groups
#'
#' @param dataset data.frame from subset_data function
#' @param grouping_variables Vector with dimension or grouping variables
#' (e.g. c("age_gr", "sex", "education level")) (maximum 3 variables)
#' ("" possible)
#'
#' @return dataset_median = dataset with weighted median by group
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords calculate_numeric_statistics
#'
#'
#'
calculate_weighted_median <- function(dataset, grouping_variables) {
  dataset <- dataset[complete.cases(dataset), ]
  dataset_grouped <- dplyr::group_by_at(dataset, 
                                        dplyr::vars(one_of(grouping_variables)))
  dataset_median <- dplyr::mutate(dataset_grouped, 
                                  median = DescTools::Median(usedvariable, 
                                                             weights = weight))
  return(dataset_median)
}

################################################################################
################################################################################
#' @title calculate_n
#'
#' @description calculate_n calculates observations by groups
#'
#' @param dataset data.frame from subset_data function
#' @param grouping_variables Vector with dimension or grouping variables
#' (e.g. c("age_gr", "sex", "education level")) (maximum 3 variables)
#' ("" possible)
#'
#' @return dataset_n = dataset with observations by group
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords calculate_numeric_statistics
#'
#'
#'
calculate_n <- function(dataset, grouping_variables) {
  dataset <- dataset[complete.cases(dataset), ]
  dataset_grouped <- dplyr::group_by_at(dataset, 
                                        dplyr::vars(one_of(grouping_variables)))
  dataset_n <- dplyr::add_count(dataset_grouped, year, 
   wt = NULL)
  return(dataset_n)
}

################################################################################
################################################################################
# Weglassen weil sd nicht in der finalen Tabelle ist und sp?ter auch erzeugt wird

#' @title calculate_sd
#'
#' @description calculate_sd calculates sd of mean by groups
#'
#' @param dataset data.frame from subset_data function
#' @param grouping_variables Vector with dimension or grouping variables
#' (e.g. c("age_gr", "sex", "education level")) (maximum 3 variables)
#' ("" possible)
#'
#' @return dataset_sd = dataset with sd of mean by group
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords calculate_numeric_statistics
#'
#'
#'
calculate_sd <- function(dataset, grouping_variables) {
  dataset_grouped <- dplyr::group_by_at(dataset, 
                                        dplyr::vars(one_of(grouping_variables)))
  dataset_sd <- dplyr::mutate(dataset_grouped, sd = sd(usedvariable / sqrt(n)))
  return(dataset_sd)
}

################################################################################
################################################################################
#' @title dataset_min_max
#'
#' @description dataset_min_max calculates minimum and maximum by groups
#'
#' @param dataset data.frame from subset_data function
#' @param grouping_variables Vector with dimension or grouping variables
#' (e.g. c("age_gr", "sex", "education level")) (maximum 3 variables)
#' ("" possible)
#'
#' @return dataset_min_max = dataset with minimum and maximum by group
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords calculate_numeric_statistics
#'
#'
#'
calculate_min_max <- function(dataset, grouping_variables) {
  
  dataset <- dataset[complete.cases(dataset), ]
  dataset_grouped <- dplyr::group_by_at(dataset, 
                                        dplyr::vars(one_of(grouping_variables)))
  dataset_min_max <- dplyr::mutate(dataset_grouped, 
                                   maximum = round(max(usedvariable, 
                                                       na.rm = T), 2),
                                   minimum = round(min(usedvariable, 
                                                       na.rm = T), 2))
  return(dataset_min_max)
}

################################################################################
################################################################################
#' @title calculate_confidence_interval_mean
#'
#' @description calculate_confidence_interval_mean calculates mean confidence 
#' intervals by groups
#' 
#' @param dataset_n dataframe with n
#' @param dataset_sd dataframe dataset_sd
#' @param dataset_mean dataframe dataset_mean
#'
#' @return dataset_confidence_interval_mean = dataset mean confidence intervals 
#' by group
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords calculate_numeric_statistics
#'
#'
#'
calculate_confidence_interval_mean <- function(dataset_n, dataset_sd, dataset_mean) {
  
  dataset_confidence_interval_mean <- cbind(dataset_n, 
                                            dataset_sd["sd"], 
                                            dataset_mean["mean"])
  
  dataset_confidence_interval_mean <- dplyr::mutate(
    dataset_confidence_interval_mean, 
    sd = sd(usedvariable / sqrt(n)))
  dataset_confidence_interval_mean <- dplyr::mutate(
    dataset_confidence_interval_mean,
    lower = mean - qt(1 - (alpha / 2), 
    as.numeric(n) - 1) * sd,
    upper = mean + qt(1 - (alpha / 2), 
    as.numeric(n) - 1) * sd
  )   
  
  dataset_confidence_interval_mean <- dplyr::mutate(
    dataset_confidence_interval_mean, 
    lower_confidence_mean = round((lower), 2))
  dataset_confidence_interval_mean <- dplyr::mutate(
    dataset_confidence_interval_mean,
    upper_confidence_mean = round((upper), 2))
  
  return(dataset_confidence_interval_mean)
}

################################################################################
################################################################################
# Percentile Berechnung als Einzelfunktion

#' @title calculate_percentiles
#'
#' @description calculate_percentiles calculates percentiles by groups
#'
#' @param dataset data.frame from subset_data function
#' @param grouping_variables Vector with dimension or grouping variables
#' (e.g. c("age_gr", "sex", "education level")) (maximum 3 variables)
#' ("" possible)
#'
#' @return dataset_percentile_values = dataset with percentiles by group
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords calculate_numeric_statistics
#'
#'
#'
calculate_percentiles <- function(dataset, grouping_variables) {
  dataset <- dataset[complete.cases(dataset), ]
  dataset_grouped <- dplyr::group_by_at(dataset, 
                                        dplyr::vars(one_of(grouping_variables)))
  
  dataset_percentile_values <- 
    dplyr::summarise(dataset_grouped,
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
  return(dataset_percentile_values)
}

################################################################################
# subset_data_groups
subset_data_groups <- function(dataset, grouping_variables, groupindex){

  data_grouped <- dplyr::group_by_at(dataset, 
                                     dplyr::vars(one_of(grouping_variables)))
  data_grouped <- dplyr::mutate(data_grouped, group_id = 
                                  dplyr::cur_group_id())
  data_grouped <- dplyr::filter(data_grouped, group_id == 
                                  groupindex)
  
  return(data_grouped)
}

################################################################################
# get_group_index
get_group_index <- function(dataset, grouping_variables){

  dataset <- dplyr::group_by_at(dataset, 
                                dplyr::vars(one_of(grouping_variables)))
  
  dataset <- dplyr::mutate(dataset, group_id = dplyr::cur_group_id())
  
  return(dataset)
}

################################################################################
# bootstrap_median
bootstrap_median <- function(x, weights, R = 1000, na_raus = TRUE){
  median <- DescTools::Median(x, weights = weights, na.rm=na_raus)
  
  bootstrap <- function(iter){
    x <- sort(x)
    index <- sample(1:length(x), length(x), replace = TRUE)
    index <- sort(index)
    xx <- x[index]
    ww <- weights[index]
    
    sort_data <- data.frame("usedvariable" = xx, 
                            "weight" = ww)
    sort_data[order(sort_data$usedvariable),]
    
    xx <- sort_data$usedvariable
    ww <- sort_data$weight
    
    boot_median <- DescTools::Median(xx, weights = ww, na.rm = na_raus)
    return(boot_median)
  }
  
  result <- sapply(1:R, bootstrap)
  quant <- quantile(result, probs = c(0.025, 0.975), na.rm = na_raus)
  lower_confidence_median <- 2 * median - quant[2]
  upper_confidence_median <- 2 * median - quant[1]
  out <-  c(median, lower_confidence_median, upper_confidence_median)
  names(out) <- c("median", "lower_confidence_median", 
                  "upper_confidence_median")
  return(out)
  
}

################################################################################
#' @title calculate_confidence_interval_median
#'
#' @description calculate_confidence_interval_median calculates median confidence
#' intervals by groups
#'
#' @param dataset data.frame from subset_data function
#' @param grouping_variables Vector with dimension or grouping variables
#' (e.g. c("age_gr", "sex", "education level")) (maximum 3 variables)
#' ("" possible)
#'
#' @return dataset_percentile_values = dataset with median confidence intervals
#' by group
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords calculate_numeric_statistics
#'
#'
#' Sortiert ?bergeben und morgen am DIW Zeiten testen

calculate_confidence_interval_median <- function(dataset, grouping_variables){
  
  dataset <- get_group_index(dataset = dataset,
                             grouping_variables = grouping_variables)
  
  confidence_interval_median <- list()
  dataset <- tibble::as_tibble(dataset)
  
  for (index in 1:max(dataset$group_id)) {
    
    data_grouped <- subset_data_groups(dataset = dataset,
                                       grouping_variables = grouping_variables,
                                       groupindex = index)
    
    confidence_interval_median[[index]] <- 
      bootstrap_median(x = data_grouped$usedvariable,
                       weights = data_grouped$weight,
                       R = 1000,
                       na_raus = TRUE)
    
  }
  
  dataset_confidence_interval_median <- as.data.frame(
    do.call(rbind, confidence_interval_median))
  
  dataset_confidence_interval_median <- dplyr::mutate(
    dataset_confidence_interval_median, 
    group_id = dplyr::row_number())
  
  dataset_confidence_interval_median <- 
    merge(dataset, dataset_confidence_interval_median, 
          by = "group_id")
  
  dataset_confidence_interval_median <- 
    dataset_confidence_interval_median[c(grouping_variables, "median", 
                                         "lower_confidence_median", 
                                         "upper_confidence_median")]
  
  dataset_confidence_interval_median <- 
    dplyr::group_by_at(dataset_confidence_interval_median, grouping_variables)
  dataset_confidence_interval_median <- 
    dplyr::distinct(dataset_confidence_interval_median, .keep_all = TRUE)
  dataset_confidence_interval_median <- 
    as.data.frame(dataset_confidence_interval_median)
  
  return(dataset_confidence_interval_median)
}  

################################################################################
################################################################################
#' @title combine_numeric_statistics
#'
#' @description combine_numeric_statistics combines different datasets with 
#' statistical parameters by groups
#' 
#' @param grouping_variables Vector with dimension or grouping variables
#' (e.g. c("age_gr", "sex", "education level")) (maximum 3 variables)
#' ("" possible)
#' @param dataset_confidence_interval_mean 
#' @param dataset_median 
#' @param dataset_min_max 
#' @param dataset_percentile_values 
#' @param dataset_median 
#' @param dataset_confidence_interval_median 
#' 
#' @return datatable_numeric = dataset all statistical parameters 
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords calculate_numeric_statistics
#'
#'
#'
combine_numeric_statistics <- function(grouping_variables, 
                                       dataset_confidence_interval_mean,
                                       dataset_median, dataset_min_max, 
                                       dataset_percentile_values, 
                                       dataset_confidence_interval_median) {

  datatable_numeric <- cbind(dataset_confidence_interval_mean, 
                             dataset_median["median"],
                             dataset_min_max[c("minimum", "maximum")])
  
  datatable_numeric <- dplyr::distinct(datatable_numeric, 
                                       mean, .keep_all = TRUE)
  
  datatable_numeric <- merge(datatable_numeric, dataset_percentile_values, 
                             by = grouping_variables)
  
  datatable_numeric <- merge(datatable_numeric, 
                             dataset_confidence_interval_median, 
                             by = grouping_variables)
  
  numeric_statistics_column_names <- c(
    grouping_variables,
    numeric_statistics_column_names
  )
  
  datatable_numeric <- datatable_numeric[, (names(datatable_numeric) 
                                            %in% numeric_statistics_column_names)]
  datatable_numeric <- dplyr::arrange(datatable_numeric, desc(year),
                                      .by_group = TRUE)
  
  return(datatable_numeric)
}  

################################################################################
################################################################################
#' @title function calculate_categorical_statistics shall create weighted 
#' proportion tables with confidence intervals
#'
#' @description calculate_categorical_statistics shall create weighted 
#' proportion value tables with confidence intervals
#' create with the information n = size of the subgroup, percent = weighted
#' proportion value, lower_confidence = lower confidence interval,
#' upper_confidence = upper 95% confidence interval
#'
#' @param dataset data.frame from subset_data (e.g. platform_data)
#' @param grouping_variables vector with all variables in the dataset
#' (e.g. c("usedvariable", "year", "sex"))
#' @param alpha Alpha for setting the confidence interval (e.g. 0.05)
#'
#' @return data_prop_complete_ci = data set with n, percent, lower_confidence,
#' upper_confidence
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords calculate_categorical_statistics
#'

calculate_categorical_statistics <- function(dataset, 
                                             grouping_variables, alpha) {
  
  
  dataset_weighted_n <- calculate_categorical_Weighted_n(
    dataset = dataset, 
    grouping_variables = grouping_variables)
  
  dataset_total_Weight <- calculate_categorical_total_Weight(
    dataset = dataset_weighted_n, 
    grouping_variables = grouping_variables)
  
  dataset_unweighted_n <- calculate_categorical_unweighted_n(
    dataset = dataset, 
    grouping_variables = grouping_variables)
  
  dataset_total_n <- calculate_categorical_total_n(
    dataset = dataset_unweighted_n, 
    grouping_variables = grouping_variables)
  
  data_prop <- cbind(dataset_total_Weight, 
                     dataset_total_n[c("n", "n_total")])
  data_prop <- data_prop[order(data_prop$year), ]
  
  data_prop_complete_ci <- calculate_confidence_interval_percent(
    dataset = data_prop, 
    grouping_variables = grouping_variables,
    alpha = alpha)
  
  return(data_prop_complete_ci)
}

################################################################################
################################################################################
#' @title calculate_categorical_Weighted_n 
#'
#' @description calculate_categorical_statistics sums all weights for the
#' grouping variables ("count_w")
#' 
#' @param dataset data.frame from subset_data (e.g. platform_data)
#' @param grouping_variables vector with all variables in the dataset
#' (e.g. c("usedvariable", "year", "sex"))
#'
#' @return dataset_n = data frame with count_w
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords calculate_categorical_statistics
#'

calculate_categorical_Weighted_n <-  function(dataset, grouping_variables) {
  
  dataset_n <- dataset[complete.cases(dataset), ]
  dataset_n <- dplyr::group_by_at(dataset_n, 
                                  dplyr::vars(one_of(grouping_variables)))
  dataset_n <- dplyr::summarise(dataset_n, count_w = sum(weight), 
                                .groups = "drop_last")
  
  return(dataset_n)
}

################################################################################
################################################################################
#' @title calculate_categorical_total_Weight 
#'
#' @description calculate_categorical_total_Weight sums total weights for the
#' grouping variables ("sum_count_w")
#' 
#' @param dataset data.frame from subset_data (e.g. platform_data)
#' @param grouping_variables vector with all variables in the dataset
#' (e.g. c("usedvariable", "year", "sex"))
#'
#' @return dataset_n = data frame with sum_count_w
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords calculate_categorical_statistics
#'

calculate_categorical_total_Weight <-  function(dataset, 
                                                grouping_variables) {
  
  dataset_total_Weight <- dplyr::group_by(dataset,
                                          eval(parse(text = grouping_variables[2])),
                                          eval(parse(text = grouping_variables[3])),
                                          eval(parse(text = grouping_variables[4])))
  
  dataset_total_Weight <- dplyr::mutate(dataset_total_Weight, 
                                        sum_count_w = sum(count_w))
  
  dataset_total_Weight <- dataset_total_Weight[
    , (names(dataset_total_Weight) 
       %in% c("usedvariable", grouping_variables, "count_w", "sum_count_w"))]
  
  return(dataset_total_Weight)
}

################################################################################
################################################################################
#' @title calculate_categorical_unweighted_n 
#'
#' @description calculate_categorical_total_Weight sums unweighted observations 
#' for the grouping variables ("n")
#' 
#' @param dataset data.frame from subset_data (e.g. platform_data)
#' @param grouping_variables vector with all variables in the dataset
#' (e.g. c("usedvariable", "year", "sex"))
#'
#' @return dataset_n = data frame with n
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords calculate_categorical_statistics
#'

calculate_categorical_unweighted_n <-  function(dataset, grouping_variables) {
  
  dataset_n <- dataset[complete.cases(dataset), ]
  dataset_n <- dplyr::group_by_at(dataset_n, 
                                  dplyr::vars(one_of(grouping_variables)))
  dataset_n <- dplyr::summarise(dataset_n, 
                                n = dplyr::n(), .groups = "drop_last")
  
  return(dataset_n)
}

################################################################################
################################################################################
#' @title calculate_categorical_total_n 
#'
#' @description calculate_categorical_total_n sums total unweighted observations 
#' for the grouping variables ("n_total")
#' 
#' @param dataset data.frame from subset_data (e.g. platform_data)
#' @param grouping_variables vector with all variables in the dataset
#' (e.g. c("usedvariable", "year", "sex"))
#'
#' @return dataset_n = data frame with n_total
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords calculate_categorical_statistics
#'

calculate_categorical_total_n <-  function(dataset, 
                                           grouping_variables) {
  
  dataset_total_n <- dplyr::group_by(dataset,
                                     eval(parse(text = grouping_variables[2])),
                                     eval(parse(text = grouping_variables[3])),
                                     eval(parse(text = grouping_variables[4])))
  
  dataset_total_n <- dplyr::mutate(dataset_total_n, 
                                   n_total = sum(n))
  
  dataset_total_n <- dataset_total_n[
    , (names(dataset_total_n) 
       %in% c("usedvariable", grouping_variables, "n", "n_total"))]
  
  return(dataset_total_n)
}

################################################################################
#' @title calculate_confidence_interval_percent
#'
#' @description function calculate_confidence_interval_percent calculates confidence 
#' interval for group percent values
#'
#' @param dataset data.frame from subset_data (e.g. platform_data)
#' @param grouping_variables vector with all variables in the dataset
#' (e.g. c("usedvariable", "year", "sex"))
#' @param alpha Alpha for setting the confidence interval (e.g. 0.05)
#'
#' @return data_prop_complete_ci = data set with n, percent, lower_confidence,
#' upper_confidence
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords calculate_categorical_statistics
#'

calculate_confidence_interval_percent <-  function(dataset, grouping_variables,
                                                   alpha) {
  
  data_prop_complete <- dplyr::mutate(dataset, 
                                      percent = count_w / sum_count_w, )
  
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
      grouping_variables,
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
  if ( ! "" %in% grouping_variables) {
    # TODO: Inconsistent variable naming; should be grouping_variable
    for (groupingvar in grouping_variables) {
      # TODO: variable is a global variable set in a different file, that is mutated
      # TODO: frequently. This makes it hard to see what the value of variable in this
      # TODO: context. The location where variable is declared is also
      # TODO: relatively hard to find. There seem to be no reason why `variable`
      # TODO: is global, it should be an argument of this function. 
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

    # TODO: Where does `numeric` come from? `numerical` would be better and would
    # TODO: allow us to eliminate these if statements.     
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
      # TODO: Why is this done here and not already part of table?
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
#' @title print_numeric_statistics
#'
#' @description global function to create aggregated data tables for numeric 
#' variables
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}

print_numeric_statistics <- function() {
  
  data <- subset_data(
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
#' @title print_categorical_statistics
#'
#' @description global function to create aggregated data tables for categorical 
#' variables
#'
#' @author Stefan Zimmermann, \email{szimmermann@diw.de}
#' @keywords
#'
#'     

print_categorical_statistics <- function() {

data <- subset_data(
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
