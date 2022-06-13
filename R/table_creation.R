#############################################################################
# Create aggregated data tables for SOEP transfer project
#############################################################################
# TODO: Variable names should be more explicit, no abbreviations.
# TODO: Use consistent snake case

### Path definition
if (Sys.info()[["user"]] == "szimmermann") {
  datapath <- "H:/data/"
  metapath <- "H:/Clone/soep-transfer/metadata/"
  exportpath <- "H:/Clone/soep-transfer/"
}

# Definition of objects
dataset <- "h_statistics" # From which data set should values be taken
cell.min <- 30 # Maximum allowed cell size
year <- "syear" # Survey year must be defined
weight <- "hhrf" # Weight must be defined
#############################################################################

## load packages
# TODO: Remove this. Inpropper way to work with dependencies.
# TODO: Dependenciess could be more specific and therefore smaller.
loadpackage(c(
  "foreign", "dplyr", "tidyverse", "readstata13", "spatstat",
  "gsubfn", "rjson"
))

## load data without labels
data.file.num <- read.dta13(paste0(datapath, dataset, ".dta"),
  convert.factors = FALSE, encoding = "UTF-8"
)

# Delete cases with no weighting
# TODO: %>% Should not be used just to call a single function.
# TODO: This is done multiple times in this file.
data.file.num <- data.file.num %>%
  filter(weight > 0)

## load data without labels
data.file.fac <- read.dta13(paste0(datapath, dataset, ".dta"),
  convert.factors = TRUE,
  nonint.factors = TRUE, encoding = "UTF-8"
)

# Weights with 0 cause problems
# TODO: %>% Should not be used just to call a single function.
data.file.fac <- data.file.fac %>%
  filter(weight > 0)

# read metainformation
meta <- read.csv(paste0(metapath, "variables.csv"),
  header = TRUE,
  colClasses = "character"
)

################################################################################
################################################################################
### Code to create the aggregated tables in variables.csv
meta_demo <- meta %>%
  filter(meantable == "demo")

meta_demo <- subset(data.file.num,
  select = meta_demo$variable
)

# Generate a list that represents all the grouping possibilities of the users
# TODO: Hard to read. Should be encapsulated by a function and the function chaining
# TODO: should be broken up into seperate statements:
# TODO: sort(names(meta_demo)) is duplicated here.
difflist <- c(
  "", combn(sort(names(meta_demo)), 1, simplify = FALSE, FUN = sort),
  combn(sort(names(meta_demo)), 2, simplify = FALSE)
)

# Generate a list that represents ne number of differentiations for each possibility
# TODO: names(meta_demo) seems to be used quite frequently.
# TODO: Could be better to store it in an extra variable.
# TODO: This might belong to the 'function' above.
# TODO: Purpose of renaming and changes are not clear.
diffcountlist <- difflist
diffcountlist[[1]] <- 0
diffcountlist[2:(1 + length(combn(names(meta_demo), 1, simplify = FALSE)))] <- 1
diffcountlist[(2 + length(combn(names(meta_demo), 1, simplify = FALSE))):length(diffcountlist)] <- 2

##############################################################################################
# Create aggregated data tables
# TODO: Loop Body is way to long.
# TODO: Too many if statements. Too many if statements whith unclear purpose.
# TODO: Should be split into seperate functions.
for (var in 1:length(meta$variable)) {
  if (meta$meantable[var] == "Yes" | meta$probtable[var] == "Yes") {
    variable <- meta$variable[var]

    for (i in seq_along(difflist)) {
      diffcount <- diffcountlist[[i]]
      diffvars <- difflist[[i]]

      if (!is.na(diffvars[1])) {
        diffvar1 <- diffvars[1]
      } else {
        diffvar1 <- ""
      }

      if (!is.na(diffvars[2])) {
        diffvar2 <- diffvars[2]
      } else {
        diffvar2 <- ""
      }

      if (!is.na(diffvars[3])) {
        diffvar3 <- diffvars[3]
      } else {
        diffvar3 <- ""
      }

      if (meta$meantable[var] == "Yes") {
        data <- get_data(
          datasetnum = data.file.num,
          datasetfac = data.file.fac,
          variable = variable,
          year = year,
          weight = weight,
          diffcount = diffcount,
          diffvars = diffvars,
          vallabel = FALSE
        )


        table.values <- get_mean_values(
          dataset = data,
          year = "year",
          diffcount = diffcount,
          diffvar1 = diffvar1,
          diffvar2 = diffvar2,
          diffvar3 = diffvar3
        )


        table.values <- create_table_lables(table = table.values)

        protected.table <- get_protected_values(dataset = table.values, cell.size = 30)


        protected.table <- expand_table(
          table = protected.table, diffvar1 = diffvar1,
          diffvar2 = diffvar2, diffvar3 = diffvar3,
          diffcount = diffcount, tabletype = "mean"
        )

        data.csv <- get_table_export(
          table = protected.table, variable = variable,
          metadatapath = paste0(metapath, "variables.csv"),
          exportpath = exportpath, diffcount = diffcount,
          tabletype = "mean"
        )

        json_create_lite(
          variable = variable,
          varlabel = meta$label_de[meta$variable == variable],
          startyear = as.numeric(unique(data.csv$year)[1]),
          endyear = as.numeric(unique(data.csv$year)[length(unique(data.csv$year))]),
          tabletype = "mean",
          exportpath = paste0(exportpath, "/numerical/", variable, "/meta.json")
        )

        print(paste(
          "The variable", variable, "is processed with grouping year,",
          paste(difflist[[i]], collapse = ","), "as numeric mean table"
        ))
      }

      if (meta$probtable[var] == "Yes") {
        data <- get_data(
          datasetnum = data.file.num,
          datasetfac = data.file.fac,
          variable = variable,
          year = year,
          weight = weight,
          diffcount = diffcount,
          diffvars = diffvars,
          vallabel = TRUE
        )

        if (diffvars == "") {
          columns <- c("usedvariable", "year")
        } else {
          columns <- c("usedvariable", "year", diffvars)
        }

        prop.data <- get_prop_values(dataset = data, groupvars = columns, alpha = 0.05)

        protected.table <- get_protected_values(dataset = prop.data, cell.size = 30)

        protected.table <- expand_table(
          table = protected.table, diffvar1 = diffvar1,
          diffvar2 = diffvar2, diffvar3 = diffvar3,
          diffcount = diffcount, tabletype = "prop"
        )

        data.csv <- get_table_export(
          table = protected.table, variable = variable,
          metadatapath = paste0(metapath, "variables.csv"),
          exportpath = exportpath, diffcount = diffcount,
          tabletype = "prop"
        )

        json_create_lite(
          variable = variable,
          varlabel = meta$label_de[meta$variable == variable],
          startyear = as.numeric(unique(data.csv$year)[1]),
          endyear = as.numeric(unique(data.csv$year)[length(unique(data.csv$year))]),
          tabletype = "prop",
          exportpath = paste0(exportpath, "/categorical/", variable, "/meta.json")
        )

        print(paste(
          "The variable", variable, "is processed with grouping year",
          paste(difflist[[i]], collapse = ","), "as a categorical percentage table"
        ))
      }
    }
  }
}