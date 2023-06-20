add_option <- function(...) optparse::add_option(...)
option_parser <- function(...) optparse::OptionParser(...)
parse_args <- function(...) optparse::parse_args(...)

parse_arguments <- function() {
    parser <- option_parser()
    parser <- add_option(
        parser,
        c("-i", "--input"),
        dest = "input",
        default = "H:/data/",
        help = "Path to the location of the input stata datasets."
    )
    parser <- add_option(
        parser,
        c("-d", "--metadata"),
        dest = "metadata",
        default = 
          "https://git.soep.de/kwenzig/publicecoredoku/raw/master/datasets/",
        help = "Path to the location of additional metadata files."
    )
    parser <- add_option(
        parser,
        c("-o", "--output"),
        dest = "output",
        default = "H:/Clone/soep-transfer/",
        help = "Target path for the generated data."
    )
    parser <- add_option(
      parser,
      c("-v", "--version"),
      dest = "version",
      default = "v37",
      help = "SOEP release version."
    )
    parser <- add_option(
        parser,
        c("-n", "--dataset-name"),
        dest = "dataset_name",
        default = "h_statistics",
        help = "dataset name which is used to create aggregated data tables"
    )
    parser <- add_option(
        parser,
        c("-m", "--minimal-group-size"),
        default = 30,
        dest = "minimal_group_size",
        help = "Minimal N for a grouping to be evaluated. Smaller 
        groups are filtered out.",
        type = "integer"
    )
    parser <- add_option(
        parser,
        c("-w", "--weight-column"),
        dest = "weight_column",
        default = "hhrf",
        help = "Name of the column used for weighing the data.",
    )
    parser <- add_option(
        parser,
        c("-y", "--survey-year-column"),
        dest = "survey_year_column",
        default = "syear",
        help = "Name of the column used for weighing the data.",
    )
    return(parse_args(parser))
}

arguments <- parse_arguments()