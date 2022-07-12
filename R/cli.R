add_option <- function(...) optparse::add_option(...)
option_parser <- function(...) optparse::OptionParser(...)
parse_args <- function(...) optparse::parse_args(...)

define_arguments <- function() {
    parser <- option_parser()
    parser <- add_option(
        parser,
        c("-i", "--input"),
        help = "Path to the location of the input stata datasets."
    )
    parser <- add_option(
        parser,
        c("-d", "--metadata"),
        help = "Path to the location of additional metadata files."
    )
    parser <- add_option(
        parser,
        c("-o", "--output"),
        help = "Target path for the generated data."
    )
    parser <- add_option(
        parser,
        c("-n", "--dataset-name"),
        default = "p_statistics",
        help = "Target path for the generated data."
    )
    parser <- add_option(
        parser,
        c("-m", "--minimal-group-size"),
        default = 30,
        help = paste0(
            "Minimal N for a grouping to be evaluated. ",
            "Smaller groups are filtered out.",
        ),
        type = "integer"
    )
    parser <- add_option(
        parser,
        c("-w", "--weight-column"),
        default = "phrf",
        help = "Name of the column used for weighing the data.",
    )
    parser <- add_option(
        parser,
        c("-y", "--survey-year"),
        default = "syear",
        help = "Name of the column used for weighing the data.",
    )
    return(parse_args(parser))
}
