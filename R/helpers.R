
setClass(
        "Metadata",
        slots = list(
                categorical = "list",
                numerical = "list",
                ordinal = "list",
                independent = "list"
        )
)


#' @export read_metadata
read_metadata <- function(path) {
        full_metadata <- read.csv(path, header = TRUE)
        statistics_metadata <- full_metadata[full_metadata$statistics == "True", ]
}
