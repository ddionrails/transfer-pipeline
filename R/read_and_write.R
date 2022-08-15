
#' @export get_labels_for_dataset
get_labels_for_dataset <- function(dataset_name, label_metadata) {
    label_metadata <- label_metadata[label_metadata$dataset == dataset_name]
    return(list(
        "age" = list("1" = "17-34", "2" = "35-65"),
        "no_age" = list("1" = "")
    ))
}
