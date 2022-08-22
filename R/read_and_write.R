
#' @export get_labels_for_dataset
get_labels_for_dataset <- function(dataset_name, label_metadata) {
    label_metadata <- label_metadata[label_metadata$dataset == dataset_name]
    variables <- as.vector(label_metadata$variable)
    labels <- as.vector(label_metadata$label)
    values <- as.vector(label_metadata$value)
    value_labels <- list()
    for (row_index in seq_along(variables)) {
        value_label <- list()

        value_label[[values[row_index]]] <- labels[row_index]

        if (variables[row_index] %in% value_labels) {

        }
        value_labels[[variables[row_index]]] <- append(
            value_labels[[variables[row_index]]],
            value_label
        )
    }
    return(value_labels)
}
