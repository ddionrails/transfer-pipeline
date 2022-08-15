get_labels_for_dataset <- transfer.pipeline::get_labels_for_dataset

test_that("Get value labels", {
        dataset <- rep("p_statistic", 3)
        variable <- c(rep("age", 2), "no_age")
        value <- c(1, 2, 1)
        label <- c("17-34", "35-65", "")


        test_dataframe <- data.frame(dataset, variable, value, label)

        expected <- list(
                "age" = list("1" = "17-34", "2" = "35-65"),
                "no_age" = list("1" = "")
        )
        result <- get_labels_for_dataset("p_statistic", test_dataframe)
        expect_identical(expected, result)
})
