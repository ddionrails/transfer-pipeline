read_metadata <- transfer.pipeline::read_metadata

test_that("Read Metadata", {
        read_metadata("./data/variables.csv")
})
