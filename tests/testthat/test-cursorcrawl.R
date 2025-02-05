test_that("cursor based paging for works works", {

  skip()

  works_filter <- "publication_year:2015-2023,primary_topic.id:t10783"

  cc <- 
    works_filter |> 
    openalex_works_cursorcrawl(n_max_pages = 10)

  mydir <- unique(dirname(cc))
  is_valid <- all(cc %in% dir(mydir, full.names = TRUE))

  expect_true(is_valid)

  # cmd <- paste0("flatterer --force --nocsv --parquet -m works --id-prefix work -j ",
  #   paste(collapse = " ", cc), " ", mydir, paste0(mydir, "/cursorcrawl"))
  
  # system(cmd)

})
