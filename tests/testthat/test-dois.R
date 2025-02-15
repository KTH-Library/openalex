test_that("doi lookup works for 20 dois", {
 
  skip_on_ci()

  dois <- paste0("10.1016/j.aos.2023.101522, 10.1051/m2an/2024042, ",
    "10.1016/j.heliyon.2024.e25125, 10.1145/3664476.3664508, ",
    "10.23919/ECC64448.2024.10590962, 10.1109/TCNS.2023.3285863, ",
    "10.23919/ECC64448.2024.10591128, 10.1007/s10570-023-05674-y, ",
    "10.1109/APWC61918.2024.10701979, 10.1137/23M1587804, ",
    "10.1109/FDL63219.2024.10673844, 10.1007/978-3-031-54776-8_12, ",
    "10.1137/22M148968X, 10.1016/j.trc.2023.104454, 10.1108/ECON-10-2023-0163, ",
    "10.1016/j.apenergy.2024.122690, 10.1038/s41467-023-44315-7, ",
    "10.1109/TCI.2024.3463485, 10.1016/j.jobe.2024.110536, ",
    "10.1007/s13721-024-00446-5") |> 
    strsplit(split = ", ") |> unlist() 

  doi_filter <- function(dois) dois |> openalex_or()

  doi_filters <- 
    dois |> 
    split_chunks_of_n(50) |> 
    map(doi_filter)
  
  #doi_filters |> map(\(x) doi_lookup_identifiers(doi_filter = x))

  #ids <- openalex_doi_lookup(dois, "identifiers")
  more <- openalex_doi_lookup(dois, "all")

  is_valid <- 
    #nrow(ids) == length(dois) & 
    nrow(more[[1]]$ids) == length(dois)
  
  expect_true(is_valid)

})

