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
    nrow(more$ids) == length(dois)
  
  expect_true(is_valid)

})

test_that("a specific doi with some attributes being null (best_oa_location_source) can be rectangularized", {

  my_doi <- "10.1007/978-94-017-9756-6"

  #my_doi |> openalex_doi_lookup("identifiers")
  works <- my_doi |> openalex_doi_lookup("all")
  
  #works <- openalex_crawl("works", query = list(filter = paste0("doi:", my_doi)), verbose = TRUE)
  
  #lol <- 
    #list(list(results = reduce(works |> map("results"), c)))
  
  #my_works <- 
    #lol |> openalex_works_to_tbls()

  is_valid <- works$work |> nrow() == 1

  expect_true(is_valid)
})

test_that("doi binds for biblio and grants works", {

  my_dois <- "10.15252/msb.20188503
10.1016/j.enconman.2021.114002
10.1038/s41598-023-29525-9
10.3389/fnagi.2023.1303036
10.1016/j.jmsy.2025.02.018
10.1016/j.jairtraman.2011.11.006
10.1109/mlsp.2014.6958872
10.1016/j.ijplas.2012.11.010
10.1109/tgrs.2003.819442
10.1016/j.enbuild.2013.05.006
10.1039/d0ta08874a
10.1088/0953-8984/28/40/403003
10.1039/c1cp20096h
10.1016/j.automatica.2011.05.029
10.3934/naco.2020018
10.1109/tifs.2021.3053462
10.1016/j.anucene.2023.110123
10.1016/j.calphad.2004.01.005
10.1007/978-3-540-87881-0_34
10.1007/s12667-013-0104-2
10.1109/wsc48552.2020.9383863
10.1109/mce.2021.3060722
10.1515/crelle.2009.086
10.1109/cse.2009.301
10.1109/mcd.2006.272996
10.1109/cluster48925.2021.00103
10.1109/icmech.2009.4957200
10.1109/ict.2016.7500374
10.1063/1.2943676
10.1016/j.euromechflu.2020.07.003
10.1109/icact.2008.4494043
10.1117/12.2581044
10.1080/11035890903469963
10.13009/eucass2017-460
10.1016/j.jmsy.2011.07.004
10.1016/j.rcim.2024.102906
10.4018/ijertcs.2019010103
10.1007/s00209-015-1446-3
10.1109/tvlsi.2018.2846298
10.1007/978-981-19-9822-5_192
10.1002/aic.690490822
10.1117/12.2612426
10.1088/1361-6404/ac79e0
10.1063/1.4830412
10.1016/j.procir.2019.04.027
10.4310/pamq.2021.v17.n5.a3
10.1038/s41563-023-01594-1
10.1007/978-3-319-53426-8_17
10.23919/jcn.2020.000032
10.4018/978-1-6684-3666-0.ch030
10.1016/s0009-2541(03)00161-x
10.1088/1361-6587/adaa16
10.1007/s10853-006-1199-9
10.1088/1361-6587/ac4b94
10.1109/pct.2007.4538553
10.1088/1741-4326/aad9ad
10.1007/s12289-011-1080-5
10.1016/j.anucene.2017.10.019
10.1088/0029-5515/48/10/105002
10.1080/14786430600589089
10.1520/jai100692
10.1109/epec.2012.6474944
10.1016/j.jtbi.2014.01.032
10.1088/0029-5515/45/12/006
10.1109/icpadm.2003.1218552
10.5194/angeo-27-2545-2009
10.1016/j.apgeochem.2013.04.014
10.1016/j.ijrmhm.2018.10.007
10.1016/j.anucene.2019.107027
10.1034/j.1600-0692.2001.300304.x
10.1016/j.ultras.2004.01.023
10.2495/wp100261
10.1109/iedm.2017.8268384
10.2202/1542-6580.2127
10.1007/s40328-015-0116-6
10.4271/2016-01-1821
10.1016/j.nme.2016.10.022
10.1088/0741-3335/43/10/301
10.1179/1433075x13y.0000000121
10.1016/j.euromechsol.2006.01.001
10.3183/npprj-2005-20-01-p016-023
10.1109/pmaps.2018.8440209
10.1088/0031-8949/t167/1/014078
10.1109/icc45855.2022.9838556
10.3183/npprj-1995-10-03-p156-161
10.1109/itherm.2004.1319211
10.1016/j.apacoust.2019.05.033
10.1080/00084433.2015.1118841
10.1109/compel.2016.7556706
10.1007/978-3-319-67443-8_29
10.1016/j.wear.2019.202957
10.1049/el:20030208
10.1002/ctpp.201700118
10.3182/20070821-3-ca-2919.00012
10.14311/app.2022.37.0024
10.1016/j.gca.2006.06.782
10.4028/www.scientific.net/amr.20-21.189
10.1520/stp46571s
10.1109/ppps.2007.4652392
10.1002/pssc.200405424
" |> readr::read_lines()
    
  res <- openalex_doi_lookup(my_dois, "all")

  # doi_filters <- 
  #   split_chunks_of_n(dois, 50) |> 
  #   map_chr(openalex_or)

  # doi_crawl2 <- function(dois) {
  #   works <- 
  #     openalex_crawl("works", fmt = "object",
  #       query = openalex_query(filter = paste0("doi:", dois))
  #     )
    
  #   lol <- 
  #     list(list(results = reduce(works |> map("results"), c)))  

  #   return(lol)
    
  # }

  # im <- 
  #   doi_filters |> map(doi_crawl2) |> 
  #       purrr::reduce(c)

  # lu <- 
  #   im |> map("results") |> list_c() |> 
  #   list() |> set_names(nm = "results") |> list() |> 
  #   openalex_works_to_tbls() 

  # is_valid <- nrow(lu$biblio) == 100 & nrow(lu$grants >= 27)

  is_valid <- 
    nrow(res$biblio == 100) &
    nrow(res$grants) >= 27 &
    class(res$biblio$biblio_volume) == "character" &
    class(res$grants$award_id) == "character"
    
  expect_true(is_valid)

})
