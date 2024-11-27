#' Convert string in wos-plaintext format to tibble
#' @param D string in wos-plaintext format
#' @importFrom dplyr ungroup
#' @importFrom tidyr pivot_wider
#' @noRd
isi2df<-function(D){
  
  #D <- D[nchar(D)>0]  # remove empty rows
  
  # remove empty rows and strange characters
  res <- try(D <- D[nchar(D)>1], silent = T)
  if(inherits(res, "try-error")){
    D <- removeStrangeChar(D)
    #next
  }else{
    D <- res
    rm(res)
  }
  
  D <- D[!(substr(D,1,3) %in% c("FN ", "VR "))]
  
  for (i in 1:length(D)){
    if (substr(D[i],1,3)=="   ") substr(D[i],1,3) <- substr(D[i-1],1,3) 
  }
  Papers <- which(substr(D,1,3)=="PT ") # first row of each document
  nP=length(Papers)  # number of documents
  
  rowPapers <- diff(c(Papers, length(D)+1))
  
  numPapers <- rep(1:nP,rowPapers)
  
  DATA <- data.frame(Tag = substr(D,1,3), content = substr(D,4,nchar(D)), Paper=numPapers)
  DATA$Tag <- gsub(" ","",DATA$Tag)
  df <- DATA %>% group_by(Paper, Tag) %>%
    summarise(cont=paste(content, collapse="---",sep="")) %>%
    arrange(Tag, Paper) %>%
    pivot_wider(names_from =  Tag,values_from = cont) %>%
    ungroup() 
  df <- as.data.frame(df)
  
  
  df$PY <- as.numeric(df$PY)
  
  missingTags <- setdiff(c("AU","DE","C1","RP","CR","PY","SO","TI","TC"),names(df))
  if (length(missingTags)>0){
    cat("\nWarning:\nIn your file, some mandatory metadata are missing. Bibliometrix functions may not work properly!\n
Please, take a look at the vignettes:
- 'Data Importing and Converting' (https://www.bibliometrix.org/vignettes/Data-Importing-and-Converting.html)
- 'A brief introduction to bibliometrix' (https://www.bibliometrix.org/vignettes/Introduction_to_bibliometrix.html)\n\n")
    cat("\nMissing fields: ",missingTags,"\n")
  }
  
  ### replace "---" with ";"
  tagsComma <- c("AU","AF","CR")
  
  nolab <- setdiff(tagsComma,names(df))
  
  tagsComma <- tagsComma[(!(tagsComma %in% nolab))]
  
  df1 <- data.frame(lapply(df[tagsComma],function(x){
    gsub("---",";",x)
  }))
  
  ### replace "---" with " "
  otherTags <- setdiff(names(df),tagsComma)
  df2 <- data.frame(lapply(df[otherTags],function(x){
    trimES(gsub("---"," ",x))
  }))
  df <- cbind(df1,df2)
  rm(df1,df2)
  
  ### store raw affiliation format to extract link among authors and affiliations
  df$C1raw <- df$C1
  ###
  
  df$DB <- "ISI"
  
  # Authors
  df$AU <- trimES(gsub(","," ",df$AU))
  
  # Toupper
  DI <- df$DI
  df <- data.frame(lapply(df,toupper))
  df$DI <- DI
  
  # add sep ; to affiliations
  df$C1 <- trim(gsub("\\[.*?\\]", "", df$C1)) # to remove author info in square brackets
  df$C1 <- gsub("\\.",".;",df$C1)

  df <- df[names(df)!="Paper"]
  
  return(df |> tibble::as_tibble())
}

read_wos_plaintext <- function(wos_plaintext, remove.duplicates = TRUE) {

  M <- isi2df(wos_plaintext)
  dbsource <- "isi"
 
  # remove double ;
  M <- M %>% 
    mutate_if(is.character, ~gsub(";;",";",.x))

  ### SR field creation
  if (isTRUE(remove.duplicates)){
    switch(dbsource,
          isi={
            id_field <- "DI"
          },
          scopus={
            if (format=="csv"){
              id_field <- "UT"
            } else {
              id_field <- "TI"
            }
            
          },
          openalex={
            id_field <- "id_oa"
          },
          openalex_api={
            id_field <- "id_oa"
          },
          dimneisons={
            id_field <- "UT"
          },
          pubmed={
            id_field <- "PMID"
          },
          lens={
            id_field <- "UT"
          },
          {
            id_field <- "TI"
          })
    d <- duplicated(M[id_field]) 
    if (sum(d)>0) cat("\nRemoved ",sum(d),"duplicated documents\n")
    M <- M[!d,]
    }
  suppressWarnings(M <- metaTagExtraction(M, Field="SR"))
  #row.names(M) <- M$SR  
  M |> readr::type_convert() |> suppressMessages()

}

trimES <- function (x) 
{
    gsub("\\s+", " ", x)
}

removeStrangeChar <- function (D) 
{
    ind <- numeric(length(D))
    for (i in 1:length(D)) {
        res <- try(ind[i] <- nchar(D[i]), silent = TRUE)
        if (inherits(res, "try-error")) {
            ind[i] <- 0
            next
        }
    }
    D <- D[ind > 1]
}

trim <- function (x) 
{
    gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

metaTagExtraction <- function (M, Field = "CR_AU", sep = ";", aff.disamb = TRUE) 
{
    if ("CR" %in% names(M)) {
        M$CR = gsub("DOI;", "DOI ", as.character(M$CR))
    }
    if (Field == "SR") {
        M <- SR(M)
    }
    if (Field == "CR_AU") {
        M <- CR_AU(M, sep)
    }
    if (Field == "CR_SO") {
        M <- CR_SO(M, sep)
    }
    if (Field == "AU_CO") {
        M <- AU_CO(M)
    }
    if (Field == "AU1_CO") {
        M <- AU1_CO(M, sep)
    }
    if (Field == "AU_UN") {
        if (isTRUE(aff.disamb)) {
            M <- AU_UN(M, sep)
        }
        else {
            M$AU_UN = gsub("\\[.*?\\] ", "", M$C1)
            M$AU1_UN = unlist(lapply(strsplit(M$RP, sep), function(l) {
                l = l[1]
                return(l)
            }))
            ind = regexpr("\\),", M$AU1_UN)
            a = which(ind > -1)
            M$AU1_UN[a] = trim(substr(M$AU1_UN[a], ind[a] + 2, 
                nchar(M$AU1_UN[a])))
        }
    }
    return(M)
}

SR <- function (M) 
{
    listAU = strsplit(as.character(M$AU), ";")
    listAU = lapply(listAU, function(l) trim.leading(l))
    if (M$DB[1] == "scopus") {
        listAU = lapply(listAU, function(l) {
            l = trim(l)
            l = sub(" ", ",", l, fixed = TRUE)
            l = sub(",,", ",", l, fixed = TRUE)
            l = gsub(" ", "", l, fixed = TRUE)
        })
    }
    FirstAuthors = gsub(",", " ", unlist(lapply(listAU, function(l) {
        if (length(l > 0)) {
            l = l[[1]]
        }
        else (l = "NA")
        return(l)
    })))
    if (!is.null(M$J9)) {
        no_art = which(is.na(M$J9) & is.na(M$JI))
        M$J9[no_art] = M$SO[no_art]
        ind = which(is.na(M$J9))
        M$J9[ind] = trim(gsub("\\.", " ", M$JI[ind]))
        SR = paste(FirstAuthors, M$PY, M$J9, sep = ", ")
    }
    else {
        no_art = which(is.na(M$JI))
        M$JI[no_art] = M$SO[no_art]
        J9 = trim(gsub("\\.", " ", M$JI))
        SR = paste(FirstAuthors, M$PY, J9, sep = ", ")
    }
    M$SR_FULL <- gsub("\\s+", " ", SR)
    SR <- gsub("\\s+", " ", SR)
    st <- i <- 0
    while (st == 0) {
        ind <- which(duplicated(SR))
        if (length(ind) > 0) {
            i <- i + 1
            SR[ind] = paste0(SR[ind], "-", letters[i], sep = "")
        }
        else {
            st <- 1
        }
    }
    M$SR <- SR
    return(M)
}

trim.leading <- function (x) 
{
    sub("^\\s+", "", x)
}