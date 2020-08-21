library(tercen)
library(dplyr)
library(Biobase)

doc_to_data <- function(df){
  filename <- tempfile()
  writeBin(ctx$client$fileService$download(df$documentId[1]), filename)
  on.exit(unlink(filename))
  
  # data_fcs = read.FCS(filename, transformation = FALSE)
  # names_parameters = data_fcs@parameters@data$desc
  # data = as.data.frame(exprs(data_fcs))
  # col_names = colnames(data)
  # names_parameters = ifelse(is.na(names_parameters),col_names,names_parameters)
  # colnames(data) = names_parameters
  # print(class(data))
  data_anndf <- get(load(filename))
  
  # data <- pData(data_anndf)

  
  data %>%
    mutate_if(is.logical, as.character) %>%
    mutate_if(is.integer, as.double) %>%
    mutate(.ci = rep_len(df$.ci[1], nrow(.)))
}

ctx = tercenCtx()

if (!any(ctx$cnames == "documentId")) stop("Column factor documentId is required") 

ctx$cselect() %>% 
  mutate(.ci = 1:nrow(.) - 1) %>%
  split(.$.ci) %>%
  lapply(doc_to_data) %>%
  bind_rows() %>%
  ctx$addNamespace() %>%
  ctx$save()

