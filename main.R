library(tercen)
library(dplyr)
library(tidyr)
library(Biobase)

doc_to_data <- function(df) {
  filename <- tempfile()
  writeBin(ctx$client$fileService$download(df$documentId[1]), filename)
  on.exit(unlink(filename))

  data_anndf <- get(load(filename))
  
  data_anndf_long <- tidyr::gather(data_anndf@data, "label", "value") 
  
  anndf_meta <- data_anndf@varMetadata
  anndf_meta$label <- rownames(anndf_meta)
  
  data <- merge(x = anndf_data_long, y = anndf_meta, by = "label", all = TRUE)

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

