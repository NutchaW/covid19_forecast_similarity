# get paths
#path <- 
full_model_list <- list.files(path=path, pattern = "metadata",recursive = TRUE)

# save data
for(x in full_model_list){
  file.copy(from=paste0(path,x), 
            to=paste0("./data/metadata/", sub('.*\\/', '', x)), 
            overwrite = TRUE, 
            recursive = FALSE, 
            copy.mode = TRUE)
}