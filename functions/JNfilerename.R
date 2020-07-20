###DON'T USE THIS IF THE FILE NAMES ARE RIGHT###
#file rename to new participant IDs

JNfilerename <- function(){
  old_files <- list.files("~/hannah-merseal/jazznets/pilot data/raw", pattern = "*.csv", full.names = TRUE)
  new_files <- paste0("~/hannah-merseal/jazznets/pilot data/raw/JN_", 1:length(old_files), ".csv")
  file.copy(from = old_files, to = new_files)
  file.remove(old_files)
}

