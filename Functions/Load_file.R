#Load file function
#adapted from H Wickham Mastering Shiny, section 9.1.3

load_file <- function(aName, aPath, aSheet, askip){
  
  ext <- tools::file_ext(aName)
  switch(ext,
         xlsx= readxl::read_excel(aPath, aSheet, skip = askip, .name_repair = "universal", col_types = "numeric"),
         csv = vroom::vroom(aPath, delim = ",", show_col_types = FALSE, .name_repair = "universal"),
         tsv = vroom::vroom(aPath, delim = "\t", .name_repair = "universal"),
         txt = vroom::vroom(aPath, show_col_types = FALSE, .name_repair = "universal"),
         validate("Invalid file. Please upload a data file")
  )
}

#note: for large files DataTable fread is faster for downloading csv files