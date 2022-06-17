
myfiles="Data/171819-tacc.csv" #read the geocoded 17, 18 and 19 - tacc combined files

##2.1 Load Data
#files <-list.files(file.path(base_dir,"/Data"), pattern = "*\\.xls$|*\\.csv$")
#print(files)
var_list <- vector(mode = "list")
var_names <- NULL
for(i in seq_along(myfiles)){
  filename <- str_sub(myfiles[i], start = 1, end = -5)
  sf_i <- tryCatch({
    if(tools::file_ext(myfiles[i]) == "xls"){
      dat2 <- readxl::read_xls(file.path(myfiles[i])) 
    } else if(tools::file_ext(myfiles[i]) == "csv"){
      dat2 <- read.csv(file.path(myfiles[i])) 
    }
    dat2 %>%
      filter(!is.na(Latitude) | !is.na(Longitude)) %>%
      st_as_sf(.,coords = c("Latitude", "Longitude"), crs = 'ESRI:102311')%>%
      st_set_crs(.,"+proj=gnom +lat_0=39.55 +lon_0=-75.35")
    #sf_proj_pipelines() <-st_transform(, CRS("+proj=gnom +lat_0=90 +lon_0=-50"))
    
  }, error = function(e){
    cat(filename, "error = ",e$message,"\n")
    return(e)
  }
  )
  if(!inherits(sf_i, "error")){
    var_list[[length(var_list)+1]] <- sf_i
    var_names[length(var_list)] <- filename
  }
}
names(var_list) <- var_names
print(var_list)
