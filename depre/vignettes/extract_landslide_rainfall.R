## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----paths, results=F---------------------------------------------------------
# the path to the iffi polygons
landslide_poly_path = "\\\\projectdata.eurac.edu/projects/Proslide/Landslides/Iffi_db_xxxx_to_2018/exportperEurac2020/Shapefiles/IFFI10_5.shp"

# the path to the iffi points
landslide_point_path = "\\\\projectdata.eurac.edu/projects/Proslide/Landslides/Iffi_db_xxxx_to_2018/exportperEurac2020/Shapefiles/IFFI10_1.shp" 

# the path to the folder with the iffi-databases 
database_dir = "\\\\projectdata.eurac.edu/projects/Proslide/Landslides/Iffi_db_xxxx_to_2018/exportperEurac2020/database" 

# the path to the root folder of the gridded rainfall data
path_ncdf = "\\\\projectdata.eurac.edu/projects/Proslide/PREC_GRIDS_updated/"

## ----libs, message=F, warning=F-----------------------------------------------
# load some libraries
library(rainfallR)
library(ggrepel)
library(gganimate)
library(dplyr)
library(ggplot2)
library(iffitoR) # load some sample landslide data that comes with the package
library(sf)
library(raster)
library(forcats)
library(glue)
library(purrr)
library(stringr)
library(crayon) # for some colours

## ----pointdataiffi, message=F, warning=F, cache=T, results=F------------------
# only query the data when working under windows due to the RODBC drivers
os = Sys.info()["sysname"]
if (os == "Windows") {
  
iffi_points = make_shapefile(
  database_dir = database_dir,
  # normally null only setting it here for me
  attribute_database_name = "tbl_frane",
  # the name without extension
  dictionary_database_name = "diz_frane",
  shapefile = landslide_point_path, 
  
  # the colums we want to retrieve directly
  attri = c("anno_min",
            "mese_min",
            "giorno_min",
            "area"),

  # tables to join from the other tables (for more see vignette)
  joins = list(
    "tbl_frane.Generalita.Cod_tipo" = c(
      "diz_frane.diz_tipo_movi.cod_tipo",
      "diz_frane.diz_tipo_movi.tipologia"
    ),
    "tbl_frane.clas_ii_liv.movimento" = c(
      "diz_frane.diz_movimenti.movimento",
      "diz_frane.diz_movimenti.nome_movimento"
    ),
    "tbl_frane.ass_gen_cause.causa" = c(
      "diz_frane.diz_cause.causa",
      "diz_frane.diz_cause.nome_causa"
    )
  )
)
  
}


## -----------------------------------------------------------------------------
# what have we got
dplyr::glimpse(iffi_points)

## ---- message=F, warning=F----------------------------------------------------
if (os == "Windows") {
  # translate the two classifying columns to english
  iffi_points_1 = iffitoR::translate_iffi(iffi_points)
  
  # get some more date information
  iffi_points_2 = iffitoR::get_date_information(iffi_points_1)
  
  # drop the geometry, so that it gets a little more space
  iffi_points_2 %>% st_drop_geometry() %>% dplyr::select(c(matches("date|day|year|month|first|second"))) %>% head()
}

## ----filter, echo=F-----------------------------------------------------------
if (os == "Windows") {
  # filter the ones that have day information
  iffi_day = iffi_points_2 %>% filter(date_info == "day")
  
  # filter only the translational slides
  iffi_translational = iffi_day %>% filter(str_detect(second_level, "translational"))
  
  #!! Cut away everything before 1980 for the rainfall data to match
  iffi_translational = iffi_translational %>% filter(year.int >= 1980)
  
  dim(iffi_translational)
}

## ----slidessameday------------------------------------------------------------
# now lets use the data that comes already preprocessed with the iffitoR package
# we need to filter the right slides as we did above for the data queried from the databse
landsld = landsld %>%
  filter(str_detect(second_level, "translational|rotational")) %>%
  filter(date_info == "day") %>%
  filter(year.int >= 1980) 

# create the list for the events on the same day
slides_same_day = list()

for (row in 1:nrow(landsld)) {
  # get the day of the event
  dts = landsld[row,][["date"]]
  dts_chr = as.character(dts) %>% str_replace_all(., "-", "")

  # add this spatial object to the list with the name being the day
  if(dts_chr %in% names(slides_same_day)){
    slides_same_day[[dts_chr]] = rbind(slides_same_day[[dts_chr]], landsld[row,])
  } else{
    slides_same_day[[dts_chr]] = landsld[row, ]
  }
}

## -----------------------------------------------------------------------------
# create a vector of the slides per day
n_slides_per_day = sapply(slides_same_day, nrow)

# what is the max slides per day
m = max(n_slides_per_day)

# when did it happen
n_slides_per_day[n_slides_per_day == m]

## ----extractrainfallslides, message=FALSE, warning=FALSE, cache=TRUE, results="hide"----

# lets only take a smaller subset
slides_same_day = slides_same_day[1:20]

# for each day get the rainfall at the slide location
out = vector("list", length=length(slides_same_day))

# measure the time
start = Sys.time()

# iterate over the spatial object
for (i in seq_along(slides_same_day)) {
  
  # print some informative message
  n = length(slides_same_day)
  str = paste0(i, "/", n)
  dashes = paste0(replicate(20, "-"), collapse = "")
  cat(yellow$bold$underline("\n------------", str, dashes, "\n\n"))
  
  # get the date of the slides
  # this is one of the inputs to the function
  dts = names(slides_same_day)[[i]] %>% as.Date(., "%Y%m%d")

  # the spatial object
  # another input
  spatial.obj = slides_same_day[[i]]

  # some other arguments
  days_back =7 
  seqq = FALSE # we only consider one day per slides (not considering the days back)

  # this returns a list with one element
  # if the dts-argument would have more than one data this list would have more arguments
  rf = invisible(rainfallR::get_rainfall(
    spatial.obj = spatial.obj,
    dts = dts,
    seqq = seqq,
    days_back = days_back
  ))
  
  # there can only be one list element as we extract data for one day each
  out[[i]] = rf
  
}

end = Sys.time()
took=end-start

## -----------------------------------------------------------------------------
# will bring each dataframe in a "long" fomat
out_long_dfs = map(out, function(x) {rainfallR::make_cumulative_rainfall(res = x, days_back=days_back)})

# one column with the cumulative sum (cumsum) and one with the dates and also an integer
# column with the dates before the event
head(out_long_dfs[[1]])

## ----extractfirst, message=F--------------------------------------------------
# so we extract the first (and only) element of each list
out_simple_list = map(out_long_dfs, ~.x[[1]])

### create unique id for each slide
# get the first and already create the new id
out_unique_slide = out_simple_list[[1]] %>% 
  mutate(id = paste0(1, "_", id))

# loop over the others and use the counter variable as additional identifier
for (i in seq_along(out_simple_list)) {
  # dont do it for the first one
  if(!i==1){
    df = out_simple_list[[i]] %>% 
      mutate(id = paste0(i, "_", id))
    out_unique_slide = rbind(out_unique_slide, df)
      
  }
}

## -----------------------------------------------------------------------------
ggplot(out_unique_slide) +
  geom_line(aes(x = days_before_event, y = cumsum, group=id, col=second_level)) +
  scale_x_reverse() +
  labs(col="",
       x = "Days before event",
       y = "Cumulative rainfall [mm/day]",
       title = "Cumulative Rainfall at slides-points") +
  theme_light()

## ---- fig.width=10, warning=F, message=F--------------------------------------
# create an upcounting couter
out_unique_slide %>% 
  group_by(id) %>% 
  filter(!is.na(area)) %>% 
  mutate(time_anim = row_number()) %>% 
  mutate(area = log10(area)) %>% 
ggplot(., aes(x=days_before_event, y=cumsum, group=id, col=second_level), lwd=2) +
  geom_line() +
  geom_point(aes(size=area)) +
  scale_x_reverse() +
  geom_segment(aes(xend=0, yend=cumsum), linetype=2, colour="grey") +
  geom_text(aes(x = -0.3, label=PIFF_ID)) +
  theme_light() +
  labs(title="Cumulative Rainfall before Landslides",
       subtitle = "at point location",
       x = "Days before Slide", 
       y = "Cumulative Rainfall per day [mm]",
       size = "log 10 area ") +
  transition_reveal(time_anim)

