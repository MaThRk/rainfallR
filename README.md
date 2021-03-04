# rainfallR

## Installation

- The more recent commits go to gitlab. However the version in Github should be more reliable


#### <u> gitlab </u>

```r
# the paths to the public and private ssh-key
public = "C://PATH/TO/PUBLIC/id_rsa.pub",
private = "C://PATH/TO/PRIVATE/id_rsa"

creds = git2r::cread_ssh_key(public, private)

# install it using devtools
devtools::install_git("git@gitlab.inf.unibz.it:proslide/rainfallR.git",
                      credentials = creds)

```


#### <u> github </u>

`remotes::install_github("RobinKohrs/rainfallR")`


## Main Functionalities

In order for this to work, the data needs to be organisd in a specific structure, that is more or less like this: 

- This means that the root-folder here is `PREC_GRIDS`
  + One level below are folders for all the years
    + and on the bottom-most level there are all the NetCDFs with the data for one month
    
```
PREC_GRIDS
├── 1980
│   ├── DAILYPCP_198001.nc
│   ├── DAILYPCP_198002.nc
│   ├── DAILYPCP_198003.nc
│   ├── DAILYPCP_198004.nc
```

***

### Get access to single NetCDFs

- One can easily get the path to one raster with the function `get_nc_path`  

```r
data_path = "absolute_or_relative_path_to/PREC_GRIDs"
days_back = 1
day = as.Date("2009-01-20")
# will return:
# $`200901`
#[1] "absolute_or_relative_path_to/PREC_GRIDs2009/DAILYPCP_200901.nc"
```

***

### Extract data of NetCDFs for Vector data

#### <u> Point data </u>

> If we are interested in the extraction of rainfall for point data, a tyical workflow could look like this:

1. **get the landslide data**

  * here the `iffitoR`-package already comes with some prebuild dataset (`landsld`)

2. **Create a list of slides that happened on the same day in South Tyrol**
  
  * You can filter the `landsld` data according to your needs first
  
  * And then run the function `iffi10_same_day` which returns a list. Each elemet in this list is a spatial dataframe with one or more slides that happened on the same day
  
3. **Extract the rainfall for those slides**

  * Now you can iterate over each element in the list and exract the rainfall for that date for those points. In [this vignette](https://robinkohrs.github.io/rainfallR/articles/extract_rainfall_landslidePoints.html) one can find an approach to do this in parallel
 
 
#### <u> Polygon data </u>

> For Polygon data we probably want to perform some kinf of aggregation. So the workflow might look a little different

1. **get the polygon data**
  
  * Something like the slope units, landslide polyons or catchments

2. **Find all the polygons that experienced landslides**

  * The function `slide_dates_in_polygon` returns one large `dataframe` where each row is one polygon that experienced at least one movement at a particular day. The column `slides_per_poly_data` contains the number of movements per day in that polygon.
  
3. **Extract the rainfall for each of the polygons**
 
  * The function `get_rainfall_for_polygons` now extracts in parallel the rainfall for the each polygon. 


#### More help

- See [this vignette for more info](https://robinkohrs.github.io/rainfallR/articles/extract_landslide_rainfall.html)

***

![](man/figures/readmeplot.png)
