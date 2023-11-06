# CellMarkerR

## Description
CellMarkerR is a localized tool that includes two core drawing functions. These functions are based on the database that is publicly available on the CellMarker website.(http://bio-bigdata.hrbmu.edu.cn/CellMarker/CellMarkerSearch.jsp)

## Getting Started

### Step.1 Install package dependencies

(Option A) Use R language code to install dependent packages.
```
# install.packages()
install.packages(c("plotly","htmlwidgets","wordcloud2","openxlsx"))

# BiocManager::install()
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install(c("plotly","htmlwidgets","wordcloud2","openxlsx"))
```

(Option B) Use conda to install dependent packages.
```
conda install -c conda-forge r-plotly r-htmlwidgets r-wordcloud2 r-openxlsx
```

### Step.2 Install the package 

(Option A) Install the package directly from local.
```
install.packages("./CellMarkerR_1.0.0.tar.gz",repos=NULL,type="source")
```

(Option B) Install the package directly from github using the devtools package. 
```
devtools::install_github("KIRA2ZERO/CellMarkerR")
```

### Step.3 Library the package and initialize instance object
```
library(CellMarkerR)
CellMarker = CellMarkerR$new()
```
### Step.4 Use class methods

```
# viewMarker
CellMarker$viewMarker(marker="CD4")
CellMarker$getViewMarkerTableResult()
CellMarker$saveViewMarkerChartResult(filePath) # File suffix name must be html
CellMarker$saveViewMarkerTableResult(filePath) # File suffix name must be one of c('csv','xlsx')

# viewCell
CellMarker$viewCell()
CellMarker$getViewCellChartResult()
CellMarker$getViewCellTableResult()
CellMarker$getViewCellRankResult()
CellMarker$saveViewCellChartResult(filePath) # File suffix name must be html
CellMarker$saveviewCellTableResult(filePath) # File suffix name must be one of c('csv','xlsx')
CellMarker$saveviewCellTableResult(filePath) # File suffix name must be one of c('csv','xlsx')
```


