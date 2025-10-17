### Libraries
```
library(ggplot2)
library(reshape)
library(RColorBrewer)
library(viridis)
library(treemapify)
library(circlepackeR)
```

### Data
```
Clients<-as.data.frame(read.csv("ClientType.csv", header = TRUE))
```

### Simple Bar Chart
```
OurClientsBar<-print(ggplot(Clients,
             aes(x=Clients$Number.Supported,
                 y=Clients$Client.Type,
                 fill=Clients$Client.Type)) + 
               
  theme_minimal() +
  geom_bar(stat="identity", 
                 alpha=.3) +
          
   labs(y = " ", 
       x = "Total",
       title = "Our Clients",) +
     
  scale_x_continuous(breaks=seq(0,24,by=4)) +
    
  theme(plot.title = element_text(hjust = .5, 
                                  vjust = 2, 
                                  size = 25),
        axis.text = element_text(size=13), 
        axis.title = element_text(size=14), 
        axis.title.x = element_text(margin=margin(t=25)), 
        axis.title.y = element_text(margin=margin(t=25))) + 
    
  guides(fill="none"))

 

ggsave("OurClients_BarPlot.png", plot = OurClientsBar, width = 15, height = 8, dpi = 500)
```


### Treemap
```
OurClientsTree<-print(ggplot(Clients,
             aes(
               #x=Clients$Number.Supported,
                # y=Clients$Client.Type,
                 fill=Clients$Client.Type,
                 area=Clients$Number.Supported,
                 label=Clients$Type.Number)) + 
               
              # theme_minimal() +
               
               geom_treemap(stat="identity", 
                            alpha=.9) +
               
               geom_treemap_text() +
          
   labs(y = " ", 
       x = " ",
       title = "Our Clients")+ 
    
  theme(plot.title = element_text(hjust = .5, 
                                  vjust = 1, 
                                  size = 20),
        axis.text = element_text(size=8)) + 
        #axis.title = element_text(size=10), 
        #axis.title.x = element_text(margin=margin(t=25)), 
        #axis.title.y = element_text(margin=margin(t=25))) +
    
  guides(fill="none") + 
    
    scale_fill_manual(values = c("#222E27","#2E3E34" , "white", "#B2C6BA","#91AD9C","#526F5D" )),breaks = waiver())
     

ggsave("OurClients_TreeMap_TCGcolors.png", plot = OurClientsTree, width = 9, height = 4, dpi = 500)
```


### Sunburst Plot
I am following [this example.](https://plotly.com/r/sunburst-charts/)
##### Libraries
```{r}
library(data.table)
library(dplyr)
library(plotly)

```

#### Dataset (different format)
```{r}

thisDF <- as.data.frame(read.csv("Client_Type_Metrics_longformat.csv", header = TRUE))
```

#### Plot
```{r}
as.sunburstDF <- function(DF, value_column = NULL, add_root = FALSE){
  require(data.table)
  
  colNamesDF <- names(DF)
  
  if(is.data.table(DF)){
    DT <- copy(DF)
  } else {
    DT <- data.table(DF, stringsAsFactors = FALSE)
  }
  
  if(add_root){
    DT[, root := "Total"]  
  }
  
  colNamesDT <- names(DT)
  hierarchy_columns <- setdiff(colNamesDT, value_column)
  DT[, (hierarchy_columns) := lapply(.SD, as.factor), .SDcols = hierarchy_columns]
  
  if(is.null(value_column) && add_root){
    setcolorder(DT, c("root", colNamesDF))
  } else if(!is.null(value_column) && !add_root) {
    setnames(DT, value_column, "values", skip_absent=TRUE)
    setcolorder(DT, c(setdiff(colNamesDF, value_column), "values"))
  } else if(!is.null(value_column) && add_root) {
    setnames(DT, value_column, "values", skip_absent=TRUE)
    setcolorder(DT, c("root", setdiff(colNamesDF, value_column), "values"))
  }
  
  hierarchyList <- list()
  
  for(i in seq_along(hierarchy_columns)){
    current_columns <- colNamesDT[1:i]
    if(is.null(value_column)){
      currentDT <- unique(DT[, ..current_columns][, values := .N, by = current_columns], by = current_columns)
    } else {
      currentDT <- DT[, lapply(.SD, sum, na.rm = TRUE), by=current_columns, .SDcols = "values"]
    }
    setnames(currentDT, length(current_columns), "labels")
    hierarchyList[[i]] <- currentDT
  }
  
  hierarchyDT <- rbindlist(hierarchyList, use.names = TRUE, fill = TRUE)
  
  parent_columns <- setdiff(names(hierarchyDT), c("labels", "values", value_column))
  hierarchyDT[, parents := apply(.SD, 1, function(x){fifelse(all(is.na(x)), yes = NA_character_, no = paste(x[!is.na(x)], sep = ":", collapse = " - "))}), .SDcols = parent_columns]
  hierarchyDT[, ids := apply(.SD, 1, function(x){paste(x[!is.na(x)], collapse = " - ")}), .SDcols = c("parents", "labels")]
  hierarchyDT[, c(parent_columns) := NULL]
  return(hierarchyDT)
}
```

&nbsp;

### Donut plot

#### Libraries
```
library(ggplot2)
library(webr)
```

#### Read in the data
```
DonutData <- as.data.frame(read.csv("Client_Type_Metrics.csv", header = TRUE))
```

#### Formatting
```
DonutData$fraction = DonutData$count / sum(DonutData$count)
DonutData$ymax = cumsum(DonutData$fraction)
DonutData$ymin = c(0, head(DonutData$ymax, n=-1))
```

#### Plot
```
DonutPlot<-
ggplot(DonutData, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
     geom_rect() +
  theme_void() +
     coord_polar(theta="y") + 
     xlim(c(2, 4))

ggsave("Donut.png", plot = DonutPlot, width = 9, height = 4, dpi = 500)
```
