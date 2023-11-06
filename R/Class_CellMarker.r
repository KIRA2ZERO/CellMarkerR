#' @title CellMarker Class
#'
#' @description
#' This class contains two core drawing functions of CellMarker website.
#'
#' @export
CellMarkerR = R6::R6Class(
    classname = "CellMarker",
    public = list(
        #' @description
        #' Creates a new instance of this [R6][R6::R6Class] class.
        initialize = function(){
            private$loadRawData()
            message("CellMarkerR is a localized tool that includes two core drawing functions. These functions are based on the database that is publicly available on the CellMarker website. (http://bio-bigdata.hrbmu.edu.cn/CellMarker/CellMarkerSearch.jsp)")
            library(ggplot2)
        },
        #' @description
        #' Return the image result of the function viewMarker , which is a plotly object.
        #' If the function viewMarker has not been executed, it will return NULL.
        #' If the function viewMarker fails to execute successfully, it will return the result of the last successful execution. 
        getViewMarkerChartResult = function() {
            return (private$viewMarkerChartResult)
        },
        #' @description
        #' Return the table result of the function viewMarker , which is a data.frame object.
        #' If the function viewMarker has not been executed, it will return NULL.
        #' If the function viewMarker fails to execute successfully, it will return the result of the last successful execution.         
        getViewMarkerTableResult = function() {
            return (private$viewMarkerTableResult)
        },
        #' @description
        #' Save the image result of the function viewMarker.
        #' 
        #' @param filePath 
        #' A complete file path, but the file extension must be html.       
        saveViewMarkerChartResult = function(filePath) {
            private$saveChartResult(private$viewMarkerChartResult,filePath)          
        },
        #' @description
        #' Save the table result of the function viewMarker.
        #' 
        #' @param filePath 
        #' A complete file path, but the file extension must be csv or xlsx.         
        saveViewMarkerTableResult = function(filePath) {
            private$saveTableResult(private$viewMarkerTableResult,filePath)
        },   
        #' @description
        #' Draw a dynamic lattice diagram by R package plotly to show the query results of a specified marker in the CellMarker database.
        #' 
        #' @param symbol
        #' Seclect the symbol to query.Only one of the parameters symbol marker or geneid can be filled at a time.
        #' @param marker
        #' Seclect the marker to query.Only one of the parameters symbol marker or geneid can be filled at a time.
        #' @param geneid
        #' Seclect the geneid to query.Only one of the parameters symbol marker or geneid can be filled at a time.
        #' @param title_name
        #' Customize the title of the image.                  
        #' @param species
        #' Select the species to query. The value must be one of c('Human', 'Mouse', 'All').   
        #' @param cell_type
        #' Select the cell_type to query. The value must be one of c('Normal cell','Cancer cell','All'). 
        #' @param marker_source
        #' Select the marker_source to query. The value must be one of c('Experiment','Company','Review','Single-cell sequencing','All').   
        #' @param year_range
        #' Customize the year_range to query. The value must be either 'All' or an expression. If it is an expression, it must contain the term 'year'. For example, expressions could be 'year >= 2012', 'year <= 2011', or 'year >= 2010 && year <= 2020'.
        #' @param show_undefined
        #' A Boolean value to determine whether to display undefined tissue_class.
        #' @param show_filter
        #' A Boolean value to determine whether to display filter process.        
        viewMarker = function(  symbol = NULL,
                                marker = NULL,
                                geneid = NULL,
                                title_name = 'CellMarker_Result',
                                species = 'Human',
                                cell_type = "All",
                                marker_source = "All",
                                year_range = "All",
                                show_undefined = FALSE,
                                show_filter = FALSE  ) {
            cellMarkerRaw = private$cellMarkerRaw
            cellMarker = private$filterSpecies(cellMarkerRaw,species,show_filter)
            cellMarker = private$filterCellType(cellMarker,cell_type,show_filter)
            cellMarker = private$filterMarkerSource(cellMarker,marker_source,show_filter)
            cellMarker = private$filterYear(cellMarker,year_range,show_filter)
            cellMarker = private$filterUndefined(cellMarker,show_undefined,show_filter)
            cellMarker = private$filterQuery(cellMarker,symbol,marker,geneid,show_filter)  
            if(dim(cellMarker)[[1]] == 0){
                message("No result found")
            }else{
                tissue_class = cellMarker$tissue_class
                cell_name  = cellMarker$cell_name
                label = paste0(tissue_class,'__',cell_name)
                df = data.frame(tissue_class,cell_name,label)
                colnames(df) = c('tissueName','cellName','label')
                p = ggplot(df,aes(x=cellName,y=tissueName,label=label)) +
                    geom_count(aes(size = after_stat(n),color=after_stat(n))) + 
                    scale_color_gradient(low = "#95c8fd", high = "#072e9e") + 
                    theme_minimal() + 
                    labs(x = '', y = '', title = title_name) +
                    theme(axis.text.x = element_text(angle = 45, hjust = 0.9, vjust = 0.9)) + 
                    theme(axis.text.y = element_text(angle = 0, hjust = 0.9, vjust = 0.5)) + 
                    theme(plot.title = element_text(hjust = 0.5))
                p = plotly::ggplotly(p,dynamicTicks = T,tooltip = c("label","n"))      
                print(p)
                private$viewMarkerTableResult = cellMarker
                private$viewMarkerChartResult = p
            }
        },            
        #' @description
        #' Return the image result of the function viewCell , which is a plotly object.
        #' If the function viewCell has not been executed, it will return NULL.
        #' If the function viewCell fails to execute successfully, it will return the result of the last successful execution. 
        getViewCellChartResult = function() {
            return (private$viewCellChartResult)
        },
        #' @description
        #' Return the table result of the function viewCell , which is a data.frame object.
        #' If the function viewCell has not been executed, it will return NULL.
        #' If the function viewCell fails to execute successfully, it will return the result of the last successful execution.         
        getViewCellTableResult = function() {
            return (private$viewCellTableResult)
        },  
        #' @description
        #' Return the rank result of the function viewCell , which is a data.frame object.
        #' 
        #' @note
        #' If the function viewCell has not been executed, it will return NULL.
        #' If the function viewCell fails to execute successfully, it will return the result of the last successful execution.                  
        getViewCellRankResult = function() {
            return (private$viewCellRankResult)
        },    
        #' @description
        #' Save the image result of the function viewCell.
        #' 
        #' @param filePath 
        #' A complete file path, but the file extension must be html.          
        saveViewCellChartResult = function(filePath) {
            private$saveChartResult(private$viewCellChartResult,filePath)
        },  
        #' @description
        #' Save the table result of the function viewCell.
        #' 
        #' @param filePath 
        #' A complete file path, but the file extension must be csv or xlsx.             
        saveViewCellTableResult = function(filePath) {
            private$saveTableResult(private$viewCellTableResult,filePath)
        },             
        #' @description
        #' Save the rank result of the function viewCell.
        #' 
        #' @param filePath 
        #' A complete file path, but the file extension must be csv or xlsx.            
        saveViewCellRankResult = function(filePath) {
            private$saveTableResult(private$viewCellRankResult,filePath)
        },        
        #' @description
        #' Draw a WorkCloudChart by R package wordcloud2 to show the query results of a specified cell in the CellMarker database.
        #' @param tissue_class_number       
        #' Select the number of tissue_class to query.The value must be in range.
        #' @param tissue_type_number       
        #' Select the number of tissue_type to query.The value must be in range.
        #' @param cell_name_number       
        #' Select the number of cell_name to query.The value must be in range.
        #' @param species
        #' Select the species to query. The value must be one of c('Human', 'Mouse', 'All').   
        #' @param cell_type
        #' Select the cell_type to query. The value must be one of c('Normal cell','Cancer cell','All'). 
        #' @param marker_source
        #' Select the marker_source to query. The value must be one of c('Experiment','Company','Review','Single-cell sequencing','All').   
        #' @param year_range
        #' Customize the year_range to query. The value must be either 'All' or an expression. If it is an expression, it must contain the term 'year'. For example, expressions could be 'year >= 2012', 'year <= 2011', or 'year >= 2010 && year <= 2020'.
        #' @param show_undefined
        #' A Boolean value to determine whether to display undefined tissue_class.
        #' @param show_filter
        #' A Boolean value to determine whether to display filter process.           
        viewCell = function(    tissue_class_number = NULL,
                                tissue_type_number = NULL,
                                cell_name_number = NULL,
                                species = 'Human',
                                cell_type = "All",
                                marker_source = "All",
                                year_range = "All",
                                show_undefined = FALSE,
                                show_filter = FALSE ) {
            if ( !is.numeric(tissue_class_number) && !is.null(tissue_class_number) ){
                stop("tissue_class_number must be numeric or NULL")
            }      
            if ( !is.numeric(tissue_type_number) && !is.null(tissue_type_number) ){
                stop("tissue_type_number must be numeric or NULL")
            }   
            if ( !is.numeric(cell_name_number) && !is.null(cell_name_number) ){
                stop("cell_name_number must be numeric or NULL")
            }                                                         
            cellMarkerRaw = private$cellMarkerRaw
            cellMarker = private$filterSpecies(cellMarkerRaw,species,show_filter)
            cellMarker = private$filterCellType(cellMarker,cell_type,show_filter)
            cellMarker = private$filterMarkerSource(cellMarker,marker_source,show_filter)
            cellMarker = private$filterYear(cellMarker,year_range,show_filter)
            cellMarker = private$filterUndefined(cellMarker,show_undefined,show_filter)  
            cellMarker = private$chooseTissueClass(cellMarker,show_filter,tissue_class_number)
            cellMarker = private$chooseTissueType(cellMarker,show_filter,tissue_type_number)
            cellMarker = private$chooseCellName(cellMarker,show_filter,cell_name_number)            
            if(dim(cellMarker)[[1]] == 0){
                message("No result found")
            }else{
                Symbol = cellMarker$Symbol
                df = as.data.frame(table(Symbol))
                colnames(df) = c("word", "freq")
                df = df[order(df$freq,decreasing = TRUE),]                
                p = wordcloud2::wordcloud2( df, 
                                            size = 2, 
                                            gridSize = 0, 
                                            color = "random-light", 
                                            backgroundColor = "#FFFFFF", 
                                            shape = "circle")  
                print(p)
                private$viewCellRankResult = df
                private$viewCellTableResult = cellMarker
                private$viewCellChartResult = p
            }
        }
    ),
    private = list(
        cellMarkerRaw = NULL,
        viewMarkerTableResult = NULL,
        viewMarkerChartResult = NULL,
        viewCellTableResult = NULL,
        viewCellChartResult = NULL,
        viewCellRankResult = NULL,
        #' @export
        loadRawData = function() {
            data_path = system.file("data", "Rdata.rdb", package = "CellMarkerR")
            lazyLoad(sub(".rdb","",data_path))
            private$cellMarkerRaw = CellMarkerAllRawData
            rm(CellMarkerAllRawData)
        },
        #' @export
        showOptions = function(col,number=NULL) {
            if(!is.null(number)){
                options = unique(c(col,"All")) 
                choose_option = options[number] 
            }else {
                options = unique(c(col,"All")) 
                options = paste0(seq_along(options), ".", options)
                for (option in options){
                    message(option)
                }
                user_input = readline(prompt="Selection: ")
                options = unique(c(col,"All")) 
                choose_option = options[as.numeric(user_input)] 
            }
            return ( choose_option )      
        },
        #' @export
        saveTableResult = function(table,filePath) {
            if(!file.exists(dirname(filePath))){
                dir.create(dirname(filePath))
            }
            fileNameSplit = strsplit(basename(filePath) ,"[.]")[[1]]
            if ( fileNameSplit[length(fileNameSplit)] == "csv" ){
                write.csv(table, file = filePath, row.names = FALSE)                
            }else if ( fileNameSplit[length(fileNameSplit)] == "xlsx" ) {
                openxlsx::write.xlsx(table, file = filePath)
            }else {
                stop("File suffix name must be one of c('csv','xlsx')")
            }
        },
        #' @export
        saveChartResult = function(chart,filePath) {
            if(!file.exists(dirname(filePath))){
                dir.create(dirname(filePath))
            }
            fileNameSplit = strsplit(basename(filePath) ,"[.]")[[1]]
            if ( fileNameSplit[length(fileNameSplit)] == "html" ){
                htmlwidgets::saveWidget(chart,file = filePath,selfcontained = FALSE)          
            }else {
                stop("File suffix name must be html")
            }    
        },
        #' @export
        chooseTissueClass = function(cellMarker_table,show_filter,tissue_class_number) {
            choose_option = private$showOptions(cellMarker_table$tissue_class,tissue_class_number)  
            if(choose_option != 'All'){
                cellMarker_table = cellMarker_table[which(cellMarker_table$tissue_class == choose_option),]  
            }
            if (show_filter){
                print(glue::glue("The cellMarker table filter by tissue_class[{choose_option}] remain {dim(cellMarker_table)[[1]]} rows."))
            }
            return( cellMarker_table )
        },
        #' @export
        chooseTissueType = function(cellMarker_table,show_filter,tissue_type_number) {
            choose_option = private$showOptions(cellMarker_table$tissue_type,tissue_type_number)  
            if(choose_option != 'All'){
                cellMarker_table = cellMarker_table[which(cellMarker_table$tissue_type == choose_option),]  
            }
            if (show_filter){
                print(glue::glue("The cellMarker table filter by tissue_type[{choose_option}] remain {dim(cellMarker_table)[[1]]} rows."))
            }
            return( cellMarker_table )            
        },
        #' @export
        chooseCellName = function(cellMarker_table,show_filter,cell_name_number) {
            choose_option = private$showOptions(cellMarker_table$cell_name,cell_name_number)  
            if(choose_option != 'All'){
                cellMarker_table = cellMarker_table[which(cellMarker_table$cell_name == choose_option),]  
            }
            if (show_filter){
                print(glue::glue("The cellMarker table filter by cell_name[{choose_option}] remain {dim(cellMarker_table)[[1]]} rows."))
            }
            return( cellMarker_table )            
        },        
        #' @export          
        filterSpecies = function(cellMarker_table,species,show_filter) {
            if( species != 'Human' && species != 'Mouse' && species != 'All' ){
                stop("The species must be one of c('Human','Mouse','All')")
            }
            if ( species != 'All' ) {
                cellMarker_table = cellMarker_table[which(cellMarker_table$species == species),]
            }
            if (show_filter){
                print(glue::glue("The cellMarker table filter by species[{species}] remain {dim(cellMarker_table)[[1]]} rows."))
            }
            return( cellMarker_table )
        },
        #' @export
        filterCellType = function(cellMarker_table,cell_type,show_filter) {
            if( cell_type != 'Normal cell' && cell_type != 'Cancer cell' && cell_type != 'All' ){
                stop("The cell_type must be one of c('Normal cell','Cancer cell','All')")
            }
            if ( cell_type != 'All' ) {
                cellMarker_table = cellMarker_table[which(cellMarker_table$cell_type == cell_type),]
            }
            if (show_filter){
                print(glue::glue("The cellMarker table filter by cell_type[{cell_type}] remain {dim(cellMarker_table)[[1]]} rows."))
            }            
            return( cellMarker_table )
        },
        #' @export
        filterUndefined = function(cellMarker_table,show_undefined,show_filter) {
            if(!is.logical(show_undefined)){
                stop("The show_undefined must be a boolean value")
            }
            if ( !show_undefined ){
                cellMarker_table = cellMarker_table[which(cellMarker_table$tissue_class != "Undefined"),]
            }
            if (show_filter){
                print(glue::glue("The cellMarker table filter undefined remain {dim(cellMarker_table)[[1]]} rows."))
            }            
            return( cellMarker_table )
        },
        #' @export
        filterMarkerSource = function(cellMarker_table,marker_source,show_filter) {
            if( marker_source != 'Experiment' && marker_source != 'Company' && marker_source != 'Review' && marker_source != 'Single-cell sequencing' && marker_source != 'All' ){
                stop("The marker_source must be one of c('Experiment','Company','Review','Single-cell sequencing','All')")
            }
            if ( marker_source != 'All' ) {
                cellMarker_table = cellMarker_table[which(cellMarker_table$marker_source == marker_source),]
            }
            if (show_filter){
                print(glue::glue("The cellMarker table filter by marker_source[{marker_source}] remain {dim(cellMarker_table)[[1]]} rows."))
            }              
            return( cellMarker_table )
        },
        #' @export
        filterYear = function(cellMarker_table,year_range,show_filter) {
            if (!is.character(year_range) || length(year_range) != 1) {
                stop("The year_range must be a single string.")
            }
            if (grepl("year", year_range)) {
                cellMarker_table$year[is.na(cellMarker_table$year)] = 0
                outcome = eval(parse(text = gsub("year", "cellMarker_table$year", year_range)))
                cellMarker_table = cellMarker_table[outcome, ]
            }
            if (show_filter){
                print(glue::glue("The cellMarker table filter by year_range[{year_range}] remain {dim(cellMarker_table)[[1]]} rows."))
            }              
            return(cellMarker_table)
        },
        #' @export
        filterQuery = function(cellMarker_table,symbol,marker,geneid,show_filter) {
            if( all(is.null(symbol),is.null(marker),is.null(geneid)) ){
                stop("This function must provide a query")
            }else if( table(c(is.null(symbol),is.null(marker),is.null(geneid)))[[2]] != 2 ){
                stop("Only one query is need")
            }
            if( !is.null(symbol) ){
                cellMarker_table = cellMarker_table[which(toupper(cellMarker_table$Symbol) == toupper(symbol)),]
                query = symbol
            }else if ( !is.null(marker) ) {
                cellMarker_table = cellMarker_table[which(toupper(cellMarker_table$marker) == toupper(marker)),]
                query = marker
            }else if ( !is.null(geneid) ){
                cellMarker_table = cellMarker_table[which(toupper(cellMarker_table$GeneID) == toupper(geneid)),]
                query = geneid
            }
            if (show_filter){
                print(glue::glue("The cellMarker table filter by query[{query}] remain {dim(cellMarker_table)[[1]]} rows."))
            }   
            return(cellMarker_table)
        }
    )
)