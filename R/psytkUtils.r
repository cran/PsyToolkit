## ##########################################################################################
##
## simple convenience functions:
## round0p(n,total)              rounds percentage with percentage sign just for convenience
## catn( strings etc )           similar to cat but adds newline
## hr()
## isNumberString                does a string represent a number
## dropNA                        removes NA values from a vector (better than na.omit)
## reportRange( numbers )
## psytkReadTable( filename )
## psytkTable( col1, col2, ... ) for outputing aligned table
##
## functions to extract data from data frames based on participants, etc:
## psytkExtractNum(...)
## psytkExtractPerc(...)
## psytkExtractCount(...)
## psytkExtractTail(...)
##
## ###########################################################################################

## round to a percentage based on an n and a total number (practical for error reporting)
round0p = function(n,total){ return( paste(round( n/total*100 , digits=0 ),"%",sep="")) }

## write out text ending in newline
catn = function(...){cat(fill=T,...)}

## reportRange is for a human readable range report (e.g., "10 to 20")

reportRange=function(data){ return( paste(min(data,na.rm=TRUE),"to",max(data,na.rm=TRUE)))}

## psytkReadTable
## read in a file. It makes sure it can deal with unequal columns and makes it so that the output has equal columns

psytkReadTable = function( filename ){
    lines = readLines( filename )
    ## ll reports for each line of the file how many items there are
    ll = as.numeric(sapply(lines, function(line) length(strsplit(line, "\\s+")[[1]]))) # split by one or more spaces
    if ( length(unique(ll))==1 ){
        return( read.table( filename , header = F ))
    }else{
        return( read.table( filename, fill = TRUE, col.names = paste0("V", 1:max(ll)), header = FALSE) )
    }
}

## extract data from data.frame such that get one line per participant
## this is for experiment files where one participant has many trials (one trial per line),
## and where the data frame has lots of participants

psytkExtractNum = function( data , dependent , names , conditions , f = mean ){

    nparticipants = length( unique( data[,"ID1"] ) )
    participants = unique( data[,"ID1"] )

    out = NULL

    for( i in 1:length(participants) ){
        tmp = rep( NA , length( names ))
        for ( j in 1:length(names) ){
            tmp[ j ] = f( data[ conditions[,j] & data[,"ID1"]==participants[i] , dependent ] )
        }
        out = rbind( out,tmp )
    }
    colnames(out)=names
    rownames(out)=participants
    return( out )
}

## this is to extract a percentage of trials for a participant

psytkExtractPerc = function( data , dependent , names , conditions , f = mean ){

    nparticipants = length( unique( data[,"ID1"] ) )
    participants = unique( data[,"ID1"] )

    out = NULL

    for( i in 1:length(participants) ){
        tmp = rep( NA , length( names ))
        for ( j in 1:length(names) ){
            tmp[ j ] = sum( conditions[,j] & data[,"ID1"]==participants[i] & dependent ) / sum( conditions[,j] & data[,"ID1"]==participants[i] ) * 100
        }
        out = rbind( out,tmp )
    }
    colnames(out)=names
    rownames(out)=participants
    return( out )
}


## this is to extract a count of trials for a participant given a condition

psytkExtractCount = function( data , names , conditions ){

    nparticipants = length( unique( data[,"ID1"] ) )
    participants = unique( data[,"ID1"] )

    out = NULL

    for( i in 1:length(participants) ){
        tmp = rep( NA , length( names ))
        for ( j in 1:length(names) ){
            tmp[ j ] = sum( conditions[,j] & data[,"ID1"]==participants[i] )
        }
        out = rbind( out,tmp )
    }
    colnames(out)=names
    rownames(out)=participants
    return( out )
}

## extract the last line of the data, and then dependent is the name of the column

psytkExtractTail = function( data , dependent  ){

    nparticipants = length( unique( data[,"ID1"] ) )
    participants = unique( data[,"ID1"] )

    out = rep( NA , nparticipants )

    for( i in 1:length(participants) ){
        tmp = data[ data[,"ID1"]==participants[i] , ]
        out[i] = as.numeric( tail( tmp , n = 1 )[[dependent]])  ## double makes sure not to return list or df
    }
    return( out )
}

## psytkExtractFromExperiment extracts experiment data from one experiment and can also remove the ID information (in first two columns)
## dataset : the psytk dataset you got with psytkReadData
## expname : the name of the experiment as listed in your survey.txt
## id1     : you can select the long UUID of a participant. You can only list one here
## id2     : you can select the count number (the second ID). This will be a number between 1 and however many participants there are
## removeID: default is True. If false, the first two ID columns are not removed
## TIP: For id1 or id2, just select one of the two

psytkExtractFromExperiment = function( dataset , expname , id = 1 , removeID = TRUE ){

    tmp = paste0( "expData.",expname )

    tmp2 = dataset[[ tmp ]]

    # note id can be a digit (by default 1), or a string

    if ( is.character(id) ){
        tmp2 = tmp2[ tmp2[,"ID1"]==id , ]
    }else{
        if ( is.numeric(id) ){
            tmp2 = tmp2[ tmp2[,"ID2"]==id , ]
        }
    }
    
    if ( removeID ){
        tmp2 = tmp2[, -c(1,2) ] ## remove the first two columns, which are the participant ID data
    }

    return( tmp2 )
}



## get real width (works only really on Linux/Mac, if on windows, will return print width)

getRealTerminalWidth = function() {
  out = tryCatch(system("stty size", intern = TRUE), error = function(e) NA)
  if (is.na(out)) return(getOption("width"))
  
  cols = as.integer(strsplit(out, " ")[[1]][2])
  if (is.na(cols)) getOption("width") else cols
}

## provide horizontal separating line for reports
## hr = function( width = NA ){
##     if ( is.na(width) ){
##         cat(strrep("-", getRealTerminalWidth() - 1 ), "\n")
##     }else{
##         cat(strrep("-", width), "\n")
##     }
## }

hr = function(){
    cat("------------------------------------------------------------------------------\n")
}



## check if a string contains a number (and nothing else, works for integers, negatives etc)
isNumberString = function(x){
    return( grepl("^[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?$", x) )
}

## removes NA values from a vector cleanly, there is no built-in function for this in Rbase
dropNA = function(x){ return( x[!is.na(x)] ) }
