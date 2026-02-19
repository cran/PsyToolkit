## For outputing text tables whereby we align columns, we need some additional code
## x must be a data frame or matrix
## all columns will be separated properly
## for headers it uses the colnames
## header: if not null, it expects the names you want in the header line and must match number of columns in data
##         if null, it takes the header
##         if False, it does not show header or colnames
##         align = can be "L","C","R" or a vector for each

## this is an internal helper function just to space out strings for one row of the table
## if content is longer than wrap, it will add it to an additional row
## you can set wrap to large number to switch it offn

psytkTableRow = function(strings, width, align = "L" , padding = 0 , wrap=999) {

    ## first define the characters. For CRAN packages we need to use ASCII only
    
    boxTopLeft  = "\u250c"       # ┌
    boxTopRight = "\u2510"       # ┐
    boxBotLeft  = "\u2514"       # └
    boxBotRight = "\u2518"       # ┘
    boxJunctionLeft   = "\u251c" # ├
    boxJunctionRight  = "\u2524" # ┤
    boxJunctionTop    = "\u252C" # ┬
    boxJunctionBot    = "\u2534" # ┴
    boxJunction       = "\u253C" # ┼
    boxLineHorizontal = "\u2500" # ─
    boxLineVertical   = "\u2502" # │
    

  # 1. Ensure 'align' is the same length as the input strings
  if (length(align) == 1) {
    align = rep(align, length(strings))
  }
  
  # 2. Logic to format an individual cell
  format_cell = function(s, w, a) {
    # Calculate padding
    pad_total = w - nchar(s)
    if (pad_total < 0) return(substr(s, 1, w)) # Truncate if too long
    
    if (a == "L") {
      return(paste0(s, strrep(" ", pad_total)))
    } else if (a == "R") {
      return(paste0(strrep(" ", pad_total), s))
    } else if (a == "C") {
      left_pad  = floor(pad_total / 2)
      right_pad = pad_total - left_pad
      return(paste0(strrep(" ", left_pad), s, strrep(" ", right_pad)))
    } else {
      stop("Align must be 'L', 'R', or 'C'")
    }
  }
  
  # 3. Apply the formatting across the vectors
  # mapply is perfect here as it iterates over multiple vectors in parallel
  formatted_cells = mapply(format_cell, strings, width, align, USE.NAMES = FALSE)
  
  # 4. Wrap in UTF-8 pipes

  return(paste0(boxLineVertical, paste(formatted_cells, collapse = boxLineVertical ), boxLineVertical))
}

psytkTable = function(x, header = NULL , align = "L" , padding=2  , wrap = 20) {

    ## first define the characters. For CRAN packages we need to use ASCII only
    
    boxTopLeft  = "\u250c"       # ┌
    boxTopRight = "\u2510"       # ┐
    boxBotLeft  = "\u2514"       # └
    boxBotRight = "\u2518"       # ┘
    boxJunctionLeft   = "\u251c" # ├
    boxJunctionRight  = "\u2524" # ┤
    boxJunctionTop    = "\u252C" # ┬
    boxJunctionBot    = "\u2534" # ┴
    boxJunction       = "\u253C" # ┼
    boxLineHorizontal = "\u2500" # ─
    boxLineVertical   = "\u2502" # │



  df = as.data.frame(x, stringsAsFactors = FALSE)
  nCols = ncol(df)

  # Convert to character with NA replaced by "NA"
  char_df = as.data.frame(lapply(df, function(col) {
    v = as.character(col)
    v[is.na(v)] = "NA"
    v
  }), stringsAsFactors = FALSE)

  # Determine width for each column based on content + 2 for padding
  if ( is.null( header) ){
      colWidths = sapply(rbind(colnames(x),char_df), function(col) max(nchar(col), na.rm = TRUE)) + padding

  }else{
      if ( header[1] == F ) {
          colWidths = sapply(char_df, function(col) max(nchar(col), na.rm = TRUE)) + padding
      }else{
          colWidths = sapply(rbind(header,char_df), function(col) max(nchar(col), na.rm = TRUE)) + padding
      }
  }


  ## ############################################################################

  ## first we create basic templates for the various horizontal lines
  ## we know the total character string, fill it with horizontal, then later change some of these for corners etc

  l = sum(colWidths) + length(colWidths) + 1 
  topLine = rep( boxLineHorizontal , l ) 
  bottomLine = topLine
  centerLine = topLine

  topLine[1] = boxTopLeft
  topLine[l] = boxTopRight
  bottomLine[1] = boxBotLeft
  bottomLine[l] = boxBotRight
  centerLine[1] = boxJunctionLeft
  centerLine[l] = boxJunctionRight

  for ( i in 1:(nCols-1) ){
      z = cumsum(colWidths)[i] + i + 1
      topLine[z]    = boxJunctionTop
      bottomLine[z] = boxJunctionBot
      centerLine[z] = boxJunction
  }

  cat(paste(topLine,collapse=""),"\n")

  ## ############################################################################
  ## now header line
  ## header == NULL   : just columnnames
  ## header == F      : no header at all
  ## header == vector : alternative names

  if ( is.null(header) ){ # this means, we do the default columnnames
      cat( psytkTableRow( colnames(x) , colWidths , "L" , wrap = wrap ) , "\n" )
      cat(paste(centerLine,collapse=""),"\n")
  }else{
      if ( header[1] != FALSE ){
          cat( psytkTableRow( header , colWidths , "L" , wrap = wrap) , "\n" )
          cat(paste(centerLine,collapse=""),"\n")
      }
  }

  for( i in 1:dim(x)[1] ){
      cat( psytkTableRow( x[i,] , colWidths ) , "\n" )
  }

  ## ############################################################################

  cat( paste(bottomLine,collapse="") , "\n" )

}
