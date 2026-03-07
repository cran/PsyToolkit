## this function creates a spreadsheet of all data for convenience and exports as xlsx2 format
## exports to XLSX using openxlsx2
## if you choose labels=TRUE, then you get the labels instead of numbers for radio/check
## if you choose orderByType=TRUE, then you get them in order of type rather than order in the questionnaire.

psytkExport = function( surveydata , file = "exported_data.xlsx" , labels = FALSE , orderByType = FALSE){

    ## we create a workbook with different sheets
    ## sheet one called "survey"
    ## sheet two called "times" for the times per question
    ## sheet three and higher one for each experiment

    ## First we do SHEET 1 ################################################################################

    ## first create a new data frame with all types that are in use.
    tmp = surveydata$surveyobject$longoverview # this is a dataframe with 1) the item name (and subnames) 2) types 

    ## if you order by type, simply order by type
    if ( orderByType ) tmp = tmp[ order(tmp[,2] ) , ]

    ## start with empty data frame
    containerA = list() # answers
    containerS = list() # scores (even though only some contain scores)

    ## first we set the IDs. If ID1 and ID2 were used by user, make them different
    
    if ( "ID1" %in% tmp[,1] | "ID2" %in% tmp[,1] ){
        containerA[["PsyToolkitID1"]] = surveydata$ID
        containerA[["PsyToolkitID2"]] = surveydata$IDcount

        containerS[["PsyToolkitID1"]] = surveydata$ID
        containerS[["PsyToolkitID2"]] = surveydata$IDcount

    }else{
        containerA[["ID1"]] = surveydata$ID
        containerA[["ID2"]] = surveydata$IDcount

        containerS[["ID1"]] = surveydata$ID
        containerS[["ID2"]] = surveydata$IDcount
    }
    
    ## now create vectors one by one and then add to the dataframe
    columnWidths = list() # used for textboxes if any
    
    for ( i in 1:nrow(tmp) ){
        label = tmp[i,1]
        qtype = tmp[i,2]
    
        ## currently, only radio and scale can have both answers and scores
        
        if ( qtype == "radio"    ){
            containerA[[ label ]] = surveydata$radioAnswers[ , label ]
            containerS[[ label ]] = surveydata$radioScores[ , label ]

            if ( label %in% colnames(surveydata$radioAnswersOther) ){
                newLabel = paste( label , " (other)",sep="" )
                containerA[[ newLabel ]] = surveydata$radioAnswersOther[ , label ]
                containerS[[ newLabel ]] = surveydata$radioAnswersOther[ , label ]
            }

        }

        if ( qtype == "scale"    ){
            containerA[[ label ]] = surveydata$scaleAnswers[ , label ]
            containerS[[ label ]] = surveydata$scaleScores[ , label ]
        }

        ## for the rest, scores and answers are the same

        if ( qtype == "rank"    ){
            containerA[[ label ]] = surveydata$rankAnswers[ , label ]
            containerS[[ label ]] = surveydata$rankAnswers[ , label ]
        }

        if ( qtype == "check"    ){
            containerA[[ label ]] = surveydata$checkAnswers[ , label ]
            containerS[[ label ]] = surveydata$checkAnswers[ , label ]
        }
        
        if ( qtype == "range"    ){
            containerA[[ label ]] = surveydata$rangeAnswers[ , label ]
            containerS[[ label ]] = surveydata$rangeAnswers[ , label ]
        }

        
        if ( qtype == "textline" ){
            containerA[[ label ]] = surveydata$textlineAnswers[ , label ]
            containerS[[ label ]] = surveydata$textlineAnswers[ , label ]
        }

        if ( qtype == "list" ){
            containerA[[ label ]] = surveydata$listAnswers[ , label ]
            containerS[[ label ]] = surveydata$listAnswers[ , label ]
        }
        
        if ( qtype == "set"      ){
            containerA[[ label ]] = surveydata$setAnswers[ , label ] # there are only set answers, even though in spreadsheat, we work with idea of "scores"
            containerS[[ label ]] = surveydata$setAnswers[ , label ]
        }
        
        if ( qtype == "scale"    ){
            containerA[[ label ]] = surveydata$scaleAnswers[ , label ]
            containerS[[ label ]] = surveydata$scaleScores[ , label ]
        }

        if ( qtype == "textbox" ){ ## todo: need to replace ~ with newline if not yet done?
            containerA[[ label ]] = surveydata$textboxAnswers[ , label ]
            containerA[[ label ]] = gsub( "~~","\n", containerA[[ label ]] , fixed=TRUE )

            containerS[[ label ]] = surveydata$textboxAnswers[ , label ]
            containerS[[ label ]] = gsub( "~~","\n", containerA[[ label ]] , fixed=TRUE )
        }
    }

    outA = data.frame( containerA )
    outS = data.frame( containerS )

    ## find the colums in outA that need wrapping because they represent a textbox question
    ## note that tmp is a dataframe. The first two columns will be reserved for ID information (hence FALSE).
    ## the rest is simply based on outA, which contains all the types

    if ( any(tmp[,2]=="textbox") ){
        colsWithTextBox = which (  c( FALSE , FALSE , tmp[match( colnames(outA[3:ncol(outA)]) , tmp[,1] ),2]=="textbox" ))
    }else{
        colsWithTextBox = NULL
    }

    ## ############################################################################
    ## Now create workbook and sheets for output.
    ## In principle, this can be done with other packages as well, but
    ## readODS does currently not allow textwrapping
    ## ############################################################################

    workbook = wb_workbook()

    workbook$set_base_font(font_color = wb_color(auto = TRUE)) # the automatic font style/color

    ## sheet 1: the main survey answers

    workbook$add_worksheet("Scores")
    workbook$add_data(sheet="Scores",x=outS,na="")

    workbook$add_worksheet("Raw answers")
    workbook$add_data(sheet="Raw answers",x=outA,na="")
    
    ## sheet 1 textwrapping if needed)

    ## the first column (with long ID) should be set to autofit so that it fits nicely
    ## all to autofit except the ones with autofit
    workbook$set_col_widths(sheet="Raw answers",cols=1:ncol(outA),widths="auto")
    workbook$set_col_widths(sheet="Scores",cols=1:ncol(outS),widths="auto")

    ## align top vertically by default
    workbook$add_cell_style( sheet="Raw answers",dims=wb_dims(cols=1:ncol(outA),rows=1:nrow(outA) ),apply_alignment=TRUE,vertical="top")
    workbook$add_cell_style( sheet="Scores",dims=wb_dims(cols=1:ncol(outS),rows=1:nrow(outS) ),apply_alignment=TRUE,vertical="top")

    ## now text-wapping and less space for the textbox columns
    targetDims = wb_dims( cols=colsWithTextBox,rows=2:(nrow(outA)+1) )
   
    if ( length( colsWithTextBox ) > 0 ){
        workbook$set_col_widths( sheet="Raw answers",cols=colsWithTextBox,widths=50 ) # 50 is in their units
        workbook$set_col_widths( sheet="Scores",cols=colsWithTextBox,widths=50 ) # 50 is in their units
        workbook$add_cell_style( sheet="Raw answers",dims=targetDims,wrap_text=TRUE)
        workbook$add_cell_style( sheet="Scores",dims=targetDims,wrap_text=TRUE)
    }

    ## sheet 2: times 

    workbook$add_worksheet("Times")
    workbook$add_data(sheet="Times",x = as.data.frame( surveydata$times ),na="")

    ## sheet 3: general survey time (TODO: Need to add duration, start and end time, client time)

    tmp = data.frame( serverStartDate = surveydata$startDate , serverStartTime = surveydata$startTime , serverEndDateTime = surveydata$endDateTime , clientStartDateTime = surveydata$clientDateTime, durationMinutes = surveydata$duration )
    workbook$add_worksheet("More times")
    workbook$add_data(sheet="More times",x = tmp,na="")
    workbook$set_col_widths(sheet="More times",cols=1:ncol(tmp),widths="auto")

    ## sheet 4: general HTML: cgi, country, widht/height, agent/browser

    tmp = list()

    if ( length(surveydata$version) > 0 ) tmp[["version"]] = surveydata$version
    if ( length(surveydata$test) > 0 ) tmp[["test"]] = surveydata$test
    if ( length(surveydata$country) > 0 ) tmp[["country"]] = surveydata$country
    if ( length(surveydata$endcode) > 0 ) tmp[["endcode"]] = surveydata$endcode
    tmp[["screenDimension"]]=surveydata$screenDimension
    # tmp[["mobile"]]=surveydata$mobile
    tmp[["screenOrientation"]]=surveydata$screenOrientation

    tmp2 = data.frame( tmp )

    workbook$add_worksheet("Extra")
    workbook$add_data(sheet="Extra",x = as.data.frame( tmp2 ),na="")
    workbook$set_col_widths(sheet="Extra",cols=1:ncol(tmp2),widths="auto")

    ## sheet 5: optional sep: items

    ## sheet 6: optional cgi vars

    if ( length( surveydata$cgi ) > 0 ){
        tmp = data.frame( surveydata$cgi )

        # in case prolific data is in it, protect and remove PROLIFIC_PID
        tmp =tmp[, !grepl("prolific_pid" , colnames(tmp), ignore.case = TRUE), drop = FALSE] # remove any column with "participant" in it (Prolific)
        tmp =tmp[, !grepl("participantid", colnames(tmp), ignore.case = TRUE), drop = FALSE] # remove any column with "participant" in it (CloudResearch)

        workbook$add_worksheet("cgi")
        workbook$add_data(sheet="cgi",x = tmp,na="")
        workbook$set_col_widths(sheet="cgi",cols=1:ncol(tmp),widths="auto")
    }

    ## Check Prolific Data and protect from release participant ID

    if ( !is.null( surveydata$PRData )){
        tmp = data.frame( surveydata$PRData )
        tmp =tmp[, !grepl("participant", colnames(tmp), ignore.case = TRUE), drop = FALSE] # remove any column with "participant" in it
        sheetname = "Prolific Demographics"
        workbook$add_worksheet(sheetname)
        workbook$add_data(sheet=sheetname,x = tmp ,na="")
        workbook$set_col_widths(sheet=sheetname,cols=1:ncol(tmp),widths="auto") # in exps, we want the ID in column 1 shown completely
    }

    ## Check Prolific Data and protect from release participant ID

    if ( !is.null( surveydata$CRData )){
        tmp = data.frame( surveydata$CRData )
        tmp =tmp[, !grepl("participant", colnames(tmp), ignore.case = TRUE), drop = FALSE] # remove any column with "participant" in it
        sheetname = "CloudResearch Demographics"
        workbook$add_worksheet(sheetname)
        workbook$add_data(sheet=sheetname,x = tmp ,na="")
        workbook$set_col_widths(sheet=sheetname,cols=1:ncol(tmp),widths="auto") # in exps, we want the ID in column 1 shown completely
    }
  
    ## sheet 6 to n: experiment data

    if ( !is.null( surveydata$surveyobject$experiments) ){
        for ( j in surveydata$surveyobject$experiments ){
            expdata = get( "surveydata" )[[ paste( "expData",j,sep=".")]]
            sheetname = paste("Exp",j,sep=" ")
            workbook$add_worksheet(sheetname)
            workbook$add_data(sheet=sheetname,x = as.data.frame( expdata ),na="")
            workbook$set_col_widths(sheet=sheetname,cols=1,widths="auto") # in exps, we want the ID in column 1 shown completely
        }
    }

    workbook$save( file )
}
