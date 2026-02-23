## this function removes data based on conditions
## it returns a new object (with all data)

psytkRemoveData = function( psytkSurveyData , selection ){

    ## check if selection is as long as surveyData

    if ( length(selection) != psytkSurveyData$n ){
        stop("Your selection is not equal the number of items", call. = FALSE) # the period is part of the parameter
    }

    # TODO: listanswers?

    surveyobject      =  psytkSurveyData$surveyobject
    ID                =  psytkSurveyData$ID[!selection]
    IDcount           =  psytkSurveyData$IDcount[!selection]
    radioAnswers      =  psytkSurveyData$radioAnswers[!selection,]   
    radioScores       =  psytkSurveyData$radioScores[!selection,]       

    setAnswers        =  psytkSurveyData$setAnswers[!selection,]   
    
    dropAnswers       =  psytkSurveyData$dropAnswers[!selection,]   
    dropScores        =  psytkSurveyData$dropScores[!selection,]   

    scaleAnswers      =  psytkSurveyData$scaleAnswers[!selection,]      
    scaleScores       =  psytkSurveyData$scaleScores[!selection,]       

    checkAnswers      =  psytkSurveyData$checkAnswers[!selection,]          
    listAnswers       =  psytkSurveyData$listAnswers[!selection,]          

    rankAnswers       =  psytkSurveyData$rankAnswers[!selection,]       
    rangeAnswers      =  psytkSurveyData$rangeAnswers[!selection,]      
    textlineAnswers   =  psytkSurveyData$textlineAnswers[!selection,]   
    textboxAnswers    =  psytkSurveyData$textboxAnswers[!selection,]    
    startDate         =  psytkSurveyData$startDate[!selection]         
    startTime         =  psytkSurveyData$startTime[!selection]          
    startDateTime     =  psytkSurveyData$startDateTime[!selection]      
    country           =  psytkSurveyData$country[!selection]            

    times             =  psytkSurveyData$times[!selection,]
    duration          =  psytkSurveyData$duration[!selection]

    cgi               =  psytkSurveyData$cgi
    n                 =  sum(selection)

    for( i in names( cgi ) ){
        cgi[i] = cgi[i][!selection]
    }

    ## now remove the selected participants from the experiments if needed

    ## #####################################################################
    
    returnList = mget(c( # mget is like "list" but automatically names the containing variables
        "surveyobject"    , # you contain the survey structure, which you need for reporting
        "radioAnswers"    , 
        "radioScores"     , 
        "dropAnswers"     , 
        "checkAnswers"    , 
        "setAnswers"      ,
        "startDate"       ,
        "startTime"       ,
        "startDateTime"   ,
        "country"         ,
        "textlineAnswers" ,
        "textboxAnswers"  ,
        "rankAnswers"     ,
        "rangeAnswers"    ,
        "scaleAnswers"    ,
        "scaleScores"     ,
        "cgi"             ,
        "times"           ,
        "duration"         ,
        "n" , "ID" , "IDcount" ))

    for ( expname in psytkSurveyData$surveyobject$experiments ){

        e = paste( "expData",expname , sep="." )

        includeIDs = psytkSurveyData$ID[ !selection ] # we select these, then we can easily include it from rows in expdata.name

        tmpexp = psytkSurveyData[[ e ]] ## get the exp data
        tmpexp = tmpexp[ tmpexp[ , 1 ] %in% includeIDs , ] ## include data we want without excluded !selection

        returnList[[ e ]] = tmpexp
    }

    return( returnList )
}
