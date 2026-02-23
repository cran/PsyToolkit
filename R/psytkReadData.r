psytkReadData = function( surveyOrDir , psytkAllowRepeatExp = TRUE , verbose = TRUE ){

    warnings = NULL # this is a vector of strings

    ## -- first check if surveyobject is a foldername or a parsed survey

    ## surveyOrDir can either be a parsed survey (then it is a list)
    ## or alternatively a foldername (string), in which case it is not a list

    surveyobject = NULL
    if ( is.list( surveyOrDir )){
        surveyobject = surveyOrDir # it does not need to be parsed anymore, this is the parsed object
    }else{
        ## -- assume surveyOrDir is the data folder

        if ( dir.exists( surveyOrDir ) ){
            surveyobject = psytkParseSurvey( surveyOrDir )
        }
    }

    ## note, the surveyobject now contains all the questionnames and types etc

    ## -- some preprocessing --------------------------------------------------------

    ## this contains subdirs for both survey (questionnaire) and experiment data
    psytkSurveyDir = surveyobject$datadir

    psytkSurveyFilesDir = file.path( psytkSurveyDir , "survey_data" )
    psytkExperimentDir = file.path( psytkSurveyDir , "experiment_data" )

    psytkSurvey = surveyobject$overview
    psytkExperiments = surveyobject$experiments

    ## -- create empty placeholders if items do not exist -------------------------
    
    emptyMatrix     = matrix( nrow = 0 , ncol = 1 ) ## needed as placeholders
    checkAnswers    = emptyMatrix
    sepAnswers      = emptyMatrix
    rankAnswers     = emptyMatrix
    rangeAnswers    = emptyMatrix
    scaleAnswers    = emptyMatrix
    textlineAnswers = emptyMatrix # problem is that this can also contain strings, so needs some work
    textboxAnswers  = emptyMatrix
    listAnswers     = emptyMatrix
    times           = emptyMatrix # not sure we need this, but it is good to know what we later need
    scaleScores     = emptyMatrix # scores are for scales,radio,check,drop
    duration        = 0 # will containt duration of survey from start to end
    screenDimension   = emptyMatrix # the screen width/height
    screenOrientation = NULL   # landscape vs portrait

    endcode         = NA # only if needed will contain value

    ## ----------------------------------------------------------------------------

    psytkRadioQuestions    = psytkSurvey[ psytkSurvey[,"types"]=="radio",    ]
    psytkDropQuestions     = psytkSurvey[ psytkSurvey[,"types"]=="drop",     ] # they are essentially also radio questions
    psytkListQuestions     = psytkSurvey[ psytkSurvey[,"types"]=="list",     ]
    psytkSetQuestions      = psytkSurvey[ psytkSurvey[,"types"]=="set",      ]
    psytkTextlineQuestions = psytkSurvey[ psytkSurvey[,"types"]=="textline", ]
    psytkTextboxQuestions  = psytkSurvey[ psytkSurvey[,"types"]=="textbox",  ]
    psytkCheckQuestions    = psytkSurvey[ psytkSurvey[,"types"]=="check",    ]
    psytkScaleQuestions    = psytkSurvey[ psytkSurvey[,"types"]=="scale",    ]
    psytkRangeQuestions    = psytkSurvey[ psytkSurvey[,"types"]=="range",    ]

    ## go through all the survey files in the data directory

    tmpFiles   = list.files( path = file.path( psytkSurveyDir , "survey_data" ) , pattern="s.*.txt" )
    psytkFiles = NULL

    ## check psytkFiles for incomplete ones, only keep complete ones currently

    for( i in tmpFiles ){
        tmp = readLines( file.path( psytkSurveyFilesDir , i ))
        if ( substring(tail(tmp,n=1),1,3) == "end" ) psytkFiles=c(psytkFiles,i)
    }

    ## now psytkFiles only contains complete files

    ## ###################################################################################

    ID            = substr(psytkFiles, 3, 38) # gsub( "-","", substr(psytkFiles, 3, 38) ) ## participant IDs
    n             = length(psytkFiles) ## number total participants included incomplete files, this can later be adjusted
    IDcount       = 1:n

    radioAnswers           = matrix( nrow = n , ncol=dim(psytkRadioQuestions)[1] )
    colnames(radioAnswers) = psytkRadioQuestions[,1]
    radioScores            = radioAnswers

    radioAnswersOther      = matrix( nrow = n , ncol=dim(psytkRadioQuestions)[1] ) ## currently there are too many but during reading can remove rows unneeded
    colnames(radioAnswersOther) = psytkRadioQuestions[,1]

    # note that drop is dropdown menues, they are otherwise the same as radio (you can choose one out of many)
    dropAnswers           = matrix( nrow = n , ncol=dim(psytkDropQuestions)[1] )
    colnames(dropAnswers) = psytkDropQuestions[,1]
    dropScores            = dropAnswers

    listAnswers           = matrix( nrow = n , ncol=dim(psytkListQuestions)[1] )
    colnames(listAnswers) = psytkListQuestions[,1]
    listScores            = listAnswers

    setAnswers             = matrix( nrow = n , ncol=dim(psytkSetQuestions)[1] ) # note, for set there are only answers (in survey file "a: <number>"
    colnames(setAnswers)   = psytkSetQuestions[,1]

    version                = rep( NA , n ) # PsyToolkitversion used
    test                   = rep( FALSE , n )  # if survey was run as test, by default assume it is false
    startDate              = rep( NA , n ) 
    startTime              = rep( NA , n )
    startDateTime          = rep( NA , n ) # simply Date + Time for easier sorting
    endDateTime            = rep( NA , n ) # simply Date + Time for easier sorting
    clientDateTime         = rep( NA , n ) # simply Date + Time for easier sorting
    country                = rep( NA , n )
    duration               = rep( NA , n ) # calculated by subtracting endtime - starttime
    welcomeScreenTime      = rep( NA , n ) # calculated by subtracting endtime - starttime

    ## the survey data can contain cgi data, these are variables only
    ## in the survey data and not in the survey.txt. We simply use a
    ## list called "cgi" to store this. As soon as we encounter a new
    ## cgi variable (we do not know the names at this point, we add a
    ## character with n entries and then fill it).

    cgi = list()

    screenDimension = matrix( nrow = n , ncol = 2 )
    colnames(screenDimension)=c("w","h")
    
    endcode = rep( "" , n ) # could be better if simply null if there are none later?


    ## Now create an object for the times
    ## we need the items that have a time (all minus "set" and "jump")

    tmpTimesNames = psytkSurvey[psytkSurvey$types!="set" & psytkSurvey$types!="jump",]$labels
    times = matrix( nrow = n , ncol = length(tmpTimesNames) )
    colnames( times ) = tmpTimesNames

    ## #############################################################################
    ## Now create special matrix for textline questions
    ## Textlines can have multiple items
    ## textlineAnswers are TextLine answers
    ## for each textline question, there can be more than one answer
    ## the total answer depends on the number of items
    ## 

    tmpTotalAnswers = sum( psytkSurvey[ psytkSurvey[,"types"]=="textline" , "items" ] )

    ## now we are going to create a vector of column names. For questions
    ## with more than 1 item, we will use .1, .2, etc
    if ( tmpTotalAnswers > 0 ){
        tmptable    = psytkSurvey[ psytkSurvey[,"types"]=="textline", ]
        tmpcolnames = NULL
        for( i in 1:dim(tmptable)[1] ){
            if ( tmptable[i,"items"] == 1 ){
                tmpcolnames=c( tmpcolnames , tmptable[i,"labels"] )
            }else{
                for( j in 1:tmptable[i,"items"] ){
                    tmpcolnames=c( tmpcolnames , paste( tmptable[i,"labels"] , j , sep="." ) )
                }
            }
        }
        textlineAnswers              = as.data.frame( matrix( nrow = n , ncol=length(tmpcolnames) ) ) # because it can contain strings as well
        colnames(textlineAnswers )   = tmpcolnames
    }
    ## #############################################################################
    ## Now create special matrix for textbox questions
    ## Textbox can have multiple items
    ## textboxAnswers are Textbox answers
    ## for each textbox question, there can be more than one answer
    ## the total answer depends on the number of items

    tmpTotalAnswers = sum( psytkSurvey[ psytkSurvey[,"types"]=="textbox" , "items" ] )

    ## now we are going to create a vector of column names. For questions
    ## with more than 1 item, we will use .1, .2, etc

    if ( tmpTotalAnswers > 0 ){
        tmptable    = psytkSurvey[ psytkSurvey[,"types"]=="textbox", ]
        tmpcolnames = NULL
        for( i in 1:dim(tmptable)[1] ){
            if ( tmptable[i,"items"] == 1 ){
                tmpcolnames=c( tmpcolnames , tmptable[i,"labels"] )
            }else{
                for( j in 1:tmptable[i,"items"] ){
                    tmpcolnames=c( tmpcolnames , paste( tmptable[i,"labels"] , j , sep="." ) )
                }
            }
        }

        textboxAnswers            = matrix( nrow = n , ncol=length(tmpcolnames) )
        colnames(textboxAnswers ) = tmpcolnames
    }

    ## #############################################################################
    ## Now create special matrix for holding check questions
    ## Check question can have multiple items, so we need to have spaceds

    ## TODO: This can now be done simpler, as we know from
    ## psytkSublabels what we needed here

    if ( sum(psytkSurvey[,"types"]=="check") > 0 ){
        tmptable    = psytkSurvey[ psytkSurvey[,"types"]=="check", ]
        tmpcolnames = NULL
        for( i in 1:dim(tmptable)[1] ){
            if ( tmptable[i,"items"] == 1 ){
                tmpcolnames=c( tmpcolnames , tmptable[i,"labels"] )
            }else{
                for( j in 1:tmptable[i,"items"] ){
                    tmpcolnames=c( tmpcolnames , paste( tmptable[i,"labels"] , j , sep="." ) )
                }
            }
        }

        checkAnswers             = matrix( nrow = n , ncol=length(tmpcolnames) )
        colnames(checkAnswers )  = tmpcolnames
    }

    ## #############################################################################
    ## Now create special matrix for holding rank questions
    ## For each item, you get a rank score 1 to the items in the question

    if ( sum(psytkSurvey[,"types"]=="rank") > 0 ){
        tmptable    = psytkSurvey[ psytkSurvey[,"types"]=="rank", ]
        tmpcolnames = NULL
        for( i in 1:dim(tmptable)[1] ){
            for( j in 1:tmptable[i,"items"] ){
                tmpcolnames=c( tmpcolnames , paste( tmptable[i,"labels"] , j , sep="." ) )
            }
        }

        rankAnswers             = matrix( nrow = n , ncol=length(tmpcolnames) )
        colnames(rankAnswers )  = tmpcolnames
    }

    ## #############################################################################
    ## Now create special matrix for holding range questions
    ## For each item, you get a numerica score

    if ( sum(psytkSurvey[,"types"]=="range") > 0 ){
        tmptable    = psytkSurvey[ psytkSurvey[,"types"]=="range", ]
        tmpcolnames = NULL
        for( i in 1:dim(tmptable)[1] ){
            for( j in 1:tmptable[i,"items"] ){
                tmpcolnames=c( tmpcolnames , paste( tmptable[i,"labels"] , j , sep="." ) )
            }
        }

        rangeAnswers            = matrix( nrow = n , ncol=length(tmpcolnames) )
        colnames(rangeAnswers ) = tmpcolnames
    }

    ## Now create special matrix for scale questions (similar to check questions)
    ## Scale question can have multiple items, with both answers and scores

    if ( sum(psytkSurvey[,"types"]=="scale") > 0 ){
        tmpcolnames = NULL
        for( i in 1:dim( psytkScaleQuestions )[1] ){
            if ( psytkScaleQuestions[i,"items"] == 1 ){
                tmpcolnames=c( tmpcolnames , psytkScaleQuestions[i,"labels"] )
            }else{
                for( j in 1:psytkScaleQuestions[i,"items"] ){
                    tmpcolnames=c( tmpcolnames , paste( psytkScaleQuestions[i,"labels"] , j , sep="." ) )
                }
            }
        }

        scaleAnswers             = matrix( nrow = n , ncol=length(tmpcolnames) )
        colnames(scaleAnswers )  = tmpcolnames
        scaleScores              = scaleAnswers
    }

    ## These values simply will have 1 or 0. The actual other answers are
    ## not yet coded here.

    ## ##############################################################################
    ## READ FILES
    ## ##############################################################################

    currentLabel  = ""
    currentType   = ""
    currentAnswer = ""
    currentTime   = "" # current answer time (T:)

    ## for textline and check questions, you can have multiple answers
    ## this variable keeps track how many a: lines there have been
    ## we have this separately for a: and s: lines
    currentQuestionItemCounterA = 0
    currentQuestionItemCounterS = 0

    if ( verbose  ){ message("Now reading questionnaire data") }

    includeItem = FALSE # this is a helper variable we set to TRUE
                        # when a new label is found but to FALSE if a
                        # label occurs in the data file but not in the
                        # survey.txt

    for( i in 1:length(psytkFiles) ){
        labelCounter = 0

        tmp = readLines( file.path( psytkSurveyFilesDir , psytkFiles[i] ))

        if ( verbose ) { message("\nStart reading data file:",psytkFiles[i]) }

        for ( l in 1:length(tmp) ){ # new in 1.0.4
            line = tmp[l]

            words = strsplit( line , " " )[[1]]
            if ( words[1] == "server-time:" & length(words) == 2  ){
                tmptime = strsplit( words[2] , "-" )[[1]]
                startDate[i] = paste( tmptime[1] , tmptime[2] , tmptime[3] , sep="-" )
                startTime[i] = paste( tmptime[4] , tmptime[5] , sep=":" )
                startDateTime[i] = paste( startDate[i] , startTime[i] )
            }

            if ( words[1] == "version:" & length(words) == 2  ){ # PsyToolkit version used
                version[i] = words[2]
            }

            if ( words[1] == "client-time:" & length(words) == 2  ){
                tmptime = strsplit( words[2] , "-" )[[1]]
                tmpDate = paste( tmptime[1] , tmptime[2] , tmptime[3] , sep="-" )
                tmpTime = paste( tmptime[4] , tmptime[5] , sep=":" )
                clientDateTime[i] = paste( tmpDate , tmpTime )
            }

            if ( words[1] == "psytest:" & length(words) == 2  ){
                if ( words[2] == "true" ) test[i] = TRUE
            }

            if ( words[1] == "end" & length(words) == 2 & l==length(tmp) ){ # this is the last line of the file with endtime
                tmptime = strsplit( words[2] , "-" )[[1]]
                endDate = paste( tmptime[1] , tmptime[2] , tmptime[3] , sep="-" ) # we do not save this separately
                endTime = paste( tmptime[4] , tmptime[5] , sep=":" )  # we do not save this separately
                endDateTime[i] = paste( endDate , endTime ) # we save just this and can be used for calculating duration
                duration[i] = as.numeric( strptime( endDateTime[i], "%Y-%m-%d %H:%M") - strptime( startDateTime[i], "%Y-%m-%d %H:%M") , units = "mins" )
            }

            if ( words[1] == "country:" & length(words) > 1  ){
                country[i] = substr( line , 10 , nchar(line) )
            }

            if ( words[1] == "endcode:" & length(words) > 1  ){
                endcode[i] = words[2]
            }

            if ( words[1] == "cgi:" ){
                tmpCgi = words[2]
                ## first check if for this cgi variable there already is a list entry in cgi
                if ( tmpCgi %in% names(cgi) ){
                    ## print(names(cgi))
                    ## the list item exists, and we just store the value
                    cgi[[tmpCgi]][i]=words[3]
                }else{
                    ## we create the cgi variable in the list "cgi" and the store the value
                    cgi[[tmpCgi]] = character(n)
                    cgi[[tmpCgi]][i]=words[3]
                }
            } 

            if ( words[1] == "screen_width:" ){
                if ( length(words) > 1 ){
                    screenDimension[i,"w"] = as.numeric(words[2])
                }
            }
            if ( words[1] == "screen_height:" ){
                if ( length(words) > 1 ){
                    screenDimension[i,"h"] = as.numeric(words[2])
                }
            }

            if ( words[1] == "l:" ){

                includeItem = TRUE ## we need to set this flag. We set it to FALSE if the question never occurs in the survey.txt
                
                currentQuestionItemCounterA = 0 ## reset this to zero for each start of question parsing
                currentQuestionItemCounterS = 0 ## reset this to zero for each start of question parsing
                ## now process previous question's data

                ## Now check if this label actually exists in
                ## survey.txt. This can happen if people used old test
                ## data. If this is the case, the whole datafile
                ## should either be discarded or those items should be
                ## skipped at least

                if ( words[2] %in% surveyobject$overview[,1] ){

                    labelCounter = labelCounter + 1
                    ## if ( labelCounter > 1 & currentType == "radio" ){
                    ##                     # cat("Answer was ",currentAnswer,"\n")
                    ## }
                    
                    ## Now the new label
                    currentLabel = words[2]
                    if ( verbose  ){ message("Found label",currentLabel) }

                }else{

                    ## there are two reasons why label might not be found
                    ## 1. The survey was changed
                    ## 2. A set item was added via "var out" by an experiment
                    ## For option 2, we need to add the item, but for that, we need checkif the next line contains "t: set"

                    ## first we need to know if there is a nextline, and if so, what it contains
                    if ( l < length(tmp) ){
                        nextline = tmp[l+1]
                        if ( nextline == "t: set" ){ ## in this case, we need to add the variable to overview and to setAnswers

                            ## note for the rbind, the surveyobject$overview has 5 columns
                            surveyobject$overview = rbind( surveyobject$overview , c( words[2] , "set" , 1 , "from experiment" , NA ) ) ## add at bottom of overview the new set question

                            newColnames = c( colnames( setAnswers ) , words[2] ) ## create for setAnswers a new set of columns with the new label
                            setAnswers = cbind( setAnswers , rep( NA , n ) ) ## add the variable to setAnswers, for starters it is empty
                            colnames(setAnswers)=newColnames ## set the colnames, this is important, because these are needed for setting the answer correctly

                            currentLabel = words[2]
                            if ( verbose  ){ message("Add label",currentLabel,"from experiments var out set") }
                        }else{

                            tmpWarning = paste("Label",words[2],"does not exist in survey.txt and this question item will be ignored. This is because of survey file:", psytkFiles[i],". This can happen if you changed your survey but you did not delete data. Or maybe the variable was generated by experiments.")
                            warnings = c(warnings,tmpWarning) 
                            
                            if ( verbose  ){
                                message(tmpWarning)
                            }
                            
                            includeItem = FALSE # this makes that the code below can know that the label does not exist

                        }
                    }

                }
            }

            if ( words[1] == "t:" & includeItem ){
                currentType = words[2]
            }

            ## if ( words[1] == "o:" ){ # we code this here as we do not need this in parsePsytk, we need to read in files later, but here we determine this
            ##     if ( length(words) > 1 ){
            ##         if ( words[2] == "sep" ) currentIsSep = T
            ##     }
            ## }

            if ( words[1] == "T:" & includeItem ){ 
                ## times are not stored for jump and set, so here we simply check if the column exists
                if ( currentLabel %in% colnames(times) ) times[ i , currentLabel ] = as.numeric( words[2] ) # answertime
            }

            if ( words[1] == "a:" & includeItem ){

                currentQuestionItemCounterA = currentQuestionItemCounterA + 1

                if ( length(words) > 1 ){
                    currentAnswer = paste(words[2:length(words)],collapse=" ")
                }else{ ## for check questions, you can have nothing, which means it was not checked
                    currentAnswer = ""
                }
                
                ## SET QUESTION #########################################
                if ( currentType == "set" ){
                    setAnswers[ i , currentLabel ] = as.numeric( currentAnswer )
                }

                ## RADIO QUESTION #######################################
                if ( currentType == "radio" ){
                    if ( currentQuestionItemCounterA == 1 ){
                        radioAnswers[ i , currentLabel ] = as.numeric( currentAnswer )
                        if ( currentAnswer == "" ){ ## this cannot really be the case for radio
                            warnings=c( warnings , paste("In file ",psytkFiles[i]," item '",currentLabel,"' no answer for a radio question. That should not happen unless option free is used.",sep=""))
                        }
                    }else{
                        radioAnswersOther[ i , currentLabel ] = currentAnswer # this is the second answer line with a string in it
                    }
                }

                ## DROP QUESTION #######################################
                if ( currentType == "drop" ){
                    if ( currentQuestionItemCounterA == 1 ){
                        dropAnswers[ i , currentLabel ] = as.numeric( currentAnswer )
                    }else{
                        ## not yet implemented, but here it is coded if people select "other"
                    }
                }

                if ( currentType == "list" ){
                    if ( currentQuestionItemCounterA == 1 ){
                        listAnswers[ i , currentLabel ] = currentAnswer
                    }else{
                        ## not yet implemented, but here it is coded if people select more than 1
                    }
                }


                ## TEXTLINE QUESTION ####################################
                ## for textlines you don't have scores ##################

                if ( currentType == "textline" ){
                    ## check how many items this question has
                    tmpNumLabels = psytkSurvey[psytkSurvey[,1]==currentLabel,"items"] ## how many items?

                    if ( isNumberString( currentAnswer ) | currentAnswer == "" ){
                        if ( tmpNumLabels == 1 ){
                            textlineAnswers[ i , currentLabel ] = as.numeric( currentAnswer )
                        }else{
                            textlineAnswers[ i , paste( currentLabel,currentQuestionItemCounterA,sep="." ) ] = as.numeric( currentAnswer )
                        }
                    }else{
                        if ( tmpNumLabels == 1 ){
                            textlineAnswers[ i , currentLabel ] = currentAnswer
                        }else{
                            textlineAnswers[ i , paste( currentLabel,currentQuestionItemCounterA,sep="." ) ] = currentAnswer
                        }
                    }
                }

                if ( currentType == "textbox" ){
                    ## check how many items this question has
                    tmpNumLabels = psytkSurvey[psytkSurvey[,1]==currentLabel,"items"] ## how many items?
                    if ( tmpNumLabels == 1 ){
                        textboxAnswers[ i , currentLabel ] = currentAnswer
                    }else{
                        textboxAnswers[ i , paste( currentLabel,currentQuestionItemCounterA,sep="." ) ] = currentAnswer
                    }
                }

                ## CHECK QUESTION #######################################
                ## for check questions it is empty (FALSE) or not (TRUE)

                if ( currentType == "check" ){
                    ## check how many items this question has
                    tmpNumLabels = psytkSurvey[psytkSurvey[,1]==currentLabel,"items"] ## how many items?

                    tmpanswer = NA
                    if( currentAnswer == "" ) tmpanswer = FALSE
                    if( currentAnswer != "" ) tmpanswer = TRUE

                    if ( tmpNumLabels == 1 ){
                        checkAnswers[ i , currentLabel ] = tmpanswer
                    }else{
                        checkAnswers[ i , paste( currentLabel,currentQuestionItemCounterA,sep="." ) ] = tmpanswer
                    }
                }

                ## RANK QUESTION #######################################
                ## for check questions it is a number

                if ( currentType == "rank" ){
                    ## check how many items this question has
                    tmpNumLabels = psytkSurvey[psytkSurvey[,1]==currentLabel,"items"] ## how many items?

                    tmpanswer = as.numeric( currentAnswer )

                    rankAnswers[ i , paste( currentLabel,currentQuestionItemCounterA,sep="." ) ] = tmpanswer
                }

                ## RANGE QUESTION #######################################
                ## for check questions it is a number

                if ( currentType == "range" ){
                    ## check how many items this question has
                    tmpNumLabels = psytkSurvey[psytkSurvey[,1]==currentLabel,"items"] ## how many items?

                    tmpanswer = as.numeric( currentAnswer )

                    rangeAnswers[ i , paste( currentLabel,currentQuestionItemCounterA,sep="." ) ] = tmpanswer
                }

                ## SCALE QUESTION #######################################
                ## this one is in scores and answers ####################

                if ( currentType == "scale" ){
                    ## check how many items this question has
                    tmpNumLabels = psytkSurvey[psytkSurvey[,1]==currentLabel,"items"] ## how many items in question?

                    if ( tmpNumLabels == 1 ){
                        scaleAnswers[ i , currentLabel ] = as.numeric(currentAnswer)
                    }else{
                        ## people might have changed number of items in the scale question

                        tmpColname = paste( currentLabel,currentQuestionItemCounterA,sep="." )
                        if ( tmpColname %in% colnames( scaleAnswers )){
                            scaleAnswers[ i , tmpColname ] = as.numeric(currentAnswer)
                        }else{
                            tmpWarning = paste( "Item",currentQuestionItemCounterA,"is not possible in question",currentLabel,"with",tmpNumLabels,"items. Best to remove surveyfile",psytkFiles[i])
                            if ( verbose ) { cat(tmpWarning) }
                            warnings = paste(warnings,tmpWarning,sep="")
                        }
                    }
                }

            }
            if ( words[1] == "s:" & includeItem ){

                currentQuestionItemCounterS = currentQuestionItemCounterS + 1

                if ( length(words)==2 ){
                    currentAnswer = words[2]
                }else{
                    currentAnswer = ""
                }

                if ( currentType == "set" ){
                    setAnswers[ i , currentLabel ] = as.numeric( currentAnswer )
                }
                if ( currentType == "radio" ){
                    radioScores[ i , currentLabel ] = as.numeric( currentAnswer )
                }


                ## SCALE QUESTION #######################################
                ## this one is in scores and answers ####################

                if ( currentType == "scale" ){
                    
                    ## check how many items this question has
                    tmpNumLabels = psytkSurvey[psytkSurvey[,1]==currentLabel,"items"] ## how many items?

                    if ( tmpNumLabels == 1 ){
                        scaleScores[ i , currentLabel ] = as.numeric(currentAnswer)
                    }else{
                        tmpColname = paste( currentLabel,currentQuestionItemCounterS,sep="." )
                        if ( tmpColname %in% colnames( scaleScores ) ){
                            scaleScores[ i , tmpColname ] = as.numeric(currentAnswer)
                        }else{
                            ## we already gave the warning for answers so we do not need to warn for the score calculation
                        }
                    }
                }
            }
        }
    }
    
    
    ## -------------------------------------------------------------------------------------------------
    ## All answers and scores are now read, now check if there were sep questions
    
    ## -------------------------------------------------------------------------------------------------

    ## -------------------------------------------------------------------------------------------------
    ## Now some further processing
    ## -------------------------------------------------------------------------------------------------

    ## radioAnswersOther is probably rarerly used so remove all columns with NA only
    radioAnswersOther = radioAnswersOther[, colSums(is.na(radioAnswersOther)) < nrow(radioAnswersOther), drop = FALSE] # drop=F keeps matrix even with one column

    ## Now we want to make sure scores are filled in with answers (because scores are not always set)
    ## needed for: scaleScores, radioScores, dropScores
    ##
    ## in short, in radioScores (etc), you only have data for answers
    ## for which there was a specific scoring, so we need to fill up
    ## the data from answers for the NAs
    
    tmpmask = is.na(radioScores) & !is.na(radioAnswers) ; scaleScores[ tmpmask ] = scaleAnswers[ tmpmask ]
    tmpmask = is.na(radioScores) & !is.na(radioAnswers) ; radioScores[ tmpmask ] = radioAnswers[ tmpmask ]
    tmpmask = is.na(dropScores)  & !is.na(dropAnswers)  ; dropScores[ tmpmask ]  = dropAnswers[ tmpmask ]

    ## ##################################################################################################################
    ## Now we are done with scores and answers and can work on other stuff
    ## ##################################################################################################################

    screenOrientation = ifelse( screenDimension[,1] > screenDimension[,2] , "L" , "P" )

    ## if all endcodes are "", then just set endcode to NA. In this case, all endcodes set before remain empty or ""
    if ( length( unique( endcode ) ) == 1 & endcode[1] == "" ){
        endcode = NA
    }

    if ( verbose ) { cat("\n") } # new line to end progress of reading files

    ## -------------------------------------------------------------------------------------------------
    ## check if we have a standard ResearchConnect project
    ## this comes with a particant Demographics file

    CRData=NULL # for the ultimate data sorted by participant IDs
    tmpCRData = NULL # for reading individual files
    
    if ( all( c("participantId", "assignmentId" , "projectId" ) %in% names(cgi) ) ){
        ## if true, there should only be one projectId
        ## but there could be multiple projectIds if you combine studies, so that should be possible from different files
        ## then we just read and combine files
        ## but we need to make sure those files have same columns, let's assume they have

        projectIds = unique( cgi$projectId ) # these should be a file for each

        if ( length( projectIds ) > 0 ){
            for ( tmpId in projectIds ){
                ## now check if there is a file with the assignments assignments_*.csv
                expectedName = paste("assignments_",tmpId,".csv",sep="" )
                if ( file.exists( file.path( psytkSurveyDir , expectedName ))){
                    if(verbose(message("Now reading CloudResearch demographic data file: ",file.path( psytkSurveyDir , expectedName ))))
                    if ( is.null ( tmpCRData ) ){
                        tmpCRData = read.csv(file.path( psytkSurveyDir , expectedName ))
                    }else{
                        tmpReadFile = read.csv(file.path( psytkSurveyDir , expectedName ))
                        tmpCRData = rbind( tmpCRData , tmpReadFile )
                    }
                }else{
                    if ( verbose ){
                        message("No demographics file(s) found. Given that you use CloudResearch, you could put it here in your data folder.")
                    }
                }
            }
            if ( !is.null( tmpCRData ) ){
                CRData = tmpCRData[ match( cgi$participantId , tmpCRData[,"ParticipantId"] ),]
            }
        }
    }

    ## -------------------------------------------------------------------------------------------------
    ## check if we have a standard Prolific project
    ## this comes with a particant Demographics file


    PRData=NULL # for the ultimate data sorted by participant IDs
    tmpPRData = NULL # for reading individual files

    if ( all( c("PROLIFIC_PID","STUDY_ID","SESSION_ID") %in% names(cgi) ) ){

        if ( verbose  ){
            message("Data were collected with Prolific, now looking for Prolific demographic data.")
            message("If you don't have these, you can download them and put in your data folder here on your local computer.")
        }

        ## if true, there should only be one projectId, but we accept that people might combine multiple studies here in one R analysis folder
        ## and thus more projectIds are possible
        
        projectIds = unique( cgi$STUDY_ID ) # check how many Prolific IDs there are in all data
        if ( length( projectIds ) > 0 ){

            ## now check if there is a file with the assignments prolific_demographics_export_*.csv
            ## Prolific can store it under different names and it might even be renamed by user.

            for ( tmpId in projectIds ){
                ## now check if there is a file with the assignments assignments_*.csv
                expectedName1 = paste("prolific_demographic_export_",tmpId,".csv",sep="" )
                expectedName2 = paste("prolific_export_",tmpId,".csv",sep="" )

                if ( file.exists( file.path( psytkSurveyDir , expectedName1 ))){ expectedName = expectedName1 }
                if ( file.exists( file.path( psytkSurveyDir , expectedName2 ))){ expectedName = expectedName2 }

                if ( file.exists( file.path( psytkSurveyDir , expectedName ))){
                    if(verbose(message("Now reading Prolific demographic data file: ",file.path( psytkSurveyDir , expectedName ))))
                    if ( is.null ( tmpPRData ) ){
                        tmpPRData = read.csv(file.path( psytkSurveyDir , expectedName ))
                    }else{
                        tmpReadFile = read.csv(file.path( psytkSurveyDir , expectedName ))
                        tmpPRData = rbind( tmpPRData , tmpReadFile )
                    }
                }else{
                    if(verbose){
                        message("No demographics file(s) found. Given that you use Prolific, you could put it here in your data folder.")
                        message("The file should start with prolific_demographic_export_ or prolific_export_")
                        message("Maybe you have not downloaded it, don't need it, etc.")
                    }
                }
            }
            if ( !is.null( tmpPRData ) ){
                PRData = tmpPRData[ match( cgi$PROLIFIC_PID , tmpPRData[,"Participant.id"] ),]
            }
        }
    }

    ## --------------------------------------------------------------------------------------------
    ## now create basic return list (if there are experiments, these will be added later)
    returnList = mget(c( # mget is like "list" but automatically names the containing variables
        "surveyobject"     , # you contain the survey structure, which you need for reporting
        "radioAnswers"     , 
        "radioScores"      , 
        "radioAnswersOther",

        "dropAnswers"      , 
        "dropScores"       , 

        "scaleAnswers"     ,
        "scaleScores"      ,
        "checkAnswers"     , 

        "setAnswers"       , # note that for set, there are only answers

        "listAnswers"      , 

        "textlineAnswers"  ,

        "textboxAnswers"   , 

        "rankAnswers"      ,

        "rangeAnswers"     ,

        "startDate"        ,
        "startTime"        ,
        "startDateTime"    ,
        "endDateTime"      ,
        "clientDateTime"   ,
        "duration"         ,
        "country"          ,
        "cgi"              ,
        "screenDimension"  ,
        "screenOrientation",
        "times"            ,
        "warnings"         ,
        "endcode"          ,
        "version"          ,
        "test"             ,
        "n" , "ID" , "IDcount" ))

    if ( !is.null( CRData ) ){
        returnList = c( returnList , mget("CRData" ))
    }

    if ( !is.null( PRData ) ){
        returnList = c( returnList , mget("PRData" ))
    }

    ## ####################################################
    ## if there are experiments, now read them in

    psytkSaveRbind = function( df1 , df2 ){
        if( is.null(df1) ){ return( df2 ) }
        if ( dim(df1)[2] == dim(df2)[2] ){
            return( rbind( df1 , df2 ) )
        }else{
            allColumns = union( colnames(df1) , colnames(df2) )

            for (col in setdiff(allColumns, names(df1))){ df1[[col]] = NA }
            for (col in setdiff(allColumns, names(df2))){ df2[[col]] = NA }

            df1Aligned = df1[,allColumns,drop=F]
            df2Aligned = df2[,allColumns,drop=F]
            return( rbind( df1Aligned, df2Aligned) )
        }
    }

    psytkDuplicateExperiments = NULL

    if ( verbose ){ hr(); message("Now reading experiment data") ; hr() }

    for ( expname in psytkExperiments ){
        
        if( verbose ){ message( "Now reading data for experiment",expname ) ; hr() }

        tmp=NULL ## will contain the experiment data frame

        for ( i in 1:n ){

            pattern = paste( expname , ".*.data." , ID[i] , ".txt" , sep="" )
            tmpFiles = list.files( psytkExperimentDir , pattern=pattern )

            if ( length(tmpFiles)>0 ){
                tmpFilename = file.path( psytkExperimentDir , tmpFiles[1] ) ## take the first file name, we might allow to take second
            }else{
                tmpWarning = paste("WARNING: No experiment file found for participant with ID",ID[i],"for experiment",expname,"\n")
                if ( verbose ) { message(tmpWarning) }
                warnings = c( warnings , tmpWarning )
            }

            if ( length(tmpFiles)==1 | ( length(tmpFiles)>1 & psytkAllowRepeatExp ) ){
                if ( verbose ){ message("Now read in file: ",tmpFilename) }
                tmptable = psytkReadTable( tmpFilename )
                tmpData = data.frame( ID1=ID[i] , ID2 = IDcount[i] ,  tmptable )
                tmp = psytkSaveRbind( tmp , tmpData ) ## also correctly combines non-matching dataframes
            }

            if ( length( tmpFiles ) > 1 ){
                if ( psytkAllowRepeatExp ){
                    if ( verbose ) {
                        message("Participant:",ID[i],expname,"more than 1 file for same experiment, but you allowed with psytkAllowRepeatExp" )
                    }
                }else{
                    if ( verbose ) {
                        message("Participant:",ID[i],expname,"more than 1 file for same experiment, but not used (psytkAllowRepeatExp is FALSE)" )
                    }
                }
                
                psytkDuplicateExperiments = unique( c( psytkDuplicateExperiments , ID[i] ) )
            }
        }
        
        tmpvarname = paste("expData",expname,sep=".")
        returnList = c( returnList , setNames( list( tmp ) , tmpvarname ) )
        rm(tmp)
    }

    if ( verbose ) { message() } # new line to end progress of reading files

    if ( verbose ) { message( "All questionnaire and experiment data are read in" )}

    if ( !is.null(warnings) ){
        if ( verbose ){
            message("There were a number of warnings in reading in PsyToolkit data (separate from regular R warnings):")
            for( i in 1:length(warnings) ){
                message( i , ": " , warnings[i] )
            }
        }
    }
    return( returnList )
}
    
