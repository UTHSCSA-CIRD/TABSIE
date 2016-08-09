toggleMaster <- function(toggleOn, toggleAll){
  #Down and dirty method to toggle things on and off-- hopefully this does work...
  toggleOff = toggleAll[!toggleAll %in% toggleOn]
  if(length(toggleOn) > 0)sapply(toggleOn, function(x){shinyjs::show(id = x, anim= TRUE) })
  if(length(toggleOff) > 0)sapply(toggleOff, function(x){shinyjs::hide(id = x, anim= TRUE) })
}
htmlLabelInfo <- function(label = "This is a label.", title="PopUp Title", content="PopUpContext"){
  html1 = "<a herf= '#' class='btn btn-primary btn-xs' data-toggle='popover' data-placement= 'auto bottom' title='"
  html2 = "' data-content='"
  html3 = "'>?</a>"
  return (HTML(paste(label, html1,title, html2, content, html3)))
}



runGGPLOT <- function(data
                      , x, fill, title = ""
                      , ylab = "Percent", xlab = ""
                      , omitNA_X = TRUE, omitNA_Y = FALSE
                      , position = "stack"
                      , isnum = c(is.numeric(data[[x]]),is.numeric(data[[fill]]))
                      , geomOpts = c('box','violin','points')
                      , width = NULL , alpha = NULL, theme = NULL){
  require(ggplot2);
  # set which type of combo plot to use
  geom_combo <- switch(match.arg(geomOpts)
                       ,box=geom_boxplot
                       ,violin=geom_violin
                       ,points={
                         if(is.null(width)) width <- 0.3;
                         if(is.null(alpha)) alpha <- 0.2;
                         geom_jitter;
                         }
                       );
  if(omitNA_X){data = data[(data[,x] !="0" & data[,x] != ""),]}
  if(omitNA_Y){data = data[(data[,fill] !="0" & data[,fill] != ""),]}
  out <- ggplot(data);
  if(all(isnum)){ # start numeric vs numeric case
    if(is.null(alpha)) alpha <- 0.3;
    out <- out + if(x==fill) { # no point in x~x scatterplot, so show distribution 
        geom_histogram(aes_string(x=x)) 
      } else {
        geom_point(aes_string(x=x,y=fill),alpha=alpha);
      };
  } # end numeric vs numeric case
  else {
    if(is.null(alpha)) alpha <- 1;
    if(!any(isnum)){ # start discrete vs discrete case
        out <- out + geom_bar(aes_string(x=x,fill=fill),position=position);
        } # discrete vs discrete case
    else if(isnum[1]){ # start x is numeric case
        ylab <- c(ylab,xlab); xlab <- ylab[1]; ylab <- ylab[2];
        out <- out + geom_combo(aes_string(x=fill,y=x),width=width,alpha=alpha) + coord_flip();
        } # end x is numeric case
    else { # start fill is numeric case
      out <- out + geom_combo(aes_string(x=x,y=fill),width=width,alpha=alpha);
      } # end fill is numeric case
    } # end case  checks 
  if(is.null(theme)) theme <- theme(axis.text.x=element_text(angle=45,hjust=1));
  out + labs(title = title, y = ylab, x = xlab) + theme;
}
#'fpSummary, a fool-proof summary in the sense that it always returns a count of NA's even if there are none.
fpSummary <- function(xx){
  out <- summary(xx);
  if(!"NA's"%in%names(out)) out["NA's"]<-0;
  out;
}
#'fpSummary, a fool-proof summary in the sense that it always returns a count of NA's even if there are none.
fpSummary <- function(xx){
  out <- summary(xx);
  if(!"NA's"%in%names(out)) out["NA's"]<-0;
  out;
}



ggMosaicPlot <- function(var1, var2){
  #Code by: http://stackoverflow.com/users/2119315/edwin
  #MOSAIC PLOT
  require(ggplot2)
  levVar1 <- length(levels(var1))
  levVar2 <- length(levels(var2))
  
  jointTable <- prop.table(table(var1, var2))
  plotData <- as.data.frame(jointTable)
  plotData$marginVar1 <- prop.table(table(var1))
  plotData$var2Height <- plotData$Freq / plotData$marginVar1
  plotData$var1Center <- c(0, cumsum(plotData$marginVar1)[1:levVar1 -1]) +
    plotData$marginVar1 / 2
  
  ggplot(plotData, aes(var1Center, var2Height)) +
    geom_bar(stat = "identity", aes(width = marginVar1, fill = var2), col = "Black") +
    geom_text(aes(label = as.character(var1), x = var1Center, y = 1.05)) 
}

pickSample <- function (data, percent){
  if(percent > 1 && percent < 100) percent = percent/100
  if(percent >1) stop("Percent is not valid")
  size = nrow(data)* percent
  s = sample(nrow(data), size)
  data[s,]
}
concatRace <- function(x){
  race = ''
  if(x[1] == "White") race = paste(race , "White")
  if(x[2] == 'Black') race = paste(race , "Black")
  if(x[3] == "American_Indian") race = paste(race , "American_Indian")
  if(x[4] == "Asian") race = paste(race , "Asian")
  if(x[5] == "Other"){
    if(race == '') race = "Other"
  }
  gsub('^[ ]','',race)
}


surveyResponded <- function(a){
  #takes a single array and steps through it returning whether or not ALL values in that array are null 0 or NA
  for(r in a){
    if(!is.na(r) & !is.null(r) & r != "0" & r != "NA" & r != "" & r != "None") return (TRUE)
  }
  return (FALSE)
}

reOrderYesNo<- function(col, midAnswers= c("maybe","unsure","maybe_contact"), dontKnowAnswers = c("i do not know", "do not know"), nonAnswers = c("none", "prefernotanswer","prefer_not_answer"), blank = c("", "0"), yes = c("yes", "y", "True"), no = c("no","n","False")){
  #This method takes a factor column
  #If the column is not a factor or if the column does not contain a convertable factor it 
  #returns the column unchanged. 
  #If the column contains a changable factor it will return the reordered factors of the column. 
  #orders as: blank, nonAnswer, 
  
  #If you want to test this, here's an example with obd as the original and obdCop as the copy
  #obdCop = obd
  #for(ii in 1:75){ obdCop[,ii] = reOrderYesNo(obdCop[,ii])}
  #Test: for(ii in names(obdCop)) {if(!identical(obdCop[[ii]],obd[[ii]])) print(table(obdCop[[ii]],obd[[ii]]))}
  
  ##obd$income <- factor(obd$income, levels(obd$income)[c(8,2,4,3,7,5,6,1)])
  #uniqueness check 
  un = c(yes, no, midAnswers, dontKnowAnswers, nonAnswers, blank)
  if(length(un) != length(unique(un))) stop("Error! yes, no, midAnswers, dontKnowAnswers, nonAnswers, and blank must be unique! Overlapping values are not allowed.")
  
  #Check for not factor
  if(class(col) != "factor") return (col)
  
  #obtain the levels, if there are more than 5 levels or fewer than 3 levels return col. 
  lev <- levels(col)
  if(length(lev) > 6 || length(lev) < 2)return(col)
  len = 2
  #going to start building the dynamic call here so we aren't calling as many if statements
  
  indexes = vector()
  #blank
  blankIndex = which(!is.na(match(tolower(lev), tolower(blank))))
  if(length(blankIndex) == 1){
    indexes = c(indexes, blankIndex)
    len = len + 1
  }
  #Non Answer
  nonAnswerIndex = which(!is.na(match(tolower(lev), tolower(nonAnswers))))
  if(length(nonAnswerIndex) == 1){
    indexes = c(indexes, nonAnswerIndex)
    len = len + 1
  } 
  dontKnowIndex = which(!is.na(match(tolower(lev), tolower(dontKnowAnswers))))
  if(length(dontKnowIndex) == 1) {
    indexes = c(indexes, dontKnowIndex)
    len = len + 1
  }
  #requires a no answer
  noIndex = which(!is.na(match(tolower(lev), tolower(no))))
  if(length(noIndex) != 1) {
    return(col)
  }else{
    indexes = c(indexes, noIndex)
  }
  #get mid/maybe not yet sure answer
  midIndex = which(!is.na(match(tolower(lev), tolower(midAnswers))))
  if(length(midIndex) == 1) {
    indexes = c(indexes, midIndex)
    len = len + 1
  }
  #requires a yes answer
  yesIndex = which(!is.na(match(tolower(lev), tolower(yes))))
  if(length(yesIndex) != 1){
    return(col)
  }else{
    indexes = c(indexes, yesIndex)
  }
  #We have levels that are not captured. For now giving a warning.
  if(len != length(lev)) {
    #warning(paste("Only", len, "of", length(lev), "levels matched passed values. Returning original column."))
    return(col)
  }
  #create the dynamic change statement
  col = factor(col, levels(col)[indexes])
  return(col)
}

  
#'# Moved from ciRd.R
#'## (AFB's functions and lookup tables)
#'
stringmap <- rbind(
  c('It depends on something else. If this is your choice  please share what it would depend on and write in your answer.','Yes')
  ,c('It depends on whether it would involve just me or whether it would involve my child or children.','Yes')
  ,c('I would be interested if the research is about: (be specific and write it in)','Yes')
  ,c('It depends on whether my childs doctor thinks it is a good idea.','Yes')
  ,c('Private (for example  Blue Cross/Blue Shield  Aetna  Humana)','Private')
  ,c('It depends on whether my doctor thinks it is a good idea.','Yes')
  ,c('I have no insurance; I pay cash for health care services','Uninsured')
  ,c('My weight in pounds (lbs) is (enter in the box below):','WeightLbs')
  ,c('My height in centimeters is (enter in the box below):','HeightCm')
  ,c('My height in feet inches is (enter in the box below):','HeightFt')
  ,c('No  I am not of Hispanic  Latino or of Spanish origin','No')
  ,c('My weight in kilos (kg) is (enter in the box below):','WeightKg')
  ,c('Yes   I am of Hispanic  Latino or of Spanish origin','Yes')
  ,c('Yes  I would be interested in being contacted','Yes')
  ,c('It depends on how much time it would take.','Yes')
  ,c('It depends on what the research is about.','Yes')
  ,c('Other race (please enter in the next box)','Other')
  ,c('I might be interested in being contacted','Maybe')
  ,c('Maybe_Contact','Maybe')
  ,c('It depends on whether I would be paid.','Yes')
  ,c('Yes  we speak another language at home','Yes')
  ,c('I am not sure how I feel about that','Unsure')
  ,c('American Indian or Alaskan Native','NativeAm')
  ,c('I think that is not a good idea','NotGoodIdea')
  ,c('I think it is a bad idea','NotGoodIdea')
  ,c('I think it is a fantastic idea','Fantastic')
  ,c('I think it is a terrible idea','Terrible')
  ,c('I do not know / I am not sure','Unsure')
  ,c('Do not know','Unsure')
  ,c('I do not know','Unsure')
  ,c('I am not sure how I feel about it','Unsure')
  ,c('Other( describe in box below)','Other')
  ,c('My feeling about this is:','Other')
  ,c('I do not know; I am not sure','Unsure')
  ,c('No  please do not contact me','No')
  ,c('No_Contact','No')
  ,c('I live with 1-2 other people','1-2')
  ,c('I live with 3-4 other people','3-4')
  ,c('I live with 7 or more people','7+')
  ,c('Not willing to be contacted','No')
  ,c('My feelings about this are:','Other')
  ,c('I think it is a good idea','GoodIdea')
  ,c('No  We speak English only','No')
  ,c('Willing to be contacted','Yes')
  ,c('Yes_Contact','Yes')
  ,c('I prefer not to answer.','PreferNotAnswer')
  ,c('I prefer not to answer','PreferNotAnswer')
  ,c('Prefer_Not_Answer','PreferNotAnswer')
  ,c('Black/African American','Black')
  ,c('I live with 5-6 people','5-6')
  ,c('Maybe  I am not sure','Unsure')
  ,c('$100 000 to $199 999','100000-199999')
  ,c('Medicare or Medicaid','MedicareMedicaid')
  ,c('$25 000 to $49 999','025000-049999')
  ,c('$50 000 to $99 999','050000-099999')
  ,c('More than $200 000','200000+')
  ,c('Less than $24 999','000000-024999')
  ,c('White/Caucasian','White')
  ,c('No_Kids','')
  ,c('I have not completed any high school','NoHighSchool')
  ,c('I have completed some high school  but I do not have a GED or diploma','SomeHighSchool')
  ,c('I am a high school graduate or I have a GED (general equivalent diploma)','HighSchool')
  ,c('I have completed some college  but I do not have a college degree','SomeCollege')
  ,c('I completed an Associates degree (including occupational or academic degrees)','Associates')
  ,c('I completed a Bachelors degree','Bachelors')
  ,c('I completed a Masters degree','Masters')
  ,c('I completed a Doctoral degree','Doctorate')
  ,c(' I prefer not to answer','PreferNotAnswer')
  ,c('None','')
  ,c('sv_cmh','CMH')
  ,c('sv_kumc','KUMC')
  ,c('sv_mcrf','MCRF')
  ,c('sv_mcw','MCW')
  ,c('sv_uiowa','IOWA')
  ,c('sv_umn','UMN')
  ,c('sv_unmc','UNMC')
  ,c('sv_uthscsa','UTHSCSA')
  ,c('sv_wisc','WISC')
  ,c('sv_utsw','UTSW')
);

colnamestringmap <- rbind(
  c('research_accept_decisions___1','PR_Me_DependsAbout')
  ,c('research_accept_decisions___2','PR_Me_If_Spec')
  ,c('research_accept_decisions___3','PR_Me_Time')
  ,c('research_accept_decisions___4','PR_Me_Doctor_Op')
  ,c('research_accept_decisions___5','PR_Me_Compensation')
  ,c('research_accept_decisions___6','PR_Me_Involve_Child')
  ,c('research_accept_decisions___7','PR_Me_Other')
  ,c('research_accept_dec_child___1','PR_Child_DependsAbout')
  ,c('research_accept_dec_child___2','PR_Child_If_Spec')
  ,c('research_accept_dec_child___3','PR_Child_TiChild')
  ,c('research_accept_dec_child___4','PR_Child_Doctor_Op')
  ,c('research_accept_dec_child___5','PR_Child_Compensation')
  ,c('research_accept_dec_child___6','PR_Child_Involve_Child')
  ,c('research_accept_dec_child___7','PR_Child_Other')
  ,c('race___1','White')
  ,c('race___2','Black')
  ,c('race___3','American_Indian')
  ,c('race___4','Asian')
  ,c('race___5','OtherRace')
  ,c('race___6','PreferNotAnswer')
);

sexstringmap <- rbind(
  c('1','Male')
  ,c('2','Female')
  ,c('f','Female')
  ,c('F','Female')
  ,c('m','Male')
  ,c('M','Male')
  ,c('female','Female')
  ,c('male','Female')
);

# for longFactorLev
stdLevels <- list(
  research_feelings=c('','PreferNotAnswer','Terrible','NotGoodIdea','Unsure','GoodIdea','Fantastic','Other')
  ,insurance=c('','PreferNotAnswer','Unsure','Uninsured','MedicareMedicaid','Private','Other')
  ,sex=c('PreferNotAnswer', 'Male','Female', 'Other')
  ,education=c('','PreferNotAnswer','NoHighSchool','SomeHighSchool','HighSchool','SomeCollege','Associates','Bachelors','Masters','Doctorate','Other')
);

mapstrings <- function(xx,map=stringmap,...){
  UseMethod('mapstrings');
}

mapstrings.default <- function(xx,map=stringmap,...){
  # xx  : character vector
  # map : data.frame or matrix with matched string in first column and 
  #       replacement string in second column
  if(is.null(nc<-ncol(map))||nc<2) stop('The map argument needs to be a matrix with character strings in the first two columns');
  matches <- match(xx,map[,1]);
  ifelse(is.na(matches),xx,map[matches,2]);
}

mapstrings.factor <- function(xx,map=stringmap,...){
  levels(xx) <- mapstrings.default(levels(xx),map,...);
  xx;
}

longFactorLev <- function(xx,map=stdLevels){
  # will compare the levels of a factor to each of the vectors in list `map`
  # if one matches exactly the ordering of that vector is used for this factor
  # otherwise it is returned unchanged
  # xx  :   A factor
  # map :   A list of character vectors
  if(!is.factor(xx)) return(xx) else levs<-levels(xx);
  # iterate over map and find matching levels without regard for their order
  whichlevels<-which(sapply(map,function(zz) identical(union(zz,levs),intersect(zz,levs))));
  # take the order of the first matching level and impose it on xx
  if(length(whichlevels)<1) return(xx) else {
    factor(xx,levels=map[[whichlevels[1]]]);
  }
}

guessnum <- function(xx,exclude='',returnval=F,tolerance=.11){
  xx <- xx[!is.na(xx)&!xx%in%exclude];
  out <- sum(is.na(as.numeric(as.character(xx))))/length(xx);
  if(returnval) out else out <= tolerance;
}

vs <- function(xx
               ,type=c('numeric','factor','logical','character','binary','multinomial','time','date','dt','znumeric')
               ,ignorevs='ignorevs',...){
  # This function takes a data.frame and returns the names of columns of a 
  # particular type
  # xx        : data.frame
  # type      : string indicating what type of column names to return (the 
  #             non-standard ones are explained below)
  # ignorevs  : the name of the option setting where you store an optional 
  #             vector of column names that should never be returned (because 
  #             e.g. you know ahead of time that they are not useful for
  #             plotting or analysis)
  
  # first we define a TRUE/FALSE test function for selecting the columns
  # instead of a lot of if/else statements, we just have one switch() 
  # statement that is more readable. For more information, type ?switch
  # The first argument is match.arg, which matches the possibly partial
  # string the user typed to full argument. Think of tye `type` argument
  # to the vs() function as a multiple-choice question.
  test <- switch(match.arg(type)
                 ,'numeric'=is.numeric
                 ,'factor'=is.factor
                 ,'logical'=is.logical # i.e. all non-missing values in this column are TRUE/FALSE
                 ,'character'=is.character
                 ,'binary'=function(zz) length(unique(zz))==2
                 ,'multinomial'=function(zz) length(unique(zz))<length(zz)
                 ,'time'=function(zz) inherits(zz,'POSIXt')
                 ,'date'=function(zz) inherits(zz,'Date')
                 ,'dt'=function(zz) inherits(zz,c('Date','POSIXt')) # i.e. either date OR time
                 ,'znumeric'=function(zz) guessnum(zz,...)
  );
  # Then we apply the test function appropriate to the data type of to each 
  # column of xx using the `sapply()` function. What it returns, `matches` is a
  # vector of TRUE/FALSE (logical) values, with each having the same name as 
  # the column in xx that it refers to. If that column is the type being sought
  # it will have a value of TRUE, otherwise a value of FALSE.
  matches <- sapply(xx,test);
  # we return our final answer...
  return(
    setdiff( # the difference between
      # ...the names from `matches` where the corresponding value is `TRUE`
      names(matches)[matches]
      # ...and an optional environment variable that can be a vector of names
      # to ignore if they exist. To set this, you can do, e.g.
      # `option(ignorevs=c('patient_num','birth_date'))`
      ,getOption(ignorevs)));
}


binfactor<-function(xx,levs,other='other',dec=T){
  # This function takes a factor `xx` and returns it re-binned and optionally 
  # sorted by size.
  # xx    : a factor
  # levs  : Either an integer of length 1 or a vector of level names to keep.
  #         The other levels will get binned together into a single level 
  #         called...
  # other : The name of the level to which all the small levels get collapsed.
  #         `other` by default.
  # dec   : If TRUE, the levels are also re-ordered so that they are in 
  #         decreasing order of how many observations are part of each level.
  #         If FALSE, then likewise but in increasing order. Finally, NA 
  #         disables re-ordering of levels.
  if(missing(levs)) levs <- names(which.max(summary(xx))) else {
    # If levs argument not specified, just keep the most populated level and
    # bin everything else together.
    if(length(levs)==1&&!is.na(as.integer(levs))) {
      levs <- names(sort(summary(xx),dec=T)[1:levs]);
    }
    # Otherwise, if a single integer was given, take that many of the most 
    # populated levels.
  }
  # Capture the current levels, and then, if they are not already part of the
  # levels you want to keep, assign to them the default `other` name.
  newlevs <-levels(xx); newlevs[!newlevs%in%levs]<-other;
  # newlevs is a vector of character values. Now we assign it to existing 
  # levels, thus overwriting them.
  levels(xx)<-newlevs;
  # If `dec` is not set to NA we rebuild the factor with a new ordering of the
  # levels, by size.
  if(is.na(dec)) xx else factor(xx,levels=names(sort(summary(xx),dec=dec)))
}

# Prepare a data.frame for heatmaps, fpca, sphpca, and other haters 
# that are unforgiving of data that just happens to be in a heterogeneous 
# format or have missing values. Haters gonna hate.
nprep <- function(xx,data.frame=T){
  # coerce xx to numeric via data.matrix, scale/center it
  warn <- getOption('warn'); options(warn=-1);
  xxinput <- scale(data.matrix(xx));
  # drop the non-pairwise-correlatable columns
  okaynames <- apply(cor(xxinput,use='pairwise'),2,function(xx) !all(is.na(xx)));
  options(warn=warn);
  # keeping only the okaynames, impute missing values 
  require(e1071);
  xxinput <- impute(xxinput[,okaynames]);
  if(data.frame) data.frame(xxinput) else xxinput;
}

pcawrap <- function(xx,respvar=c(),predvars,drop=c(),prep=nprep,pca=c('sphpca','fpca'),...){
  require(psy);
  # xx      : A matrix or data.frame
  # respvar : String, the column containing the response variable
  # predvars: String vector, the columns containing predictor variables,
  #           optional. By default all columns excluding predvars and exclude
  # drop    : String vector, columns to exclude, optional
  # prep    : function to call to preprocess data to avoid hard-to-interpret 
  #           errors from plotting function.
  # pca     : Plotting function. Currently either 'sphpca' or 'fpca'
  # ...     : Passed to plotting function
  # if respvar no longer included, error
  pca <- match.arg(pca);
  xx <- prep(xx);
  xxnames <- colnames(xx);
  if(length(respvar)>0 && !respvar%in%xxnames) 
    stop("Specified response variable got dropped during prep");
  # find final predvars
  if(missing(predvars)) predvars <- setdiff(xxnames,c(respvar,drop)) else {
    predvars <- setdiff(intersect(xxnames,predvars),c(respvar,drop));
  }
  xx <- xx[,c(respvar,predvars)];
  if(pca=='fpca'){
    # construct the formula
    frminput <- formula(paste0(respvar,'~',paste0(predvars,collapse='+')));
    fpca(frminput,data=xx,...);
  } else if(pca=='sphpca') sphpca(xx,...);
}
