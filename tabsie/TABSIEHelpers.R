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
getpData<- function(filterOption, filterDic, dataList){
  #this function is another of those nice little functions that does something easy, but 
  #that we don't want to have to program elsewhere.
  line = which(filterDic == filterOption) #which filter is activated?
  
  if(!any(line)){
    #Well! That's not supposed to happen! If we don't return things break, so we'll return the
    #default.
    createAlert(session, "systemError", "dError", content = "There is an error with the supplied data file. The filter you have selected does not exist. Resetting to default filter.", title = "ERROR!", append = TRUE)
    return(dataList[[1]])
  }
  if(line > length(dataList)){
    #Well.... someone created more filter options in their dict than actually exists... 
    createAlert(session, "systemError", "dError", content = "There is an error with the supplied data file. The filter you have selected does not exist. Resetting to default filter.", title = "ERROR!", append = TRUE)
    return(dataList[[1]])
  }
  return (dataList[[line]])
}##END getpData


#'fpSummary, a fool-proof summary in the sense that it always returns a count of NA's even if there are none.
fpSummary <- function(xx){
  out <- summary(xx);
  if(!"NA's"%in%names(out)) out["NA's"]<-0;
  out;
}

# A wrapper function around pca that makes it mor tolerant of input formats
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

