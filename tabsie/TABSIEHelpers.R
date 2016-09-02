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
  
  if(!any(line)||line > length(dataList)){
    #Well! That's not supposed to happen! If we don't return things break, so we'll return the
    #default.
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

# A wrapper function around pca that makes it more tolerant of input formats
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

rbindAllCols <- function(...){
  # take data-frames with possibly differently-named columns and 
  # rbind them anyway so that the resulting data.frame has the union
  # of the colums (but same-named columns will aligned with each other)
  dfs <- list(...)
  # this is the union 
  allcs <- unique(unlist(sapply(dfs,names)))
  dfs <- lapply(dfs,function(xx) {
    xx[,setdiff(allcs,names(xx))]<-NA
    xx[,allcs]
    })
  do.call(rbind,dfs)
}

make_gs <- function(input=rbind(1:col_extent,1:col_extent),title='defaultlog'
                    ,ws_title='S1',col_extent=200, row_extent=10000
                    ,savefile='gs'){
  # everything you need to create credentials for googlesheets
  # Note: requires a browser to be present; will prompt you to
  # log into Google and give permissions
  # move the resulting rdata file to where app is to be deployed, and load it
  # You will get list object named gsout
  # Do the following: 
  # gs_auth(gsout$token,cache=F)
  # Now you can do:
  # gs_add_row(gsout$gskey, input = c(1,2,3))
  gsout <- list();
  # create token
  token <- gs_auth(cache = F);
  # create new gs spreadsheet
  gsfile <- gs_new(title=title,ws_title = ws_title
                  ,col_extent = col_extent,input=input);
  gskey <- gs_key(gsfile$sheet_key,lookup=F,visibility = 'private');
  gs_add_row(ss=gskey,ws=ws_title,input=input[1]);
  # save the token
  saveRDS(token,file=paste0(savefile,'.rds'));
  # save the token and handle
  gsout <- list(token=token,gsfile=gsfile,gskey=gskey);
  save(gsout,file=paste0(savefile,'.rdata'));
  # return token and handle
  invisible(gsout);
  #gs_add_row(gsnew,input=c(baz[3,]))
}
