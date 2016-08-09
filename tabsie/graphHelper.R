#This file exists to keep all of the graphing options in one place and clean up the server code. 

getPointPlot <- function(pdata, input, type){
  validate(
    need(input$sizeSlide, warningRender),
    need(input$alphaSlide, warningRender)
  )
  if(type =='FN')if(input$xOmit)pdata = pdata[(pdata[,input$xVal] !="0" & pdata[,input$xVal] != ""),]
  p = ggplot(data = pdata, aes_string(x = input$xVal, y = input$yVal))
  if(input$pointJitter) {
    validate(need(input$widthSlide, warningRender))
    p = p + geom_jitter(width = input$widthSlide, size = input$sizeSlide, alpha = input$alphaSlide)
  }else {
    p = p+ geom_point(size = input$sizeSlide, alpha = input$alphaSlide)
  }
  
  if(input$pointColor != "No color") p = p + aes_string(colour = input$pointColor)
  if(input$pointShape != "No shape") p = p + aes_string(shape = input$pointShape)
  p
}

runGGPLOTFF <- function(data, x, fill, omitNA_X=TRUE, omitNA_Y = FALSE, position = "stack"){
  require(ggplot2)
  if(omitNA_X){
    data = data[(data[,x] !="0" & data[,x] != ""),]
  }
  if(omitNA_Y){
    data = data[(data[,fill] !="0" & data[,fill] != ""),]
  }
  ggplot(data = data)+ 
    geom_bar(aes_string(x = x, fill = fill), position = position)+
    guides(fill = guide_legend(reverse = TRUE))+
    scale_fill_discrete(guide = guide_legend(reverse = TRUE))
}

getViolinPlot <- function(pdata, input){
  if(input$xOmit)pdata = pdata[(pdata[,input$xVal] !="0" & pdata[,input$xVal] != ""),]
  p = ggplot(data = pdata, aes_string(x = input$xVal, y = input$yVal))
  if(input$violinColor){
    p = p + geom_violin(aes_string(fill = input$xVal), trim = input$violinTrim)
  }else{
    p = p + geom_violin(trim = input$violinTrim)
  }
  if(input$violinBoxOpt){
    p = p + geom_boxplot(width = 0.1, fill = "white")
  }
  p
}
getBoxPlot <- function(pdata, input){
  if(input$xOmit)pdata = pdata[(pdata[,input$xVal] !="0" & pdata[,input$xVal] != ""),]
  p = ggplot(data = pdata, aes_string(x = input$xVal, y = input$yVal))+ geom_boxplot()
  if(input$boxColor) p = p + aes_string(fill = input$xVal)
  p
}
addTheme <- function(p, input){
  if(input$titleField != ""){
    p = p + ggtitle(input$titleField)
  }
  #this allows them to add a title without having to fill in the x and y labels with something other than the default
  if(input$xLab != "" | input$yLab != ""){
    p = p + labs(x = input$xLab, y = input$yLab)
  }
  p = p+theme(text = element_text(size = input$textSize), axis.text.x = element_text(angle = input$xLabRotation, vjust = input$xLabHeight))
  p
}
