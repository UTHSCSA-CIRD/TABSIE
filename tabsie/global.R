#Configuration options
#warning message to display when user interface is still rendering. 
source("TABSIEHelpers.R")
warningRender = "User interface has not finished rendering, please wait."
##hash of authentication password
######################DICTIONARY!##################
#Post Dictionary- This is android style. In android aps all of the text in the app is 
#stored in a "dictionary" that can be hot-swapped in the event of a change of language.
#That way nothing is hard coded.
##BarPlot##
dictBarProportions = htmlLabelInfo("Bar Plot Format","Bar Plot Formats","The <i><b>actual values</b></i> of a 
        barplot indicate the actual number of responses while the <i><b> proportional bar graph</i></b> will show the 
        proportion of responses.")
##FN##
dictFN = htmlLabelInfo("Which visualization?", "Factor Number Visualization Types", 
        "<b>Box plots</b> show you the location of the mean with a box around the middle 50% and tails indicating 
            the upper and lower 25th percentiles.<hr/>
         <b>Violin plots</b> show the predicted distribution of responses. They may also optionally (see the <i>advanced</i> 
            tab) contain box plots.<hr/>
         A <b>point graph</b> places a point at the position of matching pairs. It is recommended that you jitter the points 
            when comparing a factor to a number.")
##Point##
dictPointOpacity = htmlLabelInfo("Point Opacity", "Alpha","This regulates the opacity/transparency of the points. 
        The lower the opacity the more transparent the plots and the easier it is to see high density points.")
##Jitter##
dicJitter=   htmlLabelInfo("Jitter the points?","Jitter Help", 
        "Jitter shifts the points vertically and horizontally. This is useful if a large number of points are overlapping. 
        Use with the point opacity and jitter width.")
dictJitterWidth = htmlLabelInfo("Jitter Width", "Jitter Width Help","The jitter width indicates the amount of vertical 
        and horizontal jittering applied to the points. The higher the jitter width the farther apart the points.")

############################htmlLabelInfo function#################

