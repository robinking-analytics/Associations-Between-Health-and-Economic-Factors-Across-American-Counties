########################################################
# set constants
DATA_DIR <- "C:/Users/robin/Documents/MIS/MIS500/Portfolio"
DATA_FILE <- "County_Health_Rankings.csv"
SOURCE_FILE <- paste(DATA_DIR,DATA_FILE,sep="/")
COL_RATE      <- 1
COL_RATENORM  <- 2
COL_RATESCALED<- 3
COL_LOCATION  <- 4
COL_LOCN_YEAR <- 5
COL_YEARSPAN  <- 6
COL_REGION    <- 7
COL_DIVISION  <- 8

########################################################
#Define functions


drawSmoothScatter  <- function(xvecname,yvecname, df, xtitle, ytitle)  {
########################################################
   #  This function draws smooth scatter plots to PNG
   #  It creates a PNG file in the DATA_DIR/graphs directory.
   #  Args:
   #    xvecname: name of vector from dataframe for x axis
   #    yvecname: name of vector from dataframe for x axis
   #    df: dataframe that contains the x and y vectors
   #    xtitle: the label for the x axis
   #    ytitle: the label for the y axis
   #  Returns:
   #    a string matching the caption on the plot (with p and r^2 values)

   #TEST DATA: This test data can be uncommented for use 
   #           when running function line-by-line
   #xvecname <- "prematuredeath.norm"
   #yvecname <- "preventablehosp.norm"
   #df <- combined.df
   #xtitle <- "Premature Death"
   #ytitle <- "Preventable Hospitalization"

   #  Get the name of the paired columns
   pairedcol <- paste(xvecname,"~",yvecname,sep="")
   # Calculate the lm fit for the pairing
   the_fit <- lm(as.formula(pairedcol),df,na.action=na.omit )
   #Get the 2 vectors
   xvec <-combined.df[[xvecname]]
   yvec <- combined.df[[yvecname]]
   #compute the title based on xtitle and ytitle
   title <- paste(xtitle, ytitle, sep=" vs. ")
   #compute the filename, in DATA_DIR/graphs
   fname <- paste( "scatter",xvecname,"-",yvecname,sep="")
   fname <- gsub("[.]","",fname)
   fname <- paste (fname, ".png", sep="")
   fname <- paste(DATA_DIR, "graphs", fname, sep="/")
   # Open the new PNG file
   png(filename=fname,width=500,height=400)
   #Plot the smooth scatter; add a line to show the linear model
   smoothScatter(x=xvec, y=yvec, 
                  main=paste(title,"Rates"),
                  cex=.175,nbin=200,nrpoints=10,
                  xlab=paste(xtitle,"Rate"),
                  ylab=paste(ytitle,"Rate"),
                  xaxt='n',yaxt='n' ) #+geom_smooth(na.rm=TRUE)     

   # get the correlation fit
   f <- summary(the_fit)$fstatistic
   #extract p
   p <- pf(f[1],f[2],f[3],lower.tail=F)
   #extract rsquared
   r2 <- summary(the_fit)$r.squared
   # add p and rsquared to the bottom of the graph
   plottext.caption <- paste("p: ",signif(p,digits=3),sep="")
   plottext.caption <- paste(plottext.caption, ", r-squared: ",signif(r2,digits=3),sep="")
   mtext(side=1,text=plottext.caption, col="black")
   dev.off()
   return (plottext.caption)
}

drawPairedPlots <- function()  {
########################################################
   # Draw paired plots to easily visualize how core rates are correlated
   #YEARS: 
   #The measures use different years, with varying overlap, as follows:
   #obesity:ob inactive:in violence:vi unemployed:ue 
   #childpov:cp uninsured: ui prevhosp:ph premdeath: pd
   #CP  UE                         2002,2003,2012
   #CP  UE  OB  IN                 2004,2005
   #CP  UE  OB  IN  UI             2006,2007
   #CP  UE  OB  IN  UI  PH         2008,2009,2010
   #CP  UE          UI  PH         2011
   #                    PH         2006-2007 
   #                        PD     2002-2004
   #                    PH  PD  VI 2003-2005
   #                        PD  VI 2004-2006,2005-2007,2006-2008,2007-2009,2008-2010
   #                            VI 2009-2011


   # Draw paired plots to visualize how rates are correlated
   pairs1 <- c("inactivity.norm", "obesity.norm","unemployed.norm","childpoverty.norm")           
   pairs2 <- c("unemployed.norm","childpoverty.norm", "uninsured.norm", "preventablehosp.norm")
   pairs3 <- c("violentcrime.norm","preventablehosp.norm","prematuredeath.norm") 
   fname <- paste(DATA_DIR, "graphs", "paired_plot_set_1.png", sep="/")
   # Open the new PNG file, draw the plot, and close the file
   png(filename=fname,width=700,height=400)
   # Plot the Children in Poverty numbers by division
   pairs(combined.df[,pairs1],cex=0.75, lower.panel=NULL) 
   dev.off()
   fname <- paste(DATA_DIR, "graphs", "paired_plot_set_2.png", sep="/")
   # Open the new PNG file, draw the plot, and close the file
   png(filename=fname,width=700,height=400)
   pairs(combined.df[,pairs2],cex=0.75, lower.panel=NULL)
   dev.off()
   fname <- paste(DATA_DIR, "graphs", "paired_plot_set_3.png", sep="/")
   # Open the new PNG file, draw the plot, and close the file
   png(filename=fname,width=700,height=400)
   pairs(combined.df[,pairs3],cex=0.75, lower.panel=NULL)
   dev.off()
}
drawBoxplotMeasureByRegionDivision <- function (df, colname, gtitle) {
########################################################
   #  Draws a PNG to DATA_DIR/graphs showing boxplot graphs of 
   #  the identified data by Division and Region in the graphs directory
   #  Args:
   #    df:      the dataframe containing the values to be plotted
   #    colname: the name of the column with rate data
   #    gtitle:  the title of the rate as it is displayed on the graph

   # Test data: uncomment for use in testing
   # df     <- childpoverty
   # colname<- "Children_in_povertynorm"
   # gtitle <- "Children in Poverty"

   # Set the file name
   fname <- paste(colname, "_by_region.png", sep="")
   fname <- paste(DATA_DIR, "graphs", fname, sep="/")
   # Open the new PNG file
   png(filename=fname,width=700,height=500)
   # Plot the Rates numbers by region
   boxplot(as.formula(paste(colname,"~region",sep="")),data=df,main=paste(gtitle,"by Region"), xlab="Region", 
          ylab=paste("Rate of",gtitle),las=2,na.rm=TRUE, notch=TRUE)
   the_fit=lm(as.formula(paste(colname,"~region",sep="")),data=df)
   # get the correlation fit
   c <- summary(the_fit)$coefficients
   p <- c[,4]
   #extract rsquared
   r2 <- summary(the_fit)$r.squared
   # Skipping: adding p and r to graph
   # add p and rsquared to the bottom of the graph
   #plottext.caption <- paste("p: ",signif(p,digits=3),collapse=", ")
   #plottext.caption <- paste(plottext.caption, ", r-squared: ",signif(r2,digits=3),sep="")
   #mtext(side=1,text=plottext.caption, col="black")
   # close the PNG file
   dev.off()

   # Set the file name
   fname <- paste(colname, "by_division.png", sep="")
   fname <- paste(DATA_DIR, "graphs", fname, sep="/")
   # Because x labels were cut off, increase the size for x labels:
   # Open the new PNG file
   png(filename=fname,width=700,height=500)
   # Give the X axis extra space so that division labels display
   par(mar=c(8.5,4,4,2) + 0.1)
   boxplot(as.formula(paste(colname,"~division",sep="")),data=df,
          main=paste(gtitle,"by Division"), xlab="",notch=TRUE,
           ylab=paste("Rate of ",gtitle),las=2,na.rm=TRUE)
   #lm_rate_division=lm(as.formula(paste(colname,"~division",sep="")),data=df)
   dev.off()
}

getScaledRegionDivisionDataFrame 	<- function(continentalOnly=FALSE) {
########################################################
   #  Uses existing dataframes: obesity, inactivity, 
   #  unemployed, childpoverty, uninsured, preventablehosp,
   #  prematuredeath (skips violentcrime since it differs by state)
   #  to draw a PNG to DATA_DIR/graphs showing line graphs of 
   #  measures by division
   #  Args:
   #    continentalOnly: boolean (default FALSE). If true, skips
   #        Hawaii and Alaska
   # Returns:
   #    A dataframe with columns for:
   #        measure (factor): the name of the measure
   #        scaled rate (numeric): the scaled value of the rate
   #        division (factor): a concatentated value of Region-Division

   if(continentalOnly) {
         excludematch <-substr(obesity[[COL_LOCATION]], 1,2)
         useobesity <- obesity[-(which (is.element(excludematch, c("HI", "AK")))),]

         excludematch <-substr(inactivity[[COL_LOCATION]], 1,2)
         useinactivity     <- inactivity[-(which (is.element(excludematch, c("HI", "AK")))),]

         excludematch <-substr(unemployed[[COL_LOCATION]], 1,2)
         useunemployed     <- unemployed[-(which (is.element(excludematch, c("HI", "AK")))),]
   
         excludematch <-substr(childpoverty[[COL_LOCATION]], 1,2)
         usechildpoverty   <- childpoverty[-(which (is.element(excludematch, c("HI", "AK")))),]

         excludematch <-substr(uninsured[[COL_LOCATION]], 1,2)
         useuninsured      <- uninsured[-(which (is.element(excludematch, c("HI", "AK")))),]

         excludematch <-substr(preventablehosp[[COL_LOCATION]], 1,2)
         usepreventablehosp<- preventablehosp[-(which (is.element(excludematch, c("HI", "AK")))),]
 
         excludematch <-substr(prematuredeath[[COL_LOCATION]], 1,2)
         useprematuredeath <- prematuredeath[-(which (is.element(excludematch, c("HI", "AK")))),]

   } else {
         useobesity        <- obesity
         useinactivity     <- inactivity   
         useunemployed     <- unemployed   
         usechildpoverty   <- childpoverty   
         useuninsured      <- uninsured   
         usepreventablehosp<- preventablehosp 
         useprematuredeath <- prematuredeath
   }

   #Set up a data frame that holds COL_RATESCALED for each measure, with REGION-DIVISION as a primary key
   combo.scaled.df <- cbind.data.frame(measure="Obesity", rate=useobesity[[COL_RATESCALED]],
              division=paste(useobesity[[COL_REGION]],useobesity[[COL_DIVISION]],sep="-"))
   combo.scaled.df <- rbind.data.frame(combo.scaled.df,
              cbind.data.frame(measure="Physical Inactivity", rate=useinactivity[[COL_RATESCALED]],
                  division=paste(useinactivity[[COL_REGION]],useinactivity[[COL_DIVISION]],sep="-")))
   combo.scaled.df <- rbind.data.frame(combo.scaled.df,
              cbind.data.frame(measure="Unemployment", rate=useunemployed[[COL_RATESCALED]],
                  division=paste(useunemployed[[COL_REGION]],useunemployed[[COL_DIVISION]],sep="-")))
   combo.scaled.df <- rbind.data.frame(combo.scaled.df,
              cbind.data.frame(measure="Child Poverty", rate=usechildpoverty[[COL_RATESCALED]],
                  division=paste(usechildpoverty[[COL_REGION]],usechildpoverty[[COL_DIVISION]],sep="-")))
   combo.scaled.df <- rbind.data.frame(combo.scaled.df,
              cbind.data.frame(measure="Uninsured", rate=useuninsured[[COL_RATESCALED]],
                  division=paste(useuninsured[[COL_REGION]],useuninsured[[COL_DIVISION]],sep="-")))
   combo.scaled.df <- rbind.data.frame(combo.scaled.df,
              cbind.data.frame(measure="Preventable Hospitalization", rate=usepreventablehosp[[COL_RATESCALED]],
                  division=paste(usepreventablehosp[[COL_REGION]],usepreventablehosp[[COL_DIVISION]],sep="-")))
   combo.scaled.df <- rbind.data.frame(combo.scaled.df,
              cbind.data.frame(measure="Premature Death", rate=useprematuredeath[[COL_RATESCALED]],
                  division=paste(useprematuredeath[[COL_REGION]],useprematuredeath[[COL_DIVISION]],sep="-")))

   return (combo.scaled.df)
}

getFirstColNameAppendIdentifier <- function (mat, identifier) {
########################################################
   #  Returns the name of the first column in the matrix with
   #  "." and the identifier appended to it.
   #  Args:
   #    mat: the matrix whose first column name will be the root 
   #         of the return value
   #    identifier: the suffix that will be added after "." to 
   #         the column name
   # Returns:
   #   The name of the first column in the matrix with
   #         "." and the identifier appended to it.

   # get the new column name and then return
   newcol <- paste(colnames(mat)[1],identifier,sep=".")
   return (newcol)
}

PlotHistAndQQNorm <- function (vec, title, rate_qualifier="Rate",type="") {
########################################################
   # Creates a PNG image file with a histogram and qqline 
   # using a fixed directory (DATA_DIR/graphs) and dynamic file name.
   # Args: 
   #   vec: the vector that contains the rates that will be plotted
   #   title: the name of the measure that will be plotted
   #   rate_qualifier: The description that appears on the X label;
   #          defaults to "Rate", but could be changed, such as to 
   #          "Rate per 100,000 population"
   #   type: a string describing the rate; defaults to "", but could
   #          be set to a transformation description (such as "log")

   # Set the file name to begin with "hist_and_qqnorm" and the type,
   # with any spaces replaced with underscores ("_")
   fname <- gsub(" ","_",title)
   fname <- paste(fname, "hist_and_qqnorm",type,sep="_")
   # add the ".PNG" extension
   fname <- paste(fname,"png",sep=".")
   # precede the filename with "graphs" under the DATA_DIR directory
   fname <- paste(DATA_DIR, "graphs", fname, sep="/")

   # Add the title as a prefix to the rate_qualifier
   rate_qualifier  <- paste(title, rate_qualifier, sep=" ")
   # Open the new PNG file
   png(filename=fname,width=700,height=400)
   # Set it up so that 2 plots are created side-by-side
   par (mfrow=c(1,2))
   # strip out 0s from histograms, since all rates should be > 0
   vec_without_na <- vec[!vec==0]
   # plot the histogram
   hist(vec_without_na,main=title,xlab=rate_qualifier)
   # draw the qqnorm plot with the qqline
   qqnorm(vec_without_na,main=title)
   qqline(vec_without_na,col="gray")
   #close the file
   dev.off()
}

getDivisions <- function (state) {
########################################################
   # Returns a vector of divisions based on a vector of states, using
   #    US Governement Census Divisions.
   # Arg: 
   #   state: the vector that contains state abbreviations
   # Returns:
   #   a vector of devisions corresponding to the states using
   #   categories defined in 
   #   https://www.census.gov/geo/reference/gtc/gtc_census_divreg.html
   #   for the mapping

   #The US Cesus assigns regions and divisions based on states as follows:
   #REGION I: NORTHEAST
   #  Division I: New England: "CT", "RI", "MA", "VT", "NH", "ME"
   #  Division 2: Middle Atlantic: "NY", "PA", "NJ"
   #REGION 2: MIDWEST
   #  Division 3:  East North Central: "IL", "OH", "MI", "IN", "WI"
   #  Division 4:  West North Central:"MO", "IA", "MN", "ND", "SD", "NE", "KS"
   #REGION 3: SOUTH
   #  Division 5: South Atlantic: "DE", "MD", "DC", "GA", "FL", "MD", "VA", "WV", "SC", "NC" 
   #  Division 6: East South Central: "MS", "KY", "TN", "AL"
   #  Division 7:  West South Central: "LA", "AR", "TX", "OK"
   #REGION 4: WEST
   #  Division 8:  Mountain: "NM", "AZ", "CO", "WY", "MT", "ID", "UT", "NV" 
   #  Division 9: Pacific: "WA", "OR", "CA", "AK", "HI" 

   # set up a vector of NA the length of vector state
   division <- rep(NA,length(state))

   #Assign divisions based on state codes
   division <- replace(division,
                     is.element(state, c("CT", "RI", "MA", "VT", "NH", "ME")),
                     "New England")
   division <- replace(division,
                     is.element(state, c("NY", "PA", "NJ")),
                     "Middle Atlantic")
   division <- replace(division,
                     is.element(state, c("IL", "OH", "MI", "IN", "WI" )),
                     "East North Central")
   division <- replace(division,
                     is.element(state, c( "MO", "IA", "MN", "ND", "SD", "NE", "KS")),
                     "West North Central")
   division <- replace(division,
                     is.element(state, c("DE", "MD", "DC", "GA", "FL", "MD", "VA", "WV", "SC", "NC" )),
                     "South Atlantic")
   division <- replace(division,
                     is.element(state, c( "MS", "KY", "TN", "AL")),
                     "East South Central")
   division <- replace(division,
                     is.element(state, c("LA", "AR", "TX", "OK" )),
                     "West South Central")
   division <- replace(division,
                     is.element(state, c("NM", "AZ", "CO", "WY", "MT", "ID", "UT", "NV")),
                     "Mountain")
   division <- replace(division,
                     is.element(state, c("WA", "OR", "CA", "AK", "HI"  )),
                     "Pacific")
}
getRegions <- function (division) {
########################################################
   # Returns a vector of regions based on a vector of divisions, using
   #    US Governement Census Divisions and Regions.
   # Arg: 
   #   division: the vector that contains division names
   # Returns:
   #   a vector of regions corresponding to the divisions using
   #   categories defined in 
   #   https://www.census.gov/geo/reference/gtc/gtc_census_divreg.html
   #   for the mapping

   #Default list to NA
   region <- rep(NA,length(division))
   #Aassign region based on division
   region <- replace(region,
                     is.element(division, c("New England","Middle Atlantic")),
                     "Northeast")
   region <- replace(region,
                     is.element(division, c("East North Central","West North Central")),
                     "Midwest")
   region <- replace(region,
                     is.element(division, c("South Atlantic", 
                        "East South Central", "West South Central")),
                     "South")
   region <- replace(region,
                     is.element(division, c("Mountain", "Pacific" )),
                     "West")
   return (region)
}


getCountyRowsByMeasureDf <- function (dfile, str_measure) {
########################################################
   # Return a dataframe with key columns for the specified
   #    measure, using the data file as a source.
   # Args:
   #   dfile: the handle to the read file
   #   str_measure: the name of the measure type that should be returned
   # Returns:
   #   a data.frame with several columns, including rates, normalized rates,
   #      scaled rates, 
   #      and a key string locationyear which represents the state, county, and year
   #              for each row (used later to join other measures

   # Columns in data file:
   #    State,County,State.code,County.code,Year.span,
   #    Measure.name,Measure.id,Numerator,Denominator,
   #    Raw.value,Confidence.Interval.Lower.Bound,
   #    Confidence.Interval.Upper.Bound,Data.Release.Year,fipscode


   # Get the rows that represent county-specific (not county, not all US) data 
   # (to avoid duplicating the use of data points) where the measure is str_measure
   # Remove PR since limited records
   targetrows <- which( dfile$County.code!=0 & 
                dfile$Raw.value!="" &  #skip rows without rates
                dfile$State!="" &  #there were 2 rows without state
                dfile$State!="PR" &  #PR has only 1 year, 1 measure: 2012 Unemployment data 
                dfile$Measure.name==str_measure & 
                dfile$Year.span >= 2002)   #only premature death data exists before 2002
   # Get the location variables 
   state <- dfile$State[targetrows]  # the 2-letter state code 
   county_code <- dfile$County.code[targetrows]  # a code representing the county 
                                                 # (unique within each state)

   # RATE: represents the rate, but some have specific qualifications: 
   #         Premature Death:  per 100,000 population
   #         Preventable hospital stays:  per 1,000 fee-for-service Medicare enrollees
   #         Violent crime rate: per 100,000 population
   rate <- dfile$Raw.value[targetrows]
   rate_qualifier <- "Rate"
   if (str_measure=="Preventable hospital stays") {
       rate_qualifier <- "per 1000 FFS Medicare enrollees"
   } else if (is.element(str_measure, c("Violent crime rate", "Premature Death"))) {
       rate_qualifier <- "per 100,000"
   }

   # Set yearspan as character information, since it will be handled
   # as a string to define the primary key
   yearspan <- as.character(dfile$Year.span[targetrows])

   #create the location key with state and county code for variables that
   #represent years differently (some have 1 year, others 2 or 3 year ranges)
   location  <- paste(state, county_code, yearspan,  sep = '-') 

   #create the location key with state, county code, and year
   locationyear  <- paste(state, county_code, yearspan,  sep = '-') 


   # After reviewing histogram and QQNorm, 
   # a few variables were found to be skewed.
   # bit of a right skew: children in poverty, uninsured,obesity
   # big right skew: premature death, preventable hospital stays, unemployment, violent crime

   # set the default normalized rate to the raw value rate
   ratenorm <- rate
   # transform rates as needed to approximate normalized distribution
   if (is.element(str_measure, c("Children in poverty", "Uninsured","Adult obesity"))) {
      # Get normalized data for variables with slight right skews using sqrt():
      ratenorm <- sqrt(rate)
      # Visually inspect the transformed data points
      PlotHistAndQQNormLog ( ratenorm,str_measure,rate_qualifier="rate",type="Square Root") 
   }
   else if (is.element(str_measure, c("Violent crime rate", "Unemployment", 
                          "Preventable hospital stays", "Premature Death"))) {
      # Get normalized data for variables with large right skews using log():
      # Note: log(0) gives -Inf, log(NA) gives NA
      # for the log transformation, replace any 0 values with NA
      temp_vec <- rate 
      temp_vec [temp_vec==0] <- NA  
      ratenorm <- log (temp_vec)
      # Visually inspect the transformed data points for normal distribution
      PlotHistAndQQNormLog ( ratenorm,str_measure, rate_qualifier,type="Log") 
   }

   # get the scaled rate, for use in a chart that shows all measures together
   ratescaled <- scale(rate)
   
   # get the vectors for division and region
   division <- getDivisions (state)
   region <- getRegions(division)

   #create a dataframe with rates and keys
   retval <- data.frame(rate, ratenorm, ratescaled,location, 
                locationyear,yearspan,region,division)
   #create a measure-specific label for the rate column (replacing spaces with _)
   ratelabel <- gsub(" ","_", str_measure)
   #label the columns, naming the specific rate in the rate, 
   #    normalized rate, and scaled rate
   colnames(retval) <- c(ratelabel, paste(ratelabel,"norm",sep=""),
                          paste(ratelabel,"scaled",sep=""),
                          paste(ratelabel,"location",sep=""),
                          "locationyear","yearspan","region","division")
   #Draw plots to check for normal distribution in the original rate
   PlotHistAndQQNorm (rate,str_measure,rate_qualifier)
   return (retval)
}

getAnovaSummaryForMeasuresByLocation <- function() {
########################################################
#  This function is not called within the body of code.
#  It was run manually to check the ANOVA for each 
#  measure by region and by division.

# check the ANOVA between childpoverty and location
df=childpoverty
childpoverty.region.anova <- aov (df[[COL_RATENORM]]~df[[COL_REGION]],data=df)
childpoverty.division.anova <- aov (df[[COL_RATENORM]]~df[[COL_DIVISION]],data=df)
summary(childpoverty.region.anova )[[1]][[5]][[1]]
summary(childpoverty.division.anova )[[1]][[5]][[1]]

# check the ANOVA between prematuredeath and location
df=prematuredeath
prematuredeath.region.anova <- aov (df[[COL_RATENORM]]~df[[COL_REGION]],data=df)
prematuredeath.division.anova <- aov (df[[COL_RATENORM]]~df[[COL_DIVISION]],data=df)
summary(prematuredeath.region.anova )[[1]][[5]][[1]]
summary(prematuredeath.division.anova )[[1]][[5]][[1]]

# check the ANOVA between adultobesity and location
df=obesity
obesity.region.anova <- aov (df[[COL_RATENORM]]~df[[COL_REGION]],data=df)
obesity.division.anova <- aov (df[[COL_RATENORM]]~df[[COL_DIVISION]],data=df)
summary(obesity.region.anova )[[1]][[5]][[1]]
summary(obesity.division.anova )[[1]][[5]][[1]]

# check the ANOVA between inactivity and location
df=inactivity
inactivity.region.anova <- aov (df[[COL_RATENORM]]~df[[COL_REGION]],data=df)
inactivity.division.anova <- aov (df[[COL_RATENORM]]~df[[COL_DIVISION]],data=df)
summary(inactivity.region.anova )[[1]][[5]][[1]]
summary(inactivity.division.anova )[[1]][[5]][[1]]

# check the ANOVA between preventable hospitalizations and location
df=preventablehosp
preventablehosp.region.anova <- aov (df[[COL_RATENORM]]~df[[COL_REGION]],data=df)
preventablehosp.division.anova <- aov (df[[COL_RATENORM]]~df[[COL_DIVISION]],data=df)
summary(preventablehosp.region.anova )[[1]][[5]][[1]]
summary(preventablehosp.division.anova )[[1]][[5]][[1]]

# check the ANOVA between uninsured and location
df=uninsured
uninsured.region.anova <- aov (df[[COL_RATENORM]]~df[[COL_REGION]],data=df)
uninsured.division.anova <- aov (df[[COL_RATENORM]]~df[[COL_DIVISION]],data=df)
summary(uninsured.region.anova )[[1]][[5]][[1]]
summary(uninsured.division.anova )[[1]][[5]][[1]]

# check the ANOVA between unemployed and location
df=unemployed
unemployed.region.anova <- aov (df[[COL_RATENORM]]~df[[COL_REGION]],data=df)
unemployed.division.anova <- aov (df[[COL_RATENORM]]~df[[COL_DIVISION]],data=df)
summary(unemployed.region.anova )[[1]][[5]][[1]]
summary(unemployed.division.anova )	[[1]][[5]][[1]]
}

########################################################
# start the main body of this R file
# Upload the data file
# read the file, bringing all strings in as default
county_health_datafile <- read.table(file=SOURCE_FILE,
                         header=TRUE,sep=",",stringsAsFactors=FALSE)


# set the dataframe for each of the measures that will be inspected
obesity <- getCountyRowsByMeasureDf(county_health_datafile, "Adult obesity")
inactivity <- getCountyRowsByMeasureDf(county_health_datafile, "Physical inactivity")
violentcrime <- getCountyRowsByMeasureDf(county_health_datafile, "Violent crime rate")
unemployed <- getCountyRowsByMeasureDf(county_health_datafile, "Unemployment") 
childpoverty<- getCountyRowsByMeasureDf(county_health_datafile, "Children in poverty")
uninsured <- getCountyRowsByMeasureDf(county_health_datafile, "Uninsured")
preventablehosp <- getCountyRowsByMeasureDf(county_health_datafile, "Preventable hospital stays")
prematuredeath <- getCountyRowsByMeasureDf(county_health_datafile, "Premature Death")

# Set up a "pivot table" or "cross tab" data frame that
#   holds normed rate for each measure using location year as a key
combined.df <- merge(data.frame( inactivity.norm=inactivity[[COL_RATENORM]], 
                                 locationyear=inactivity[[COL_LOCN_YEAR]]),
                     data.frame( obesity.norm=obesity[[COL_RATENORM]], 
                                 locationyear=obesity[[COL_LOCN_YEAR]] ), 
                                 by="locationyear", all=TRUE)
combined.df <- merge(combined.df,data.frame( uninsured.norm=uninsured[[COL_RATENORM]] , 
                                 locationyear=uninsured[[COL_LOCN_YEAR]] ), 
                                 by="locationyear", all=TRUE)
combined.df <- merge(combined.df,data.frame( unemployed.norm=unemployed[[COL_RATENORM]] , 
                                 locationyear=unemployed[[COL_LOCN_YEAR]] ), 
                                 by="locationyear", all=TRUE)
combined.df <- merge(combined.df,data.frame( childpoverty.norm=childpoverty[[COL_RATENORM]] , 
                                 locationyear=childpoverty[[COL_LOCN_YEAR]] ), 
                                 by="locationyear", all=TRUE)
combined.df <- merge(combined.df,data.frame( preventablehosp.norm=preventablehosp[[COL_RATENORM]], 
                                 locationyear=preventablehosp[[COL_LOCN_YEAR]] ), 
                                 by="locationyear", all=TRUE)
combined.df <- merge(combined.df,data.frame( prematuredeath.norm=prematuredeath[[COL_RATENORM]], 
                                 locationyear=prematuredeath[[COL_LOCN_YEAR]] ), 
                                 by="locationyear", all=TRUE)
combined.df <- merge(combined.df,data.frame( violentcrime.norm=violentcrime[[COL_RATENORM]], 
                                 locationyear=violentcrime[[COL_LOCN_YEAR]] ), 
                                 by="locationyear", all=TRUE)
#summary(lm(preventablehosp.norm ~ violentcrime.norm,data=combined.df))
#summary(lm(preventablehosp,norm ~ obesity.norm,data=combined.df))

# Create Plots based on the data
# Draw paired plots to inspect the interactions of variables
drawPairedPlots() 
     
#Draw smooth scatter plots for each of the variables being evaluated
drawSmoothScatter("prematuredeath.norm","preventablehosp.norm",combined.df,
                   "Premature Death","Preventable Hospitalization")
drawSmoothScatter("uninsured.norm","unemployed.norm",combined.df,
                   "Uninsured","Unemployment")
drawSmoothScatter("inactivity.norm","unemployed.norm",combined.df,
                   "Physical Inactivity","Unemployment")
drawSmoothScatter("uninsured.norm","preventablehosp.norm",combined.df,
                   "Uninsured","Preventable Hospitalization")
drawSmoothScatter("obesity.norm","unemployed.norm",combined.df,
                   "Adult Obesity","Unemployment")
drawSmoothScatter("obesity.norm","inactivity.norm",combined.df,
                   "Adult Obesity","Physical Inactivity")
drawSmoothScatter("obesity.norm","childpoverty.norm",combined.df,
                   "Adult Obesity","Childhood Poverty")
drawSmoothScatter("inactivity.norm","childpoverty.norm",combined.df,
                   "Physical Inactivity","Childhood Poverty")
drawSmoothScatter("uninsured.norm","childpoverty.norm",combined.df,
                   "Uninsured","Childhood Poverty")
drawSmoothScatter("unemployed.norm","childpoverty.norm",combined.df,
                   "Unemployment","Childhood Poverty")
drawSmoothScatter("preventablehosp.norm","childpoverty.norm",combined.df,
                   "Preventable Hospitalization","Childhood Poverty")
drawSmoothScatter("prematuredeath.norm","violentcrime.norm",combined.df,
                   "Premature Death","Violent Crime")


# Draw 2 boxplots: 1 factored by region, 1 factored by division
drawBoxplotMeasureByRegionDivision(childpoverty, "Children_in_poverty", 
                                   "Children in Poverty") 
drawBoxplotMeasureByRegionDivision(prematuredeath, "Premature_Death", 
                                   "Premature Death") 
drawBoxplotMeasureByRegionDivision(obesity, "Adult_obesity", 
                                   "Adult Obesity") 
drawBoxplotMeasureByRegionDivision(inactivity, "Physical_inactivity", 
                                   "Physical Inactivity") 
drawBoxplotMeasureByRegionDivision(preventablehosp, "Preventable_hospital_stays", 
                                   "Preventable Hospital Stays") 
drawBoxplotMeasureByRegionDivision(uninsured, "Uninsured", 
                                   "Uninsured") 
drawBoxplotMeasureByRegionDivision(unemployed, "Unemployment", 
                                   "Unemployment") 
# Skip violent crime, since there was a caution against comparing
# between states
#drawBoxplotMeasureByRegionDivision(violentcrime, "Violent_crime_rate", "Violent Crime Rate") 



#Draw composite plots of each of the measures, organized by region-division
   # first, get a dataframe to support the graph:
   combo.scaled.df <- getScaledRegionDivisionDataFrame (continentalOnly=TRUE)
   combo.aggr <- aggregate (.~measure+division,combo.scaled.df,mean)
   

   drawBoxplotMeasureByRegionDivision(childpoverty, "Children_in_povertyscaled", 
                                      "Children in Poverty (scaled)") 
   drawBoxplotMeasureByRegionDivision(prematuredeath, "Premature_Deathscaled", 
                                      "Premature Death (scaled)") 
   drawBoxplotMeasureByRegionDivision(obesity, "Adult_obesityscaled", 
                                      "Adult Obesity (scaled)") 
   drawBoxplotMeasureByRegionDivision(inactivity, "Physical_inactivityscaled", 
                                      "Physical Inactivity (scaled)") 
   drawBoxplotMeasureByRegionDivision(preventablehosp, "Preventable_hospital_staysscaled", 
                                      "Preventable Hospital Stays (scaled)") 
   drawBoxplotMeasureByRegionDivision(uninsured, "Uninsuredscaled", 
                                      "Uninsured (scaled)") 
   drawBoxplotMeasureByRegionDivision(unemployed, "Unemploymentscaled", 
                                      "Unemployment (scaled)") 
   
   
   #Draw boxplots
library(ggplot2)
fname <- paste(DATA_DIR, "graphs", "boxplot_measures_by_division.png", sep="/")
# Open the new PNG file
png(filename=fname,width=700,height=400)
#look at plotting each scaled version of data separately?
ggplot(data = combo.scaled.df , 
         aes(x=division, y=rate,group=measure, colour=measure)) + 
   geom_boxplot(outlier.shape=8) + 
   xlab("Region and Division") + ylab("Scaled Rate") +
   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + labs(colour="Health Measures")+
   ggtitle("Scaled Rates for Measures, Averaged over Census Location Divisions")
dev.off()

#Draw dotplots of scaled values 
   # load the ggplot2 library
   library(ggplot2)
   # draw a line plot with scaled rates on the y axis, regions and divisions on the x-axis,
   # and colors representing measures
   fname <- paste(DATA_DIR, "graphs", "dot_graph_measures_by_division.png", sep="/")
   # Open the new PNG file
   png(filename=fname,width=700,height=400)

   # Draw Dot plots
   ggplot(data = combo.aggr , aes(x=division, y=rate,group=measure, colour=measure)) + 
      geom_point(shape = 15, size = 2)+ xlab("Region and Division") + ylab("Scaled Rate")+
      ggtitle("Scaled Rates for Measures, Averaged over Census Location Divisions")  + 
      labs(colour="Health Measures") + geom_hline(yintercept = 0,color="gray") + theme(
         panel.background = element_blank(),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.key = element_rect(fill = "white"),
         axis.line = element_line(colour = "black"),
         axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
   
   dev.off()

#Now, draw it with only continent US states (exclude HI and AK)
   # first, get a dataframe to support the graph:
   combo.scaled.df <- getScaledRegionDivisionDataFrame (continentalOnly=TRUE)
   combo.aggr <- aggregate (.~measure+division,combo.scaled.df,mean)
   # load the ggplot2 library
   library(ggplot2)
   # draw a line plot with scaled rates on the y axis, regions and divisions on the x-axis,
   # and colors representing measures
   fname <- paste(DATA_DIR, "graphs", "line_graph_measures_by_division_no_HI_AK.png", sep="/")
   # Open the new PNG file
   png(filename=fname,width=700,height=400)
   ggplot(data = combo.aggr , aes(x=division, y=rate,group=measure, colour=measure)) + 
        geom_line() + geom_point()+ xlab("Region and Division") + ylab("Scaled Rrate")+
        theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) + labs(colour="Health Measures")+
        ggtitle("Scaled Rates for Measures Over Census Location Divisions: Continental States")
   dev.off()



# Print the 3D graph for obesity, inactivity, and childhood poverty
#  -- the three with highest rsquaredvalues
library("scatterplot3d")
fname <- paste(DATA_DIR, "graphs", "scatter3d_obesity_inactivity_childpov.png", sep="/")
png(filename=fname,width=700,height=400)
scatterplot3d(x=combined.df$obesity.norm, y=combined.df$inactivity.norm,
       z=combined.df$childpoverty.norm,highlight.3d=TRUE, tick.marks=FALSE,type="h",
       lty.hplot=3,lty.hide=3, xlab="Adult Obesity",ylab="Physical Inactivity", angle=35 ,
       zlab="Childhood Poverty",main="Adult Obesity, Hospital Stays, and Childhood Poverty")
dev.off()

# Print the same 3 variables in 2D scatter + color plot
# Set up for extraction to plotting function
df_for_plot <- combined.df
col_surv <- na.omit(df_for_plot[,c("obesity.norm","inactivity.norm","childpoverty.norm")])
#get the colors for the plot
pal <- colorRampPalette(colors=c("yellow2","red"))
# set up 5 colors on the gradient
k <- 5
ryc <- pal(k)
#set the breaks
breaks_for_plot <- seq(min(col_surv$childpoverty.norm),
                   max(col_surv$childpoverty.norm),length=k+1)
fac <- cut(col_surv$childpoverty.norm,breaks=breaks_for_plot,include.lowest=TRUE)
cols_for_plot <- ryc[as.numeric(fac)]

fname <- paste(DATA_DIR, "graphs", "scatter_color_obesity_inactivity_childpov.png", sep="/")
png(filename=fname,width=700,height=400)
plot (df_for_plot$obesity.norm~df_for_plot$inactivity.norm,
       main="Obesity, Inactivity, and Childhood Poverty Rates, by County",
       xlab="Adult Obesity Rate",ylab="Physical Inactivity Rate",
       col=alpha(cols_for_plot,0.4),  pch=19,xaxt='n',yaxt='n')
legend("bottomright",col=ryc,pch=19,title="Childhood Poverty",
       legend=c("low",".",".",".","high"))
dev.off()
 
