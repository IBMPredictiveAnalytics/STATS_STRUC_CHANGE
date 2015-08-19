"STATS STRUC CHANGE extension command"

#Licensed Materials - Property of IBM
#IBM SPSS Products: Statistics General
#(c) Copyright IBM Corp. 2014
#US Government Users Restricted Rights - Use, duplication or disclosure 
#restricted by GSA ADP Schedule Contract with IBM Corp.


helptext = "STATS STRUC CHANGE DEPENDENT=dependent variable
[INDEPENDENT=independent variables]
[FLUCTESTS = {CUSUMREC*  CUSUMOLS  MOSUMREC  MOSUMOLS  RE  ME}]
[CHOW={NO*|YES} FROM=start TO=end] 
[BANDWIDTH = fraction]
[/OPTIONS PLOT={YES*|NO} SCTEST={YES*|NO} ALPHA=confidencelevel INCLUDELAG={NO*|YES}
START = start PERIODICITY=periodicity
[/HELP].

Test for structural change in model using one or more fluctuation tests or Chow tests.

Example:
STATS STRUC CHANGE DEPENDENT=y INDEPENDENT=x1 x2 x3
TESTS=CUSUMREC CUSUMOLS
/OPTIONS PLOT=YES SCTEST=YES.

Example with CHOW test and time series
STATS STRUC CHANGE DEPENDENT=y INDEPENDENT=x1 x2 x3
CHOW = YES FROM=2000 2 TO=2002 4
/OPTIONS START = 1995 4 PERIODICITY=4.


DEPENDENT and INDEPENDENT name the variables in the linear model.
Categorical variables are automatically converted to R factors.
If no independent variables are named, the tests are based on the dependent
variable alone, i.e., it is as if that variable were regressed on a a constant.

FLUCTESTS names one or more tests for structural change.  The default is CUSUMREC.
The tests are as follows.
The first four calculate an empirical process of sums of residuals.  They
are based on recursive residuals or OLS residuals (REC or OLS)
The sums are either cumulative or for a moving data window (CUSUM or MOSUM).
The remaining two are based on recursive estimates of the regression
coefficients or moving OLS estimates.

CHOW specifies a CHOW test.  If used, the FROM and TO parameters must be specified.
The CHOW F statistic is calculated for each point between FROM and TO, and a test
is performed based on the maximum F statistic.  In the associated plot, the labeling on
the x axis shows the second component in a date as the fraction of the period.  That is,
with quarterly data rather than seeing 1,2,3,4 as the second component, you might see
0, .25., .50, .75.

The bandwidth determines the size of the window for moving residuals or estimates.
it is expressed as a fraction between 0 and 1 and defaults to .15, i.e., 15% of the
sample.

PLOT and SCTEST determine whether plots and test statistics are displayed.
By default, both are displayed.  PLOT=NO SCTEST=NO is an error as there
would be no output.

ALPHA determines the size of the confidence intervals shown in the plots.
It defaults to .05.

INCLUDELAG determines whether the lagged dependent variable is included
in the equation.

For time series data, START can specify the initial date as a number, 
e.g., 2000, or pair of numbers, e.g., 2000 4, and PERIODICITY specifies the periodicity.  
For example,
START=2000 3 PERIODICITY=12 would define monthly data starting in
year 2000, month 3.  This information is used only for labelling the plots
and interpreting the FROM and TO parameters.

For CHOW tests, FROM and TO determine the possible location of a structural change.
The values can be specified as dates similar to the START
parameter.  For time series, these can have one or two components.
If FROM and TO are less than 1, they are interpreted as fractions of the sample.
For example, if there are N cases and FROM = .2, the starting value is case N*.2
If data are not declared as time series, FROM and TO are interpreted as case
numbers if they are integers.

/HELP displays this text and does nothing else.
"

# author=  'jkp, IBM'
# version=  '1.0.1'
# history
# 10-29-2012 original version
# 05-20-2014 fix false require failure

# override for api to account for extra parameter in V19 and beyond
StartProcedure <- function(procname, omsid) {
    if (substr(spsspkg.GetSPSSVersion(),1, 2) >= 19) {
       spsspkg.StartProcedure(procname, omsid)
    }
    else {
       spsspkg.StartProcedure(omsid)
    }
}


strucch <- function(dep, indep=NULL, thetests=list(), 
		chow=FALSE, chowfrom=NULL, chowto=NULL, 
		bandwidth, plotit=TRUE, testit=TRUE, alpha=.05, 
    includelag=FALSE, start=NULL, periodicity=NULL) {

    setuplocalization("STATS_STRUC_CHANGE")
    
    if (xor(is.null(start), is.null(periodicity))) {
            stop("Eith both starting date and periodicity must be specified or neither")
    }
    if (!(plotit || sctest)) {
            stop("Neither plots nor tests were requested, so no output is produced")
    }
    if (chow && (is.null(chowfrom) || is.null(chowto))) {
            stop(gtxt("Chow tests were specified without starting and ending cases or proportions"), call.=FALSE)
    }
    if (chow) {
            thetests = append(thetests, "chow")
            # for time series, from and to must be duples  (c will flatten if necessary)
            if (length(chowfrom) == 1 && !is.null(start))
                    chowfrom = c(chowfrom,1)
            if (length(chowto) == 1 && !is.null(start))
                    chowto = c(chowto, 1)
            strfrom = paste(chowfrom, collapse=":")
            strto = paste(chowto, collapse=":")
            if (!is.null(periodicity) && (chowfrom[[2]] > periodicity || chowto[[2]] > periodicity)) {
                    stop(gtxt("The starting or ending date for a Chow test exceeds the specified periodicity of the data"), call.=FALSE)
            }
    }
    if (length(thetests) == 0) {
            stop(gtxt("No tests were specified"), call.=FALSE)
    }


    tryCatch(library(strucchange), error=function(e) {
        stop(gtxt("The R strucchange package is required but could not be loaded."), call. = FALSE)})
        
		if (!is.null(indep)) { 
				allvars = c(dep, indep)
		}
		else {
				allvars = dep
		}
		dta <- spssdata.GetDataFromSPSS(allvars, missingValueToNA = TRUE, factorMode = "labels")
		if (!is.null(start)) {
				dta <- ts(dta, start=start, frequency=periodicity)

		}
		if (is.null(indep)) {   # constant term only
				model = paste(dep, 1, sep="~")
		}
		else {
				rhs = paste(indep, collapse="+")
				model = paste(dep, rhs, sep="~")
		}
		kwds =list("cusumrec","cusumols","mosumrec","mosumols","re","me", "chow")
		rkwds=list("Rec-CUSUM", "OLS-CUSUM", "Rec-MOSUM", "OLS-MOSUM", "RE", "ME", "chow")
		tlabels=list(gtxt("Recursive CUSUM"), gtxt("OLS-Based CUSUM"), gtxt("Recursive MOSUM"),
		  gtxt("OLS-Based MOSUM"), gtxt("Recursive Estimates"), gtxt("Moving Estimates"), gtxt("Sup-F Chow"))
		ntests = length(thetests)
		m = matrix(nrow=ntests, ncol=3)
		i = 0
		StartProcedure(gtxt("Structural Change Tests"), "STATS STRUC CHANGE") 
		for (tst in thetests) {
				i = i + 1
				slot = match(tst, kwds)
				tst = rkwds[[slot]]
				lbl = tlabels[[slot]]
				if (tst != "chow") {
						result = tryCatch(efp(as.formula(model), type=tst, data=dta, h=bandwidth, dynamic=includelag),
								error = function(e) {return(NaN)})
						if (length(result) == 1 && is.nan(result)) {
								print(sprintf(gtxt("Test cannot be computed: %s"), lbl))
								m[i,1] = lbl
								m[i,2] = NA
								m[i,3] = NA
								next
						}
				ylab = gtxt("Empirical Fluctuation Process")
				}
				else {
				result = tryCatch(Fstats(as.formula(model), from=chowfrom, to=chowto, data=dta), 
						error = function(e) {return(NaN)})
				if (length(result) == 1 && is.nan(result)) {
						print(gtxt("Chow tests cannot be computed"))
						m[i,1] = lbl
						m[i,2] = NA
						m[i,3] = NA
						next
				}
				ylab = gtxt("F Statistics")
				lbl = paste(lbl, gtxt("from="), strfrom, gtxt("to="), strto)
				}
				if (plotit) {
						thetitle = paste(gtxt("Model:", model, lbl))
						tryCatch(plot(result, alpha = alpha, ylab = ylab,
								main = thetitle), error=function(e) {print(paste(lbl, gtxt("Plot cannot be produced")))})
				}
				if (testit) {
						tresult = tryCatch(sctest(result), error=function(e) {return(NaN)})
						if (length(tresult) == 1 && is.nan(tresult)) {
								m[i,1] = lbl
								m[i,2] = NA
								m[i,3] = NA
						} else {
						m[i,1] = tresult$method
						m[i,2] = round(tresult$statistic,4)
						m[i,3] = round(tresult$p.value,5)
						}
				}
		}
		testresults = data.frame(m[,2], m[,3], row.names=m[,1])
		spsspivottable.Display(testresults, title = gtxt("Structural Change Tests"),
		format=formatSpec.Coefficient,
				collabels=c(gtxt("Test Statistic"), gtxt("P-Value")), templateName="STRUCCHANGETESTS", outline=gtxt("Structural Change Tests"),
				caption = paste(gtxt("Model:"), model))
		spsspkg.EndProcedure()
		# clean up workspace
    tryCatch(rm(list=ls()), warning = function(e) {return(NULL)})
}

# localization initialization
setuplocalization = function(domain) {
    # find and bind translation file names
    # domain is the root name of the extension command .R file, e.g., "SPSSINC_BREUSCH_PAGAN"
    # This would be bound to root location/SPSSINC_BREUSCH_PAGAN/lang

    fpath = Find(file.exists, file.path(.libPaths(), paste(domain, ".R", sep="")))
    bindtextdomain(domain, file.path(dirname(fpath), domain, "lang"))
} 

gtxt <- function(...) {
		return(gettext(...,domain="STATS_STRUC_CHANGE"))
	}

gtxtf <- function(...) {
		return(gettextf(...,domain="STATS_STRUC_CHANGE"))
	}
	
Run <- function(args) {
    #Execute the STATS STRUC CHANGE extension command

    cmdname = args[[1]]
    args = args[[2]]
    oobj = spsspkg.Syntax(list(
        spsspkg.Template("DEPENDENT", subc="",  ktype="existingvarlist", var="dep", islist=FALSE),
        spsspkg.Template("INDEPENDENT", subc="", ktype="existingvarlist", var="indep", islist=TRUE),
        spsspkg.Template("FLUCTESTS", subc="",  ktype="str", var="thetests", 
            vallist=list("cusumrec","cusumols","mosumrec","mosumols","re","me"), islist=TRUE),
				spsspkg.Template("CHOW", subc="", ktype="bool", var="chow"),
				spsspkg.Template("FROM", subc="", ktype="float", var="chowfrom", islist=TRUE),
				spsspkg.Template("TO", subc="", ktype="float", var="chowto", islist=TRUE),
        spsspkg.Template("BANDWIDTH", subc="", ktype="float", var="bandwidth"),
        spsspkg.Template("PLOT", subc="OPTIONS", ktype="bool", var="plotit"),
        spsspkg.Template("SCTEST", subc="OPTIONS", ktype="bool", var="testit"),
        spsspkg.Template("ALPHA", subc="OPTIONS", ktype="float", var="alpha"),
        spsspkg.Template("INCLUDELAG", subc="OPTIONS", ktype="bool", var="includelag"),
        spsspkg.Template("START", subc="OPTIONS", ktype="int", var="start", islist=TRUE),
        spsspkg.Template("PERIODICITY", subc="OPTIONS", ktype="int", var="periodicity"),
        spsspkg.Template("HELP", subc="", ktype="bool")
				))
        

    # A HELP subcommand overrides all else
    if ("HELP" %in% attr(args,"names")) {
        #writeLines(helptext)
        helper(cmdname)
    } else {
        res <- spsspkg.processcmd(oobj, args, "strucch")
    }
}

helper = function(cmdname) {
    # find the html help file and display in the default browser
    # cmdname may have blanks that need to be converted to _ to match the file
    
    fn = gsub(" ", "_", cmdname, fixed=TRUE)
    thefile = Find(file.exists, file.path(.libPaths(), fn, "markdown.html"))
    if (is.null(thefile)) {
        print("Help file not found")
    } else {
        browseURL(paste("file://", thefile, sep=""))
    }
}
if (exists("spsspkg.helper")) {
assign("helper", spsspkg.helper)
}