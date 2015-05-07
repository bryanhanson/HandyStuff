##' Calculate Custom Summary Statistics
##'
##' This function computes a selection of summary statistics on a data frame which
##' contains response variables and categorical variables.  If there is only one
##' response variable, the results are returned in a data frame.  For more than one
##' response variable, the results are returned in a list of data frames.  See the examples.
##'
##' @param data A data frame containing the variables.
##'
##' @param groupVars Character.  A vector of the categorical variable names.
##'
##' @param respVars Character.  A vector of the response variable names.
##'
##' @param ... Other parameters to be passed downstream.
##'
##' @param ci Numeric.  The confidence interval desired.
##'
##' @return Either a data frame or a list of data frames.
##' 
##' @name sumStatDeluxe
##' @rdname sumStatDeluxe
##' @export
##' @keywords summary
##'
##' @examples
##' require('plyr')
##' #
##' ### One response variable returns a data frame:
##' #
##' tst <- sumStatDeluxe(chickwts, groupVars = "feed", respVars = "weight")
##' tst
##' #
##' ### Two response variables returns a list of data frames:
##' #
##' tst <- sumStatDeluxe(iris, groupVars = "Species",
##' respVars = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
##' tst
##' #
##' ### Two categorical variables, one response:
##' #
##' tst <- sumStatDeluxe(warpbreaks, groupVars = c("wool", "tension"), respVars = "breaks")
##' # One categorical, multiple response variables:
##' tst <- sumStatDeluxe(airquality, groupVars = "Month",
##' respVars = c("Ozone", "Solar.R", "Wind"))
##' tst
##'
sumStatDeluxe <- function(data = NULL, groupVars = NULL, respVars = NULL, ci = 0.95, ...) {
	# read ?var carefully for role of na.rm - it must always be true for numeric vectors
	
	# local function to compute std error
	se <- function(x) { sd(x, na.rm = TRUE)/sqrt(length(na.omit(x))) }
	
	# local function to compute lower ci of mean
	Lci <- function(x) {
		m <- mean(na.omit(x))
		s <- se(x)
		f <- qt(ci/2 + .5, length(na.omit(x)))
		l <- m - f*s
		}

	# local function to compute upper ci of mean
	Uci <- function(x) {
		m <- mean(na.omit(x))
		s <- se(x)
		f <- qt(ci/2 + .5, length(na.omit(x)))
		l <- m + f*s
		}
	
	# local function - acts on a vector, returns a vector
	processVec <- function(df, i) {
		M <- mean(df[,i], na.rm = TRUE)
		S <- sd(df[,i], na.rm = TRUE)
		SE <- se(df[,i])
		N <- length(na.omit(df[,i]))
		L <- Lci(df[,i])
		U <- Uci(df[,i])
		res <- c(N, round(S, 2), round(SE,2),
			round(L, 2), round(M, 2), round(U,2))
		}

	# local function - acts on df, returns a df
	# used when length(respVars >= 2)
	processDF <- function(df, respVars) {
		df <- df[,respVars]
		ans <- processVec(df, 1)
		for (n in 2:ncol(df)) {
			ans <- rbind(ans, processVec(df, n))
			}
		ans <- as.data.frame(ans)
		rownames(ans) <- names(df)
		colnames(ans) <- c("N", "StdDev", "StdError", "lowerCI", "mean", "upperCI")
		return(ans)
		}
	
	if (length(respVars) == 1) {
		DF <- plyr::ddply(data, groupVars, processVec, respVars, ...)
		names(DF) <- c(groupVars, "N", "StdDev", "StdError", "lowerCI", "mean", "upperCI")
		return(DF)
		}

	if (length(respVars) >= 2) {
		L <- plyr::dlply(data, groupVars, processDF, respVars, ...)
		return(L)
		}
	
	}
