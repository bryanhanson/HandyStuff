##' Draw a QQ Plot using ggplot2
##'
##' This function creates a QQ plot (normal probability plot), including a reference line,
##' using ggplot2 graphics.  The reference line is based on the normal distribution.
##' The plot may be faceted.
##'
##' @param dataframe A data frame containing the variables to be plotted.  \code{NA}  will
##' be omitted by this function.
##'
##' @param respvar Character.  The name of the response variable in the data frame.
##'
##' @param facetvars Character.  One or two names of the variables to be used for facetting.
##' 
##' @return A ggplot2 object.  This can be printed by \code{print(object)} or just \code{object}.
##' 
##' @name ggqq_norm
##' @rdname ggqq_norm
##' @export
##'
##' @keywords plot
##'
##' @examples
##' #
##' ### Set up test data
##' #
##' set.seed(123)
##' require("ggplot2")
##' ns <- 200
##' #
##' ### Normally distributed data
##' #
##' td1 <- data.frame(resp = rnorm(ns), f1 = sample(c("A", "B"), ns, replace = TRUE),
##' f2 = sample(c("C", "D"), ns, replace = TRUE))
##' res <- ggqq_norm(td1, "resp", c("f1", "f2"))
##' print(res + labs(title = "Normal Distribution"))
##' #
##' ### Normally distributed data w/an outlier
##' #
#' td1 <- data.frame(resp = rnorm(ns), f1 = sample(c("A", "B"), ns, replace = TRUE),
##' f2 = sample(c("C", "D"), ns, replace = TRUE))
##' bad <- sample(1:ns, 4)
##' td1$resp[bad[1:2]] <- 5
##' td1$resp[bad[3:4]] <- -5
##' res <- ggqq_norm(td1, "resp", c("f1", "f2"))
##' print(res + labs(title = "Normal Distribution + Outliers"))
##' #
##' ### Uniformly distributed data
##' #
##' td2 <- data.frame(resp = runif(ns), f1 = sample(c("A", "B"), ns, replace = TRUE),
##' f2 = sample(c("C", "D"), ns, replace = TRUE))
##' res <- ggqq_norm(td2, "resp", c("f1", "f2"))
##' print(res + labs(title = "Uniform Distribution"))
##' 
ggqq_norm <- function(dataframe = NULL, respvar = NULL, facetvars = NULL) {

	if (!requireNamespace("ggplot2", quietly = TRUE)) {
		stop("You need to install package ggplot2 to use this function")
		}

	# preliminaries...
	if (is.null(dataframe)) stop("You must provide a data frame")
	if (is.null(respvar)) stop("You must provide a response variable (respvar)")
	if (is.null(facetvars)) stop("You must provide variables for faceting (facetvars)")
	no.fac <- length(facetvars)
	if (no.fac > 2) stop("Maximum 2 facetvars allowed")
	df1 <- dataframe[c(respvar, facetvars)]
	df1 <- stats::na.omit(df1)

	if (no.fac == 1) fstring <- paste(".~", facetvars[1], sep = "")
	if (no.fac == 2) fstring <- paste(facetvars[1], facetvars[2], sep = "~")
	
	# compute the theoretical quantiles
	df2 <- stats::aggregate(x = df1[respvar], by = as.list(df1[facetvars]), FUN =
		function(x){
			q <- stats::qqnorm(x, plot = FALSE)$x
			q
			})
	
	# reorganize the not so helpful aggregate output
	df1$theoretical <- NA_real_
	f1.lvls <- levels(unlist(df1[facetvars[1]]))
	
	if (no.fac == 1) {
		for (i in 1:length(f1.lvls)) {
			tmp <- which(df1[facetvars[1]] == f1.lvls[i])
			df1$theoretical[tmp] <- unlist(df2[[respvar]][i])
			}
		}
		
	if (no.fac == 2) {
		f2.lvls <- levels(unlist(df1[facetvars[2]]))
		no.f1 <- length(f1.lvls)
		no.f2 <- length(f2.lvls)
		ind.mat <- matrix(1:(no.f1*no.f2), nrow = no.f1)
		for (i in 1:length(f1.lvls)) {
			for (j in 1:length(f2.lvls)) {
				tmp1 <- which(df1[facetvars[1]] == f1.lvls[i])
				tmp2 <- which(df1[facetvars[2]] == f2.lvls[j])
				tmp3 <- intersect(tmp1, tmp2)
				df1$theoretical[tmp3] <- unlist(df2[[respvar]][ind.mat[i,j]])
				}
			}
		}
		
	names(df1) <- c("sample", facetvars, "theoretical")
	
	theoretical <- sample <- NA
	
	# do the plot
	p <- ggplot2::ggplot(data = df1, ggplot2::aes(x = theoretical, y = sample)) +
	  	ggplot2::geom_point() +
  		ggplot2::geom_smooth(method = "lm", se = FALSE) +
  		ggplot2::xlab("Theoretical Quantiles") +
 		ggplot2::ylab("Sample Quantiles") +
 		ggplot2::facet_grid(fstring)
 	p
	}