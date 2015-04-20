#' Create an html Page Showing All Examples in Rd Files
#' 
#' This function runs all the example code in a package's Rd files, using
#' \code{knitr} to create a \code{markdown} file.  This file is then converted
#' to html and opened in a browser.  This function was written by Sasha Epskamp
#' and published on his blog (sachaepskamp.com/blog/HTMLexamples).  I modified
#' it to accept more than one file for \code{exclude}.
#' 
#' 
#' @param pkg Path to the package directory.
#'
#' @param openChunk Character.  The default opening markdown chunk.
#'
#' @param includeDontshow Logical indicating whether to include the
#' \code{Dontshow} sections of the Rd file.
#'
#' @param includeDontrun Logical indicating whether to include the
#' \code{Dontrun} sections of the Rd file.
#'
#' @param exclude Character vector listing the Rd files to skip.  For instance,
#' \code{c("func1.Rd", "func2.Rd")}.
#'
#' @return Creates and displays a web page.
#'
#' @author Bryan A. Hanson, DePauw University. \email{hanson@@depauw.edu}
#'
#' @references sachaepskamp.com/blog/HTMLexamples
#' @keywords utilities
#' @examples
#' 
#' # Set the working dir to the pkg dir
#' # Note that this will leave some files in the dir
#' \dontrun{
#' examplePage(getwd(), exclude = c("jSeq"))
#' }
#' 
#' @export examplePage
examplePage <- function(pkg, openChunk = "```{r, message=FALSE, warning = FALSE, error = FALSE}", 
    includeDontshow = FALSE, includeDontrun = TRUE, exclude = NULL) {

# Taken from http://sachaepskamp.com/blog/HTMLexamples
# A small modification was made to the Exclude block

    # Inner function to find closing brackets:
    findClose <- function(x, openLoc, open = "\\{", close = "\\}") {
        # Find close:
        nest <- 1
        i <- openLoc + 1
        repeat {
            # If open bracket in line:
            if (grepl(open, x[i])) {
                nest <- nest + length(gregexpr(open, x[i])[[1]])
            }
            if (grepl(close, x[i])) {
                nest <- nest - length(gregexpr(close, x[i])[[1]])
            }
            if (nest == 0) 
                break
            i <- i + 1
        }
        return(i)
    }

    files <- list.files(paste0(pkg, "/man"), pattern = "\\.Rd$", ignore.case = TRUE, 
        full.names = TRUE)

    # Exclude:
    if (!is.null(exclude)) 
    	for (i in seq_along(exclude)) {
        files <- files[!grepl(exclude[i], files)]
		}
		
    # Preparation:
    n <- length(files)
    subs <- character(n)

    # For each rd file:
    for (i in seq_along(files)) {
    	cat("Parsing Rd file:", basename(files[i]), "\n")
        # Read file:
        txt <- readLines(files[i])

        # Only include if there is only one example section:
        if (sum(grepl("\\\\examples\\{", txt)) == 1) {
            # Extract examples section:
            start <- grep("\\\\examples\\{", txt)
            end <- findClose(txt, start)
            txt <- txt[(start + 1):(end - 1)]

            # Don't show fields:
            dontshows <- grep("\\\\dontshow\\{", txt)
            if (length(dontshows) > 0) {
                ends <- numeric(length(dontshows))
                for (k in seq_along(dontshows)) {
                  ends[k] <- findClose(txt, dontshows[k])
                }

                # Remove:
                if (includeDontshow) {
                  txt <- txt[-c(dontshows, ends)]
                } else txt <- txt[-do.call(c, mapply(dontshows, ends, FUN = ":", 
                  SIMPLIFY = FALSE))]
            }

            # Don't run fields:
            dontruns <- grep("\\\\dontrun\\{", txt)
            if (length(dontruns) > 0) {
                ends <- numeric(length(dontruns))
                for (k in seq_along(dontruns)) {
                  ends[k] <- findClose(txt, dontruns[k])
                }

                # Remove:
                if (includeDontrun) {
                  txt <- txt[-c(dontruns, ends)]
                } else txt <- txt[-do.call(c, mapply(dontruns, ends, FUN = ":", 
                  SIMPLIFY = FALSE))]
            }

            # Enter main title and first R chunk:
            txt <- c(paste("##", gsub("\\.rd$", "", basename(files[i]), ignore.case = TRUE)), 
                openChunk, txt, "```")

            # Crawl over lines. If a title is encountered, close chunk and replace
            # title with markdown:
            j <- 3
            repeat {
                # Small section (start with exactly two hashes, does not end with nonword:
                if (grepl("^\\s*##\\s*(\\w|\\s)+$", txt[j])) {
                  txt[j] <- gsub("^\\s*##\\s*", "#### ", txt[j])
                  txt <- c(txt[1:(j - 1)], "```", txt[j], openChunk, txt[(j + 
                    1):length(txt)])
                  j <- j + 2

                  # Else large section, starts with #, ends with nonchar, or starts with
                  # more than 2 #'s
                } else if (grepl("\\w", txt[j]) & (grepl("^\\s*###", txt[j]) | 
                  grepl("^\\s*#.*[#-]\\s*$", txt[j]))) {
                  txt[j] <- gsub("^\\W*(?=\\w)", "### ", txt[j], perl = TRUE)
                  txt[j] <- gsub("(?<=\\w)\\W*$", "", txt[j], perl = TRUE)

                  txt <- c(txt[1:(j - 1)], "```", txt[j], openChunk, txt[(j + 
                    1):length(txt)])
                  j <- j + 2
                } else if (grepl("^\\s*#\\W*$", txt[j])) {
                  # If start is comment and no words, remove:
                  txt <- txt[-j]
                  j <- j - 1
                }

                j <- j + 1
                if (j > length(txt)) 
                  break
            }

            emptySections <- which(txt[-length(txt)] == openChunk & txt[-1] == 
                "```")
            if (length(emptySections) > 0) 
                txt <- txt[-c(emptySections, emptySections + 1)]

            txt <- gsub("\\\\%", "%", txt)
            subs[i] <- paste(txt, collapse = "\n")
        }
    }

    message("Done parsing Rd files")

    subs <- subs[order(nchar(subs), decreasing = TRUE)]
    subs <- c(paste0("# ", basename(pkg), "\n\n```{r,echo=FALSE,message=FALSE}\nlibrary(\"", 
        basename(pkg), "\")\n```"), subs)

    # Write Rmd:
    RmdFile <- paste0(basename(pkg), ".Rmd")
    write(paste(subs, collapse = "\n\n"), RmdFile)
    message("Rmd file written, ready to knit")

    # Knit:
    mdFile <- gsub("Rmd", "md", RmdFile)
    knitr::knit(RmdFile, mdFile)
    message("Done knitting, ready for markdown")

    # Markdown:
    htmlFile <- gsub("Rmd", "html", RmdFile)
    markdown::markdownToHTML(mdFile, htmlFile)

    browseURL(htmlFile)

    return(htmlFile)
}
