##' Find S3 Methods That Apply to an Object
##'
##' Adapted from Josh O'Brien's idea at
##' https://stackoverflow.com/questions/23840404/function-to-return-all-s3-methods-applicable-to-an-object
##'
##' @param object An R object.
##'
##' @return A vector of applicable methods.
##' 
##' @name findS3Meth
##' @rdname findS3Meth
##' @export
##' @keywords utilities
##'
##' @examples
##' data(ToothGrowth)
##'	tst1 <- findS3Meth(ToothGrowth); tst1
##' tst2 <- findS3Meth(ToothGrowth$supp); tst2


findS3Meth <- function(object) {
    x <- unlist(lapply(class(object),function(x) utils::methods(class=x)))   
    sort(x[!duplicated(sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x))])
}

