\name{dichotomize}
\alias{dichotomize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dichotomize Tumor data based on Methylation values of Normal samples to account for contamination
}
\description{
This function filters and dichotomizes Methylation values of Tumor samples based on the methylation values of Normal samples. This method accounts for the contamination of Tumor tissue with surrounding Normal tissue which routinely confounds methylation analysis of cancer samples
}
\usage{
dichotomize(Mset, T, N, index.T, index.N, filter = c("none", "noncg", "sex", "snp", "rpt", "all", "custom"), custom.filter)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Mset}{
%%     ~~Describe \code{Mset} here~~
}
  \item{T}{
%%     ~~Describe \code{T} here~~
}
  \item{N}{
%%     ~~Describe \code{N} here~~
}
  \item{index.T}{
%%     ~~Describe \code{index.T} here~~
}
  \item{index.N}{
%%     ~~Describe \code{index.N} here~~
}
  \item{filter}{
%%     ~~Describe \code{filter} here~~
}
  \item{custom.filter}{
%%     ~~Describe \code{custom.filter} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (Mset, T, N, index.T, index.N, filter = c("none", "noncg", 
    "sex", "snp", "rpt", "all", "custom"), custom.filter) 
{
    if (!missing(Mset) && is(Mset, "MethylumiSet")) {
        if (!missing(T) && !missing(N)) 
            warning("Both MethyLumiSet and Tumor-Normal beta value matrices are provided. Using methyLumiSet for processing \n")
        if (missing(index.T) || missing(index.N)) 
            stop("Tumor and Normal sample indices are missing. Cannot process MethyLumiSet \n")
        beta <- betas(Mset)
    }
    if (missing(Mset) && (!missing(T) && (!missing(N)))) {
        if (!is(T, "matrix") && !is(N, "matrix")) 
            stop("Expect Tumor-Normal beta values to be matrices. Please provide valid matrices to proceed \n")
        beta <- cbind(T, N)
        if (missing(index.T) || missing(index.N)) {
            index.T <- 1:ncol(T)
            index.N <- (ncol(T) + 1):ncol(beta)
        }
    }
    if (!any(grepl("^cg", rownames(beta)))) 
        stop("Probe names are not standard Illumina 450k probe names \n")
    if (missing(filter)) {
        filter <- "all"
    }
    else {
        filter <- match.arg(filter, several.ok = TRUE)
    }
    if (!(all(filter \%in\% c("none", "noncg", "sex", "snp", "rpt", 
        "all", "custom")))) 
        stop("Please provide a valid filter. See '?applyFilters' for valid filters \n")
    beta.filtered <- applyFilters(beta, filter, custom.filter = custom.filter)
    beta.filtered <- na.omit(beta.filtered)
    gc()
    betaT.raw <- beta.filtered[, index.T, drop = FALSE]
    betaN.raw <- beta.filtered[, index.N, drop = FALSE]
    medianN <- apply(betaN.raw, 1, median, na.rm = T)
    probesN <- names(subset(medianN, medianN < 0.2))
    betaT.dichotomized <- betaT.raw[probesN, , drop = FALSE]
    betaT.dichotomized <- betaT.dichotomized > 0.3
    storage.mode(betaT.dichotomized) <- "numeric"
    betaN.dichotomized <- betaN.raw > 0.3
    storage.mode(betaN.dichotomized) <- "numeric"
    retval <- SimpleList()
    retval$RAW <- SimpleList(Tumor.Raw = betaT.raw, Normal.Raw = betaN.raw)
    retval$DICHOTOMIZED <- SimpleList(Tumor.Dichotomized = betaT.dichotomized, 
        Normal.Dichotomized = betaN.dichotomized)
    gc()
    return(retval)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
