dichotomize <- function(Mset, T, N, index.T, index.N, filter=c("none", "noncg", "sex", "snp", "rpt", "all", "custom"))
{
	if(!missing(Mset) && is(Mset, "MethylumiSet")){
		if(!missing(T) && !missing(N)) warning("Both MethyLumiSet and Tumor-Normal beta value matrices are provided. Using methyLumiSet for processing \n")
		if(missing(index.T) || missing(index.N)) stop("Tumor and Normal sample indices are missing. Cannot process MethyLumiSet \n")
		beta <- betas(Mset)		
	}

	if(missing(Mset) && (!missing(T) && (!missing(N)))){
		if(!is(T, "matrix") && !is(N, "matrix")) stop("Expect Tumor-Normal beta values to be matrices. Please provide valid matrices to proceed \n")
		beta <- cbind(T, N)
		if(missing(index.T) || missing(index.N)){
			index.T <- 1:ncol(T)
			index.N <- (ncol(T)+1):ncol(beta)
		}
	}

	if(!any(grepl("^cg", rownames(beta)))) stop("Probe names are not standard Illumina 450k probe names \n")

	filter <- match.arg(filter, several.ok=TRUE)
	if(!(all(filter %in% c("none", "noncg", "sex", "snp", "rpt", "all", "custom")))) stop("Please provide a valid filter. See '?applyFilters' for valid filters \n")
	
	beta.filtered <- applyFilters(beta, filter)
	beta.filtered <- na.omit(beta.filtered)
	gc()
	betaT.raw <- beta.filtered[, index.T, drop=FALSE]
	betaN.raw <- beta.filtered[, index.N, drop=FALSE]
	medianN <- apply(betaN.raw, 1, median, na.rm = T)
	probesN <- names(subset(medianN, medianN < 0.2))
	betaT.dichotomized <- betaT.raw[probesN, , drop=FALSE]
	betaT.dichotomized <- betaT.dichotomized > 0.3
	storage.mode(betaT.dichotomized) <- "numeric"
	betaN.dichotomized <- betaN.raw > 0.3
	storage.mode(betaN.dichotomized) <- "numeric"

	retval <- SimpleList()
	retval$TUMOR <- SimpleList("Beta.Raw" = betaT.raw, "Beta.Dichotomized" = betaT.dichotomized)
	retval$NORMAL <- SimpleList("Beta.Raw" = betaN.raw, "Beta.Dichotomized" = betaN.dichotomized)

	gc()

	return(retval)
}
		
