applyFilters <- function(x, filter="all")
{
	if(missing(x) || !is(x, "matrix")) stop("Please provide a valid matrix as input \n")
	if(!any(grepl("^cg", rownames(x)))) stop("Probe names for the beta values matrix are not standard Illumina 450k probe names \n")
	
	if(!(all(filter %in% c("none", "noncg", "sex", "snp", "rpt", "all", "custom")))) stop("Please provide a valid filter. See '?applyFilters' for valid filters \n")
	
	if("none" %in% filter){
		message("Not performing any filtering as Filter : none was specified \n\n") 
		return(x)
	}
	
	if("all" %in% filter){
		data(filters)
		if(length(filter) > 1) warning("Multiple filter types specified along with type 'all'. Overriding the other filters and applying only the 'all' filter \n")
		message("Applying Filter : all \n\n")
		filter.all <- filters$filter.all
		x <- x[-which(rownames(x) %in% filter.all), , drop=FALSE]
		return(x)
	}

	if("custom" %in% filter){
		if(!is(filter, "character")) stop("Custom filter should be a chracter vector containing probe names that are to be filtered out \n")
		if(!any(grepl("^cg", filter))) stop("Custom filter contains non-standard Illumina 450k probe names \n")
		message("Applying provided custom filter \n\n")
		x <- x[-which(rownames(x) %in% filter), , drop=FALSE]
		return(x)
	}

	if(!("none" %in% filter) && !("all" %in% filter) && !("custom" %in% filter)){
		data(filters)
		f <- paste("filter", filter, sep=".")
		message(paste("Applying filters : ", paste(filter, collapse = ", "), "\n\n", sep=""))
		filter.apply <- unique(unlist(lapply(f, function(x){filters[f]})))
		x <- x[-which(rownames(x) %in% filter.apply), , drop=FALSE]
		return(x)
	}
}
	

	
	
