binaryCluster <- function(x)
{
	if(!is(x, "SimpleList")){
		stop("Data should be a SimpleList object \n")
	}
	message("Performing clustering using Ward's method with a binary distance metric \n")
	dat <- x$FILTERED$Tumor.Filtered
	# Cluster the samples
	d <- dist(t(dat), method = "binary") 
	fit <- hclust(d, method="ward")
	# Cluster the probes
	d.probe <- dist(dat, method = "binary") 
	fit.probe <- hclust(d.probe, method="ward")
	# Extract raw betas for plotting 
	betaT <- x$RAW$Tumor.Raw
	betaN <- x$RAW$Normal.Raw
	# Order Tumor samples and probes based on their clustering order
	datT.clustered <- betaT[rownames(dat)[fit.probe$order], fit$order, drop=FALSE]
	datN.clustered <- betaN[rownames(datT.clustered), , drop=FALSE]

	retval <- SimpleList()
	retval$CLUSTER <- SimpleList("Tumor.Clustered" = datT.clustered, "Normal.Clustered" = datN.clustered)
	retval$DISTANCE <- SimpleList("Dist.Sample" = d, "Dist.Probe" = d.probe)
	retval$FIT <- SimpleList("Fit.Sample" = fit, "Fit.Probe" = fit.probe)

	return(retval)
}
		
