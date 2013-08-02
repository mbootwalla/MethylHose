TNplot <- function(x, disease, palette="jet")
{
	require("matlab")
	def.par <- par(no.readonly=TRUE)
	
	if(!is(x, "SimpleList")){
		stop("Data should be a SimpleList object \n")
	}
	if(missing(disease)){
		stop("Please provide the name of the Tumor type being visualized \n")
	}

	datT <- x$CLUSTER$Tumor.Clustered
	datN <- x$CLUSTER$Normal.Clustered
	fit.sample <- x$FIT$Fit.Sample

	# Restrict total no. of data points to 1 million for plotting purposes
	#n <- ceiling(1000000 / ncol(datT))
	#if(nrow(datT) > n){
#		datT <- datT[sample(rownames(datT), n), , drop=FALSE]
#		datN <- datN[rownames(datT), , drop=FALSE]
#	}

	if(match.arg(palette) == "jet"){
		colors <- jet.colors(100)
	} else {
		colors <- palette
	}

	par(mar = c(5, 2, 0, 3))
	layout(matrix(c(4,3,2,1), nrow=2, ncol=2, byrow=T), widths=c(1.5,4), heights=c(1.5,4))
	image(1:ncol(datT), 1:nrow(datT), t(datT), axes=F, col=colors, xlab="", ylab="")
	box()
	mtext(paste(ncol(datT), disease, "Tumor Samples", sep=" "), side=1, line=1)
	mtext(paste(nrow(datT), "Probes", sep=" "), side=2, line=1)
	par(mar = c(5,2,0,5))
	image(1:ncol(datN), 1:nrow(datN), t(datN), axes=F, col=colors, xlab="", ylab="")
	box()
	mtext(paste(ncol(datN), "Normals", sep=" "), side=1, line=1)
	par(mar = c(0, 2, 0, 3))
	plot(as.dendrogram(fit.sample), axes=F, xaxs="i", leaflab="none")
	par(mar=c(5,2,3,5), cex=0.75)
	colorstrip(colors, description=expression(paste(beta, " value 0 -> 1", sep="")))
	par(def.par)
}


colorstrip <- function(colors, description)
{
	count <- length(colors)
	m <- matrix(1:count, count, 1)
	image(m, col=colors, ylab="", axes=FALSE)
	box()
	mtext(description, 1, adj=0.5, line=0.5)
}
