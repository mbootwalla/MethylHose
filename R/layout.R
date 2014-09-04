lo <- function (rown, coln, nrow, ncol, cellheight = NA, cellwidth = NA
, treeheight_col = 50, treeheight_row = 0, legend, main = NULL, sub = NULL, info = NULL
, fontsize, fontsize_row, fontsize_col = cexCol * fontsize, ...){

	#annotation_colors <- annTracks$colors
	#row_annotation <- annTracks$annRow
	#annotation <- annTracks$annCol
	
	coln_height <- unit(10, "bigpts")
	if(!is.null(coln)){
		longest_coln = which.max(nchar(coln))
		gp = gpar(fontsize = fontsize_col, ...)
		coln_height <- coln_height +  unit(1.1, "grobheight", textGrob(coln[longest_coln], rot = 90, gp = gp))
	}

	rown_width <- rown_width_min <- unit(10, "bigpts")
	if(!is.null(rown)){
		longest_rown = which.max(nchar(rown))
		gp = gpar(fontsize = fontsize_row, ...)
		rown_width <- rown_width_min + unit(1.2, "grobwidth", textGrob(rown[longest_rown], gp = gp))
	}
	
	gp = list(fontsize = fontsize, ...)
	# Legend position
	if( !is_NA(legend) ){
		longest_break = which.max(nchar(as.character(legend)))
		longest_break = unit(1.1, "grobwidth", textGrob(as.character(legend)[longest_break], gp = do.call(gpar, gp)))
		# minimum fixed width: plan for 2 decimals and a sign 
		min_lw = unit(1.1, "grobwidth", textGrob("-00.00", gp = do.call(gpar, gp)))
		longest_break = max(longest_break, min_lw)
		title_length = unit(1.1, "grobwidth", textGrob("Scale", gp = gpar(fontface = "bold", ...)))
		legend_width = unit(12, "bigpts") + longest_break * 1.2
		legend_width = max(title_length, legend_width)
	}
	else{
		legend_width = unit(0, "bigpts")
	}
	
	#.annLegend.dim <- function(annotation, fontsize){
		# Width of the corresponding legend
	#	longest_ann <- unlist(lapply(annotation, names))
	#	longest_ann <- longest_ann[which.max(nchar(longest_ann))]
	#	annot_legend_width = unit(1, "grobwidth", textGrob(longest_ann, gp = gpar(fontsize=fontsize, ...))) + unit(10, "bigpts")
		
		# width of the legend title
	#	annot_legend_title <- names(annotation)[which.max(nchar(names(annotation)))]
	#	annot_legend_title_width = unit(1, "grobwidth", textGrob(annot_legend_title, gp = gpar(fontface = "bold", fontsize=fontsize, ...)))
	#	
		# total width 
	#	max(annot_legend_width, annot_legend_title_width) + unit(5, "bigpts")
	#}
	
	# Column annotations
	#if( !is_NA(annotation) ){
		# Column annotation height		
	#	annot_height = unit(ncol(annotation) * (8 + 2) + 2, "bigpts")
	#}
	#else{
		annot_height = unit(0, "bigpts")
	#}
	
	# add a viewport for the row annotations
	#if ( !is_NA(row_annotation) ) {
		# Row annotation width		
	#	row_annot_width = unit(ncol(row_annotation) * (8 + 2) + 2, "bigpts")
	#}
	#else {
		row_annot_width = unit(0, "bigpts")
	#}
	
	# Width of the annotation legend
	annot_legend_width <- unit(0, "bigpts")
	#annot_legend_width <- 
	#	if( annotation_legend && !is_NA(annotation_colors) ){ 
	#		.annLegend.dim(annotation_colors, fontsize)
	#	}else unit(0, "bigpts")

	# Tree height
	treeheight_col = unit(treeheight_col, "bigpts") + unit(5, "bigpts")
	treeheight_row = unit(treeheight_row, "bigpts") + unit(5, "bigpts") 
	
	# main title
	main_height <- if(!is.null(main)) unit(1, "grobheight", main) + unit(20, "bigpts") else unit(0, "bigpts")
	# sub title
	sub_height <- if(!is.null(sub)) unit(1, "grobheight", sub) + unit(10, "bigpts")	else unit(0, "bigpts")
	# info panel
	if( !is.null(info) ){
		info_height <- unit(1, "grobheight", info) + unit(20, "bigpts")
		info_width <- unit(1, "grobwidth", info) + unit(10, "bigpts")
	}else{
		info_height <- unit(0, "bigpts")
		info_width <- unit(0, "bigpts")		
	}
	
	# Set cell sizes
	if(is.na(cellwidth)){
		matwidth = unit(1, "npc") - rown_width - legend_width - row_annot_width  - treeheight_row - annot_legend_width
	}
	else{
		matwidth = unit(cellwidth * ncol, "bigpts")
	}

	if(is.na(cellheight)){
		matheight = unit(1, "npc") - treeheight_col - annot_height - main_height - coln_height - sub_height - info_height
	
		# recompute the cell width depending on the automatic fontsize
		if( is.na(cellwidth) && !is.null(rown) ){
			cellheight <- convertHeight(unit(1, "grobheight", rectGrob(0,0, matwidth, matheight)), "bigpts", valueOnly = T) / nrow
			fontsize_row <- convertUnit(min(unit(fontsize_row, 'points'), unit(0.6*cellheight, 'bigpts')), 'points')
			
			rown_width <- rown_width_min + unit(1.2, "grobwidth", textGrob(rown[longest_rown], gp = gpar(fontsize=fontsize_row, ...)))
			matwidth <- unit(1, "npc") - rown_width - legend_width - row_annot_width  - treeheight_row - annot_legend_width
		}
	}
	else{
		matheight = unit(cellheight * nrow, "bigpts")
	}	
		
	# HACK: 
	# - use 6 instead of 5 column for the row_annotation
	# - take into account the associated legend's width
	# Produce layout()
	unique.name <- vplayout(NULL)
	lo <- grid.layout(nrow = 7, ncol = 6
			, widths = unit.c(treeheight_row, row_annot_width, matwidth, rown_width, legend_width, annot_legend_width)
			, heights = unit.c(main_height, treeheight_col,  annot_height, matheight, coln_height, sub_height, info_height))
	hvp <- viewport( name=paste('aheatmap', unique.name, sep='-'), layout = lo)
	pushViewport(hvp)
	
	#grid.show.layout(lo); stop('sas')
	# Get cell dimensions
	vplayout('mat')
	cellwidth = convertWidth(unit(1, "npc"), "bigpts", valueOnly = T) / ncol
	cellheight = convertHeight(unit(1, "npc"), "bigpts", valueOnly = T) / nrow
	upViewport()
		
	height <- as.numeric(convertHeight(sum(lo$height), "inches"))
	width <- as.numeric(convertWidth(sum(lo$width), "inches"))
	# Return minimal cell dimension in bigpts to decide if borders are drawn
	mindim = min(cellwidth, cellheight) 
	return( list(width=width, height=height, vp=hvp, mindim=mindim, cellwidth=cellwidth, cellheight=cellheight) )
}

vplayout <- function ()
{
	graphic.name <- NULL
	
	function(x, y, verbose = getOption('verbose') ){
		# initialize the graph name
		if( is.null(x) ){
			graphic.name <<- grid:::vpAutoName()
			return(graphic.name)
		}
		name <- NULL
		if( !is.numeric(x) ){
					
			name <- paste(graphic.name, x, sep='-')
			
			if( !missing(y) && is(y, 'viewport') ){
				y$name <- name
				return(pushViewport(y))			
			}
			if( !is.null(tryViewport(name, verbose=verbose)) )
				return()
			
			switch(x
				, main={x<-1; y<-3;}
				, ctree={x<-2; y<-3;}
				, cann={x<-3; y<-3;}
				, rtree={x<-4; y<-1;}
				, rann={x<-4; y<-2;}
				, mat={x<-4; y<-3;}
				, rnam={x<-4; y<-4;}
				, leg={x<-4; y<-5;}
				, aleg={x<-4; y<-6;}
				, cnam={x<-5; y<-3;}
				, sub={x<-6; y<-3;}
				, info={x<-7; y<-3;}
				, stop("aheatmap - invalid viewport name")
			)
		}
		if( verbose ) message("vp - create ", name)
		pushViewport(viewport(layout.pos.row = x, layout.pos.col = y, name=name))
	}	
}
