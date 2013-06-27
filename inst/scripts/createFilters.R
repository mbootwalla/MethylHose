library(FDb.InfiniumMethylation.hg19)
# Retrieve mapping information for the Illumina 450k chip based on hg19
data(hm450k)
#hm450k <- get450k()

# Create the cg filter which retains the names of only CpG probes
filter.noncg <- names(hm450k[-which(values(hm450k)$probeType == "cg")])
filter.noncg <- filter.noncg[order(filter.noncg)]

# Create the sex filter which retains all the probes not on sex chromosomes
filter.sex <- names(hm450k[which(seqnames(hm450k) %in% c("chrX", "chrY"))])
filter.sex <- filter.sex[order(filter.sex)]

# Create the snp filter based on the SNP135common track from UCSC
library(FDb.UCSC.snp135common.hg19)
snp135common <- features(FDb.UCSC.snp135common.hg19)
filter.snp <- suppressWarnings(names(subsetByOverlaps(resize(hm450k, 10, fix='end'), snp135common)))
filter.snp <- filter.snp[order(filter.snp)]

# Create REPEAT filter
# Create a GRanges containing all REPEATS based on Repeat Masker and Tandem Repeat Finder
require(parallel)
require(GenomicFeatures)
require(BSgenome.Hsapiens.UCSC.hg19)
REPEATS <- suppressWarnings(do.call(c, mclapply(seqlevels(hm450k)[-which(seqlevels(hm450k) == "chrUn")], function(ch) {
  GRanges(ch, union(masks(Hsapiens[[ch]])$RM, masks(Hsapiens[[ch]])$TRF))
})))
filter.rpt <- names(subsetByOverlaps(resize(hm450k, 15, fix='end'), REPEATS, type="within"))
filter.rpt <- filter.rpt[order(filter.rpt)]

# Combine all filters to create the all filter
filter.all <- unique(c(filter.noncg, filter.sex, filter.snp, filter.rpt))
filter.all <- filter.all[order(filter.all)]

# Store all the filters into a named list
filters <- list("filter.noncg" = filter.noncg, "filter.sex" = filter.sex, "filter.snp" = filter.snp, "filter.rpt" = filter.rpt, "filter.all" = filter.all)

