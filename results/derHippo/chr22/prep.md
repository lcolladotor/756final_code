Pre-processing report
=====================

# Setup

## Libraries

<div class="chunk" id="setup"><div class="rcode"><div class="source"><pre class="knitr r">## Load libraries

## Available from http://cran.r-project.org/web/packages/getopt/index.html
# install.packages("getopt")
library("getopt")

## Available from http://cran.at.r-project.org/web/packages/knitrBootstrap/index.html
# install.packages("knitrBootstrap")
library("knitrBootstrap")

## Available from http://cran.at.r-project.org/web/packages/reshape2/index.html
# install.packages("reshape2")
library("reshape2")

## Available from http://www.bioconductor.org/packages/release/bioc/html/GenomicRanges.html
# source("http://bioconductor.org/biocLite.R")
# biocLite("GenomicRanges")
library("GenomicRanges")

## Available from http://cran.at.r-project.org/web/packages/geepack/index.html
# install.packages("geepack")
library("geepack")

## Available from https://github.com/lcolladotor/derfinder
## Read README.md for installation instructions
# install.packages(devtools)
# library(devtools)
# install_github("derfinder", "lcolladotor")
library("derfinder")

## Available from http://www.bioconductor.org/packages/release/bioc/html/TxDb.Hsapiens.UCSC.hg19.knownGene.html
# source("http://bioconductor.org/biocLite.R")
# biocLite("TxDb.Hsapiens.UCSC.hg19.knownGene")
suppressMessages(library("TxDb.Hsapiens.UCSC.hg19.knownGene"))
</pre></div>
</div></div>


## Directories

<div class="chunk" id="directories"><div class="rcode"><div class="source"><pre class="knitr r">## Chr
chr <- paste0("chr", opt$chrnum)
chrnum <- as.numeric(opt$chrnum)

## Create dir to save files
if(opt$verbose) message("Creating directories")
</pre></div>
<div class="message"><pre class="knitr r">## Creating directories
</pre></div>
<div class="source"><pre class="knitr r">wdir <- file.path(opt$dirResult, opt$project, chr) # working dir
rawdir <- file.path(wdir, "raw")
dir.create(wdir, recursive=TRUE)
</pre></div>
<div class="warning"><pre class="knitr r">## Warning: '/home/bst/student/lcollado/756final_code/results/derHippo/chr22'
## already exists
</pre></div>
<div class="source"><pre class="knitr r">dir.create(rawdir, recursive=TRUE)
</pre></div>
<div class="warning"><pre class="knitr r">## Warning:
## '/home/bst/student/lcollado/756final_code/results/derHippo/chr22/raw'
## already exists
</pre></div>
<div class="source"><pre class="knitr r">
## Shortcuts for previous directories
pdir <- file.path("/dcs01/lieber/ajaffe/Brain/derRuns", opt$project) ## original project dir
rdir <- file.path(pdir, "derAnalysis", opt$run) ## original project-run dir
cdir <- file.path(rdir, chr) # original project-run-chr dir

## Dirs:
dirs <- c(wdir=wdir, rawdir=rawdir, pdir=pdir, rdir=rdir, cdir=cdir)
save(dirs, file=file.path(wdir, "dirs.Rdata"))
dirs
</pre></div>
<div class="output"><pre class="knitr r">##                                                                         wdir 
##            "/home/bst/student/lcollado/756final_code/results/derHippo/chr22" 
##                                                                       rawdir 
##        "/home/bst/student/lcollado/756final_code/results/derHippo/chr22/raw" 
##                                                                         pdir 
##                                "/dcs01/lieber/ajaffe/Brain/derRuns/derHippo" 
##                                                                         rdir 
##       "/dcs01/lieber/ajaffe/Brain/derRuns/derHippo/derAnalysis/run1-v0.0.42" 
##                                                                         cdir 
## "/dcs01/lieber/ajaffe/Brain/derRuns/derHippo/derAnalysis/run1-v0.0.42/chr22"
</pre></div>
</div></div>


# Pre-processing

## Copying original files

<div class="chunk" id="copy"><div class="rcode"><div class="source"><pre class="knitr r">## Copy raw files
if(opt$verbose) message("Copying raw files")
</pre></div>
<div class="message"><pre class="knitr r">## Copying raw files
</pre></div>
<div class="source"><pre class="knitr r">	
## Project-specific files
file.copy(file.path(rdir, "sampleDepths.Rdata"), rawdir)
</pre></div>
<div class="output"><pre class="knitr r">## [1] TRUE
</pre></div>
<div class="source"><pre class="knitr r">if(file.exists(file.path(rdir, "colsubset.Rdata"))) {
	file.copy(file.path(rdir, "colsubset.Rdata"), rawdir)
}
</pre></div>
<div class="output"><pre class="knitr r">## [1] TRUE
</pre></div>
<div class="source"><pre class="knitr r">
## Chr-specific files
file.copy(file.path(cdir, "annotation.Rdata"), rawdir)
</pre></div>
<div class="output"><pre class="knitr r">## [1] TRUE
</pre></div>
<div class="source"><pre class="knitr r">file.copy(file.path(cdir, "regions.Rdata"), rawdir)
</pre></div>
<div class="output"><pre class="knitr r">## [1] TRUE
</pre></div>
<div class="source"><pre class="knitr r">file.copy(file.path(cdir, "optionsStats.Rdata"), rawdir)
</pre></div>
<div class="output"><pre class="knitr r">## [1] TRUE
</pre></div>
</div></div>


## Identify pairs

<div class="chunk" id="pairs"><div class="rcode"><div class="source"><pre class="knitr r">## Identify pairs
if(opt$verbose) message("Identifying pairs")
</pre></div>
<div class="message"><pre class="knitr r">## Identifying pairs
</pre></div>
<div class="source"><pre class="knitr r">
## Load original regions
load(file.path(rawdir, "regions.Rdata"))
## Keep only the region info
regions <- regions$regions
## Order by cluster (determine by optionsStats$maxRegionGap)
regions <- regions[order(regions$cluster), ]

## Assign chr length info
data(hg19Ideogram, package = "biovizBase", envir = environment())
seqlengths(regions) <- seqlengths(hg19Ideogram)[names(seqlengths(regions))]

## Save regions
save(regions, file=file.path(wdir, "regions.Rdata"))

## Identify clusters that have 2 or more regions: aka, have at least a pair
p.idx <- which(runLength(regions$cluster) > 1)
## Number of pairs
p.n <- sum(runLength(regions$cluster)[p.idx] - 1)
if(p.n == 0) stop("No pairs found")

regsub <- regions[ regions$cluster %in% p.idx]
save(regsub, file=file.path(wdir, "regsub.Rdata"))

na <- rep(NA, p.n)
pairs <- data.frame(start1=na, end1=na, startM=na, endM=na, start2=na, end2=na, cluster=na)
i <- 0
for(j in unique(regsub$cluster)) {
	if(opt$verbose) message(paste("Processing region cluster", j))
	## Subset
	cluster <- regsub[regsub$cluster == j]
	## Order by chr position
	cluster <- cluster[order(start(cluster))]
	for(k in seq_len(length(cluster) - 1)) {
		i <- i + 1
		current <- cluster[k:(k+1)]
		pairs[i, ] <- c(start1=start(current[1]), end1=end(current[1]), startM=end(current[1])+1, endM=start(current[2])-1, start2=start(current[2]), end2=end(current[2]), cluster=j)
	}
}
</pre></div>
<div class="message"><pre class="knitr r">## Processing region cluster 2
## Processing region cluster 3
## Processing region cluster 10
## Processing region cluster 13
## Processing region cluster 14
## Processing region cluster 15
## Processing region cluster 17
## Processing region cluster 18
## Processing region cluster 19
## Processing region cluster 20
## Processing region cluster 22
## Processing region cluster 25
## Processing region cluster 26
## Processing region cluster 27
## Processing region cluster 28
## Processing region cluster 34
## Processing region cluster 35
## Processing region cluster 38
## Processing region cluster 39
## Processing region cluster 41
## Processing region cluster 43
## Processing region cluster 49
## Processing region cluster 51
## Processing region cluster 52
## Processing region cluster 53
## Processing region cluster 54
## Processing region cluster 55
## Processing region cluster 56
## Processing region cluster 57
## Processing region cluster 60
## Processing region cluster 61
## Processing region cluster 62
## Processing region cluster 63
## Processing region cluster 64
## Processing region cluster 65
## Processing region cluster 66
## Processing region cluster 67
## Processing region cluster 68
## Processing region cluster 69
## Processing region cluster 74
## Processing region cluster 77
## Processing region cluster 78
## Processing region cluster 79
## Processing region cluster 83
## Processing region cluster 85
## Processing region cluster 86
## Processing region cluster 87
## Processing region cluster 88
## Processing region cluster 90
## Processing region cluster 91
## Processing region cluster 95
## Processing region cluster 96
## Processing region cluster 98
## Processing region cluster 99
## Processing region cluster 101
## Processing region cluster 102
## Processing region cluster 103
## Processing region cluster 104
## Processing region cluster 105
## Processing region cluster 107
## Processing region cluster 108
## Processing region cluster 111
## Processing region cluster 114
## Processing region cluster 115
## Processing region cluster 116
## Processing region cluster 118
## Processing region cluster 124
## Processing region cluster 125
## Processing region cluster 126
## Processing region cluster 127
## Processing region cluster 128
## Processing region cluster 129
## Processing region cluster 130
## Processing region cluster 131
## Processing region cluster 132
## Processing region cluster 133
## Processing region cluster 134
## Processing region cluster 135
## Processing region cluster 136
## Processing region cluster 138
## Processing region cluster 140
## Processing region cluster 141
## Processing region cluster 142
## Processing region cluster 144
## Processing region cluster 146
## Processing region cluster 148
## Processing region cluster 149
## Processing region cluster 151
## Processing region cluster 152
## Processing region cluster 154
## Processing region cluster 155
## Processing region cluster 156
## Processing region cluster 158
## Processing region cluster 160
## Processing region cluster 161
## Processing region cluster 163
## Processing region cluster 165
## Processing region cluster 166
## Processing region cluster 167
## Processing region cluster 169
## Processing region cluster 170
## Processing region cluster 173
## Processing region cluster 174
## Processing region cluster 177
## Processing region cluster 178
## Processing region cluster 180
## Processing region cluster 181
## Processing region cluster 182
## Processing region cluster 184
## Processing region cluster 185
## Processing region cluster 187
## Processing region cluster 188
## Processing region cluster 189
## Processing region cluster 194
## Processing region cluster 195
## Processing region cluster 197
## Processing region cluster 199
## Processing region cluster 201
## Processing region cluster 202
## Processing region cluster 203
## Processing region cluster 204
</pre></div>
<div class="source"><pre class="knitr r">pairs$width1 <- pairs$end1 - pairs$start1 + 1
pairs$widthM <- pairs$endM - pairs$startM + 1
pairs$width2 <- pairs$end2 - pairs$start2 + 1
pairs$widthNoM <- pairs$width1 + pairs$width2

## Save pairs info
save(pairs, file=file.path(wdir, "pairs.Rdata"))
</pre></div>
</div></div>


### Basic pairs exploration

<div class="chunk" id="pairsExplore"><div class="rcode"><div class="source"><pre class="knitr r">dim(pairs)
</pre></div>
<div class="output"><pre class="knitr r">## [1] 573  11
</pre></div>
<div class="source"><pre class="knitr r">summary(pairs)
</pre></div>
<div class="output"><pre class="knitr r">##      start1              end1              startM        
##  Min.   :18070704   Min.   :18070705   Min.   :18070706  
##  1st Qu.:29951184   1st Qu.:29951184   1st Qu.:29951185  
##  Median :39709674   Median :39709734   Median :39709735  
##  Mean   :36397927   Mean   :36397932   Mean   :36397933  
##  3rd Qu.:41918842   3rd Qu.:41918849   3rd Qu.:41918850  
##  Max.   :51170990   Max.   :51170992   Max.   :51170993  
##       endM              start2              end2             cluster   
##  Min.   :18072427   Min.   :18072428   Min.   :18072431   Min.   :  2  
##  1st Qu.:29951186   1st Qu.:29951187   1st Qu.:29951188   1st Qu.: 64  
##  Median :39710111   Median :39710112   Median :39710152   Median :124  
##  Mean   :36398166   Mean   :36398167   Mean   :36398172   Mean   :110  
##  3rd Qu.:41920903   3rd Qu.:41920904   3rd Qu.:41920904   3rd Qu.:151  
##  Max.   :51171396   Max.   :51171397   Max.   :51171401   Max.   :204  
##      width1           widthM         width2          widthNoM    
##  Min.   :  1.00   Min.   :   1   Min.   :  1.00   Min.   :  2.0  
##  1st Qu.:  1.00   1st Qu.:   1   1st Qu.:  1.00   1st Qu.:  4.0  
##  Median :  2.00   Median :   7   Median :  2.00   Median :  7.0  
##  Mean   :  6.26   Mean   : 234   Mean   :  6.21   Mean   : 12.5  
##  3rd Qu.:  6.00   3rd Qu.: 152   3rd Qu.:  6.00   3rd Qu.: 13.0  
##  Max.   :108.00   Max.   :2934   Max.   :108.00   Max.   :167.0
</pre></div>
<div class="source"><pre class="knitr r">with(pairs, pairs(~width1 + widthM + width2))
</pre></div>
<div class="rimage default"><img src="figure/pairsExplore1.png" title="plot of chunk pairsExplore" alt="plot of chunk pairsExplore" class="plot" /></div>
<div class="source"><pre class="knitr r">with(pairs, pairs(~widthM + widthNoM))
</pre></div>
<div class="rimage default"><img src="figure/pairsExplore2.png" title="plot of chunk pairsExplore" alt="plot of chunk pairsExplore" class="plot" /></div>
<div class="source"><pre class="knitr r">
## How many pairs seem 'long' enough? In percent
nrow(subset(pairs, width1 > 10 & width2 > 10)) / nrow(pairs) * 100
</pre></div>
<div class="output"><pre class="knitr r">## [1] 2.792
</pre></div>
<div class="source"><pre class="knitr r">nrow(subset(pairs, width1 > 10 & widthM > 10 & width2 > 10)) / nrow(pairs) * 100
</pre></div>
<div class="output"><pre class="knitr r">## [1] 2.269
</pre></div>
<div class="source"><pre class="knitr r">nrow(subset(pairs, width1 > 20 & width2 > 20)) / nrow(pairs) * 100
</pre></div>
<div class="output"><pre class="knitr r">## [1] 1.222
</pre></div>
<div class="source"><pre class="knitr r">nrow(subset(pairs, width1 > 20 & widthM > 20 & width2 > 20)) / nrow(pairs) * 100
</pre></div>
<div class="output"><pre class="knitr r">## [1] 0.6981
</pre></div>
<div class="source"><pre class="knitr r">nrow(subset(pairs, width1 > 50 & width2 > 50)) / nrow(pairs) * 100
</pre></div>
<div class="output"><pre class="knitr r">## [1] 0.349
</pre></div>
<div class="source"><pre class="knitr r">nrow(subset(pairs, width1 > 50 & widthM > 50 & width2 > 50)) / nrow(pairs) * 100
</pre></div>
<div class="output"><pre class="knitr r">## [1] 0.1745
</pre></div>
<div class="source"><pre class="knitr r">nrow(subset(pairs, width1 > 100 & width2 > 100)) / nrow(pairs) * 100
</pre></div>
<div class="output"><pre class="knitr r">## [1] 0
</pre></div>
<div class="source"><pre class="knitr r">nrow(subset(pairs, width1 > 100 & widthM > 100 & width2 > 100)) / nrow(pairs) * 100
</pre></div>
<div class="output"><pre class="knitr r">## [1] 0
</pre></div>
</div></div>



## Extract coverage data

<div class="chunk" id="getCoverage"><div class="rcode"><div class="source"><pre class="knitr r">## Load coverage data
if(opt$verbose) message("Loading coverage data")
</pre></div>
<div class="message"><pre class="knitr r">## Loading coverage data
</pre></div>
<div class="source"><pre class="knitr r">load(file.path(pdir, "derCoverageInfo", "fullCov.Rdata"))
cov <- fullCov[[chrnum]]
rm(fullCov)

## Apply colsubset if present
if(opt$verbose) message("Extracting coverage per pair region")
</pre></div>
<div class="message"><pre class="knitr r">## Extracting coverage per pair region
</pre></div>
<div class="source"><pre class="knitr r">if(file.exists(file.path(rdir, "colsubset.Rdata"))) {
	load(file.path(rdir, "colsubset.Rdata"))
	cov <- cov[colsubset]
} 
save(cov, file=file.path(wdir, "cov.Rdata"))

## Load transform info and sample depth adjustments
load(file.path(rawdir, "optionsStats.Rdata"))
load(file.path(rawdir, "sampleDepths.Rdata"))
names(sampleDepths) <- colnames(cov)

groupInfo <- optionsStats$groupInfo
names(groupInfo) <- colnames(cov)

## Get pair region coverage
# testing
# pairs <- pairs[1:6, ]
covdata <- apply(pairs, 1, function(x) {
	y <- log2(as.data.frame(cov[x["start1"]:x["end2"], ]) + optionsStats$scalefac)
	y$base <- seq_len(nrow(y))
	y$region=factor(rep(c("region1", "regionM", "region2"), x[c("width1", "widthM", "width2")]), levels=c("region1", "regionM", "region2"))
	new <- melt(y, id.vars=c("base", "region"), variable.name="sample", value.name="coverage")
	new$sampleDepth <- sampleDepths[new$sample]
	new$group <- groupInfo[new$sample]
	return(new)
})
save(covdata, file=file.path(wdir, "covdata.Rdata"))
</pre></div>
</div></div>


## Map to exons

<div class="chunk" id="exons"><div class="rcode"><div class="source"><pre class="knitr r">## Load genomic state object
load('/dcs01/lieber/ajaffe/Brain/derRuns/derfinderExample/derGenomicState/GenomicState.Hsapiens.UCSC.hg19.knownGene.Rdata')

## Identify exons from the chr
exons <- GenomicState.Hsapiens.UCSC.hg19.knownGene$fullGenome[ GenomicState.Hsapiens.UCSC.hg19.knownGene$fullGenome$theRegion == "exon" & seqnames(GenomicState.Hsapiens.UCSC.hg19.knownGene$fullGenome) == chr]
save(exons, file=file.path(wdir, "exons.Rdata"))

## Find overlaps to exons:
ov <- findOverlaps(regsub, exons)
ov.mat <- as.matrix(ov)
save(ov, ov.mat, file=file.path(wdir, "ov.Rdata"))
</pre></div>
</div></div>


# Example

<div class="chunk" id="plotCluster"><div class="rcode"><div class="source"><pre class="knitr r">## Explore cluster
# for derHippo chr11:
# which.max(pairs$widthNoM) is 574

## Identify top 10 clusters by area
df <- data.frame(area=regsub$area, clusterChr=paste0(as.integer(regsub$cluster), chr=as.character(seqnames(regsub))))
regionClustAreas <- tapply(df$area, df$clusterChr, sum)
bestArea <- sapply(names(head(sort(regionClustAreas, decreasing=TRUE), 10)), function(y) { which(df$clusterChr == y)[[1]]})
bestArea
</pre></div>
<div class="output"><pre class="knitr r">## 124chr22 166chr22 133chr22  78chr22   2chr22 151chr22   3chr22 163chr22 
##      334      580      422      212        1      504       15      567 
##  95chr22  35chr22 
##      246       71
</pre></div>
<div class="source"><pre class="knitr r">
## Use cluster #1 by area
bestCluster <- as.integer(regsub$cluster[bestArea[1]])
reg <- regsub[regsub$cluster == bestCluster]
reg <- reg[ order(start(reg))]
bestCluster.region <- which.max(width(reg)[-length(reg)])
i <- which(pairs$start1 == start(reg[bestCluster.region]))

p <- plotCluster(idx=bestCluster.region, regions=reg, annotation=reg, coverageInfo=cov, groupInfo=groupInfo, titleUse="qval", txdb=TxDb.Hsapiens.UCSC.hg19.knownGene, p.ideogram=NULL, maxExtend=300L, colsubset=NULL, forceLarge=FALSE)
print(p)
</pre></div>
<div class="rimage default"><img src="figure/plotCluster.png" title="plot of chunk plotCluster" alt="plot of chunk plotCluster" class="plot" /></div>
<div class="source"><pre class="knitr r">save(p, file=file.path(wdir, "figure", "p.Rdata") )
</pre></div>
</div></div>



<div class="chunk" id="gee"><div class="rcode"><div class="source"><pre class="knitr r">## Explore data
head(covdata[[i]])
</pre></div>
<div class="output"><pre class="knitr r">##   base  region sample coverage sampleDepth group
## 1    1 region1    CO1    5.358       28.25    CO
## 2    2 region1    CO1    5.459       28.25    CO
## 3    3 region1    CO1    5.615       28.25    CO
## 4    4 region1    CO1    5.615       28.25    CO
## 5    5 region1    CO1    5.672       28.25    CO
## 6    6 region1    CO1    5.728       28.25    CO
</pre></div>
<div class="source"><pre class="knitr r">summary(covdata[[i]])
</pre></div>
<div class="output"><pre class="knitr r">##       base         region          sample         coverage   
##  Min.   :  1   region1: 2700   CO1    :  882   Min.   :5.00  
##  1st Qu.:221   regionM:19250   CO2    :  882   1st Qu.:5.00  
##  Median :442   region2:  100   CO3    :  882   Median :5.00  
##  Mean   :442                   CO4    :  882   Mean   :5.27  
##  3rd Qu.:662                   CO5    :  882   3rd Qu.:5.00  
##  Max.   :882                   CO6    :  882   Max.   :8.66  
##                                (Other):16758                 
##   sampleDepth    group     
##  Min.   :27.6   CT  :7056  
##  1st Qu.:28.4   CO  :7938  
##  Median :28.7   ETOH:7056  
##  Mean   :28.6              
##  3rd Qu.:28.9              
##  Max.   :29.3              
## 
</pre></div>
<div class="source"><pre class="knitr r">
## Attempt to fit a couple GEE models
gfit.ind <- geeglm(coverage ~ sampleDepth + group + region, id = sample, data = covdata[[i]], family = gaussian, corstr = "independence")
gfit.ind
</pre></div>
<div class="output"><pre class="knitr r">## 
## Call:
## geeglm(formula = coverage ~ sampleDepth + group + region, family = gaussian, 
##     data = covdata[[i]], id = sample, corstr = "independence")
## 
## Coefficients:
##   (Intercept)   sampleDepth       groupCO     groupETOH regionregionM 
##       4.16022       0.10442      -0.11309      -0.03026      -2.09715 
## regionregion2 
##      -0.12466 
## 
## Degrees of Freedom: 22050 Total (i.e. Null);  22044 Residual
## 
## Scale Link:                   identity
## Estimated Scale Parameters:  [1] 0.05323
## 
## Correlation:  Structure = independence  
## Number of clusters:   25   Maximum cluster size: 882
</pre></div>
<div class="source"><pre class="knitr r">gfit.ex <- geeglm(coverage ~ sampleDepth + group + region, id = sample, data = covdata[[i]], family = gaussian, corstr = "exchangeable")
gfit.ex
</pre></div>
<div class="output"><pre class="knitr r">## 
## Call:
## geeglm(formula = coverage ~ sampleDepth + group + region, family = gaussian, 
##     data = covdata[[i]], id = sample, corstr = "exchangeable")
## 
## Coefficients:
##   (Intercept)   sampleDepth       groupCO     groupETOH regionregionM 
##       4.16022       0.10442      -0.11309      -0.03026      -2.09715 
## regionregion2 
##      -0.12466 
## 
## Degrees of Freedom: 22050 Total (i.e. Null);  22044 Residual
## 
## Scale Link:                   identity
## Estimated Scale Parameters:  [1] 0.05323
## 
## Correlation:  Structure = exchangeable    Link = identity 
## Estimated Correlation Parameters:
##   alpha 
## 0.01904 
## 
## Number of clusters:   25   Maximum cluster size: 882
</pre></div>
<div class="source"><pre class="knitr r">gfit.ar <- geeglm(coverage ~ sampleDepth + group + region, id = sample, data = covdata[[i]], family = gaussian, corstr = "ar1")
gfit.ar
</pre></div>
<div class="output"><pre class="knitr r">## 
## Call:
## geeglm(formula = coverage ~ sampleDepth + group + region, family = gaussian, 
##     data = covdata[[i]], id = sample, corstr = "ar1")
## 
## Coefficients:
##   (Intercept)   sampleDepth       groupCO     groupETOH regionregionM 
##       2.09502       0.17000      -0.17425      -0.05274      -1.93964 
## regionregion2 
##       0.06386 
## 
## Degrees of Freedom: 22050 Total (i.e. Null);  22044 Residual
## 
## Scale Link:                   identity
## Estimated Scale Parameters:  [1] 0.06372
## 
## Correlation:  Structure = ar1    Link = identity 
## Estimated Correlation Parameters:
##  alpha 
## 0.9845 
## 
## Number of clusters:   25   Maximum cluster size: 882
</pre></div>
</div></div>



# Reproducibility

Date the report was generated.

<div class="chunk" id="reproducibility1"><div class="rcode"><div class="output"><pre class="knitr r">## [1] "2013-12-14 18:49:30 EST"
</pre></div>
</div></div>


Wallclock time spent generating the report.

<div class="chunk" id="reproducibility2"><div class="rcode"><div class="output"><pre class="knitr r">## Time difference of 2.434 hours
</pre></div>
</div></div>


`R` session information.

<div class="chunk" id="reproducibility3"><div class="rcode"><div class="output"><pre class="knitr r">## R version 3.0.2 Patched (2013-10-17 r64066)
## Platform: x86_64-unknown-linux-gnu (64-bit)
## 
## locale:
##  [1] LC_CTYPE=en_US.iso885915       LC_NUMERIC=C                  
##  [3] LC_TIME=en_US.iso885915        LC_COLLATE=en_US.iso885915    
##  [5] LC_MONETARY=en_US.iso885915    LC_MESSAGES=en_US.iso885915   
##  [7] LC_PAPER=en_US.iso885915       LC_NAME=C                     
##  [9] LC_ADDRESS=C                   LC_TELEPHONE=C                
## [11] LC_MEASUREMENT=en_US.iso885915 LC_IDENTIFICATION=C           
## 
## attached base packages:
## [1] parallel  methods   stats     graphics  grDevices utils     datasets 
## [8] base     
## 
## other attached packages:
##  [1] reshape2_1.2.2                          
##  [2] TxDb.Hsapiens.UCSC.hg19.knownGene_2.10.1
##  [3] GenomicFeatures_1.14.2                  
##  [4] AnnotationDbi_1.24.0                    
##  [5] Biobase_2.22.0                          
##  [6] derfinder_0.0.42                        
##  [7] RcppArmadillo_0.3.930.1                 
##  [8] Rcpp_0.10.6                             
##  [9] geepack_1.1-6                           
## [10] GenomicRanges_1.14.4                    
## [11] XVector_0.2.0                           
## [12] IRanges_1.20.6                          
## [13] BiocGenerics_0.8.0                      
## [14] knitrBootstrap_0.9.0                    
## [15] getopt_1.20.0                           
## 
## loaded via a namespace (and not attached):
##  [1] biomaRt_2.18.0          Biostrings_2.30.1      
##  [3] biovizBase_1.10.5       bitops_1.0-6           
##  [5] BSgenome_1.30.0         bumphunter_1.3.6       
##  [7] Cairo_1.5-3             cluster_1.14.4         
##  [9] codetools_0.2-8         colorspace_1.2-4       
## [11] DBI_0.2-7               dichromat_2.0-0        
## [13] digest_0.6.4            doRNG_1.5.5            
## [15] evaluate_0.5.1          foreach_1.4.1          
## [17] formatR_0.10            Formula_1.1-1          
## [19] ggbio_1.8.8             ggplot2_0.9.3.1        
## [21] grid_3.0.2              gridExtra_0.9.1        
## [23] gtable_0.1.2            Hmisc_3.13-0           
## [25] iterators_1.0.6         knitr_1.5              
## [27] labeling_0.2            lattice_0.20-24        
## [29] locfit_1.5-9.1          markdown_0.6.3         
## [31] MASS_7.3-29             matrixStats_0.8.12     
## [33] munsell_0.4.2           pkgmaker_0.17.4        
## [35] plyr_1.8                proto_0.3-10           
## [37] qvalue_1.36.0           RColorBrewer_1.0-5     
## [39] RCurl_1.95-4.1          registry_0.2           
## [41] R.methodsS3_1.5.2       rngtools_1.2.3         
## [43] Rsamtools_1.14.2        RSQLite_0.11.4         
## [45] rtracklayer_1.22.0      scales_0.2.3           
## [47] splines_3.0.2           stats4_3.0.2           
## [49] stringr_0.6.2           survival_2.37-4        
## [51] tcltk_3.0.2             tools_3.0.2            
## [53] VariantAnnotation_1.8.8 XML_3.98-1.1           
## [55] xtable_1.7-1            zlibbioc_1.8.0
</pre></div>
</div></div>

