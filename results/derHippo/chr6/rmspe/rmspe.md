Calculate RMSPE, qvalues and compare with exons
===============================================

# Setup

## Libraries

<div class="chunk" id="setup"><div class="rcode"><div class="source"><pre class="knitr r">## Load libraries
## Available from http://cran.r-project.org/web/packages/getopt/index.html
# install.packages("getopt")
library("getopt")

## Available from http://cran.at.r-project.org/web/packages/knitrBootstrap/index.html
# install.packages("knitrBootstrap")
library("knitrBootstrap")

# --- Specify any other libraries you need
# --- Not needed (since you load them in template.R), but good for the report
# --- Anyhow, just copying and pasting from template.R is good
# install.packages("cvTools")
library("cvTools")

# install.packages("geepack")
library("geepack")

# install.packages("ggplot2")
library("ggplot2")

# source("http://bioconductor.org/biocLite.R")
# biocLite("qvalue")
library("qvalue")

# source("http://bioconductor.org/biocLite.R")
# biocLite("GenomicRanges")
library("GenomicRanges")
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
<div class="source"><pre class="knitr r">ddir <- file.path(opt$dirResult, opt$project, chr) # data directory (might change say if step2 uses the results from step2)
wdir <- file.path(opt$dirResult, opt$project, chr, opt$results) # working dir
dir.create(wdir, recursive=TRUE)
</pre></div>
<div class="warning"><pre class="knitr r">## Warning:
## '/Users/lcollado/enigma2/756final_code/results/derHippo/chr6/rmspe'
## already exists
</pre></div>
<div class="source"><pre class="knitr r">
## Want to save some 'object'? Use:
# save(object, file=file.path(wdir, "object.Rdata"))
</pre></div>
</div></div>





# RMSPE

<div class="chunk" id="loadData"><div class="rcode"><div class="source"><pre class="knitr r">load(file.path(ddir, "geeAR1", "geeAR1.Rdata"))
load(file.path(ddir, "geeInd", "geeInd.Rdata"))
load(file.path(ddir, "geeEx", "geeEx.Rdata"))
</pre></div>
</div></div>


## Calculate the RMSPE

<div class="chunk" id="rmspe"><div class="rcode"><div class="source"><pre class="knitr r">rmspe.all <- vector("list", length(geeAR1))
names(rmspe.all) <- names(geeAR1)

rmspe.all <- lapply(names(rmspe.all), function(x) {
	ar <- unlist(rmspe(y=geeAR1[[x]]$y, yHat=geeAR1[[x]]$fitted.values, includeSE=TRUE))
	ind <- unlist(rmspe(y=geeInd[[x]]$y, yHat=geeInd[[x]]$fitted.values, includeSE=TRUE))
	ex <- unlist(rmspe(y=geeEx[[x]]$y, yHat=geeEx[[x]]$fitted.values, includeSE=TRUE))
	df <- data.frame(rbind(ar, ind, ex))
	df$method <- factor(c("AR1", "Ind", "Ex"))
	df$pairID <- rep(as.integer(x), 3)
	rownames(df) <- NULL
	return(df)
})
rmspe.all <- do.call(rbind, rmspe.all)
save(rmspe.all, file=file.path(wdir, "rmspe.all.Rdata"))
</pre></div>
</div></div>


## Explore the results

<div class="chunk" id="rmspeEDA"><div class="rcode"><div class="source"><pre class="knitr r">ggplot(rmspe.all, aes(x=method, y=rmspe)) + geom_violin()
</pre></div>
<div class="rimage default"><img src="figure/rmspeEDA1.png" title="plot of chunk rmspeEDA" alt="plot of chunk rmspeEDA" class="plot" /></div>
<div class="source"><pre class="knitr r">ggplot(rmspe.all, aes(x=method, y=se)) + geom_violin()
</pre></div>
<div class="rimage default"><img src="figure/rmspeEDA2.png" title="plot of chunk rmspeEDA" alt="plot of chunk rmspeEDA" class="plot" /></div>
<div class="source"><pre class="knitr r">plot(rmspe.all$rmspe[rmspe.all$method=="Ind"], rmspe.all$rmspe[rmspe.all$method=="Ex"])
abline(0, 1, col="red")
</pre></div>
<div class="rimage default"><img src="figure/rmspeEDA3.png" title="plot of chunk rmspeEDA" alt="plot of chunk rmspeEDA" class="plot" /></div>
<div class="source"><pre class="knitr r">plot(rmspe.all$rmspe[rmspe.all$method=="Ind"], rmspe.all$rmspe[rmspe.all$method=="AR1"])
abline(0, 1, col="red")
</pre></div>
<div class="rimage default"><img src="figure/rmspeEDA4.png" title="plot of chunk rmspeEDA" alt="plot of chunk rmspeEDA" class="plot" /></div>
<div class="source"><pre class="knitr r">plot(rmspe.all$se[rmspe.all$method=="Ind"], rmspe.all$se[rmspe.all$method=="Ex"])
abline(0, 1, col="red")
</pre></div>
<div class="rimage default"><img src="figure/rmspeEDA5.png" title="plot of chunk rmspeEDA" alt="plot of chunk rmspeEDA" class="plot" /></div>
<div class="source"><pre class="knitr r">plot(rmspe.all$se[rmspe.all$method=="Ind"], rmspe.all$se[rmspe.all$method=="AR1"])
abline(0, 1, col="red")
</pre></div>
<div class="rimage default"><img src="figure/rmspeEDA6.png" title="plot of chunk rmspeEDA" alt="plot of chunk rmspeEDA" class="plot" /></div>
<div class="source"><pre class="knitr r">ggplot(subset(rmspe.all, method!="Ex"), aes(x=rmspe, y=se, color=method)) + geom_point()
</pre></div>
<div class="rimage default"><img src="figure/rmspeEDA7.png" title="plot of chunk rmspeEDA" alt="plot of chunk rmspeEDA" class="plot" /></div>
<div class="source"><pre class="knitr r">ggplot(subset(rmspe.all, method!="Ex"), aes(x=pairID, y=rmspe, color=method)) + geom_point()
</pre></div>
<div class="rimage default"><img src="figure/rmspeEDA8.png" title="plot of chunk rmspeEDA" alt="plot of chunk rmspeEDA" class="plot" /></div>
<div class="source"><pre class="knitr r">ggplot(subset(rmspe.all, method!="Ex"), aes(x=pairID, y=se, color=method)) + geom_point()
</pre></div>
<div class="rimage default"><img src="figure/rmspeEDA9.png" title="plot of chunk rmspeEDA" alt="plot of chunk rmspeEDA" class="plot" /></div>
<div class="source"><pre class="knitr r">tapply(rmspe.all$rmspe, rmspe.all$method, summary)
</pre></div>
<div class="output"><pre class="knitr r">## $AR1
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.0380  0.0851  0.1260  0.1860  0.2120  1.4800 
## 
## $Ex
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.0380  0.0821  0.1230  0.1700  0.1960  1.1800 
## 
## $Ind
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.0380  0.0821  0.1230  0.1700  0.1960  1.1800
</pre></div>
<div class="source"><pre class="knitr r">tapply(rmspe.all$se, rmspe.all$method, summary)
</pre></div>
<div class="output"><pre class="knitr r">## $AR1
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000824 0.002650 0.003680 0.004230 0.005350 0.018500 
## 
## $Ex
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000823 0.002460 0.003390 0.004070 0.005010 0.018400 
## 
## $Ind
##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
## 0.000823 0.002460 0.003390 0.004070 0.005010 0.018400
</pre></div>
</div></div>



# Compare on exons

## Adjust q-values and perform tests

<div class="chunk" id="loadStats"><div class="rcode"><div class="source"><pre class="knitr r">load(file.path(ddir, "geeAR1", "geeAR1.stat.Rdata"))
load(file.path(ddir, "geeInd", "geeInd.stat.Rdata"))
load(file.path(ddir, "geeEx", "geeEx.stat.Rdata"))

pvals <- vector("list", length(geeAR1))
names(pvals) <- names(geeAR1)

pvals <- lapply(names(pvals), function(x) {
	ar <- geeAR1.stat[[x]]$pval[1]
	ind <- geeInd.stat[[x]]$pval[1]
	ex <- geeEx.stat[[x]]$pval[1]
	df <- data.frame(pval=c(ar, ind, ex))
	df$method <- factor(c("AR1", "Ind", "Ex"))
	df$pairID <- rep(as.integer(x), 3)
	return(df)
})
pvals <- do.call(rbind, pvals)
qvals <- rep(NA, nrow(pvals))
for(i in c("AR1", "Ind", "Ex")) {
	qvals[pvals$method == i] <- qvalue(pvals$pval[pvals$method == i])$qvalues
}
pvals$qval <- qvals
pvals$pvalSig <- pvals$pval < 0.05
pvals$qvalSig <- pvals$qval < 0.10
</pre></div>
</div></div>


<div class="chunk" id="explorePvals"><div class="rcode"><div class="source"><pre class="knitr r">summary(pvals)
</pre></div>
<div class="output"><pre class="knitr r">##       pval        method        pairID         qval        
##  Min.   :0.0000   AR1:300   Min.   :  1   Min.   :0.00000  
##  1st Qu.:0.0000   Ex :300   1st Qu.:234   1st Qu.:0.00000  
##  Median :0.0002   Ind:300   Median :435   Median :0.00007  
##  Mean   :0.1258             Mean   :432   Mean   :0.03588  
##  3rd Qu.:0.0843             3rd Qu.:630   3rd Qu.:0.02841  
##  Max.   :0.9850             Max.   :888   Max.   :0.26950  
##   pvalSig         qvalSig       
##  Mode :logical   Mode :logical  
##  FALSE:252       FALSE:133      
##  TRUE :648       TRUE :767      
##  NA's :0         NA's :0        
##                                 
## 
</pre></div>
<div class="source"><pre class="knitr r">ggplot(pvals, aes(x=method, y=pval)) + geom_violin()
</pre></div>
<div class="rimage default"><img src="figure/explorePvals1.png" title="plot of chunk explorePvals" alt="plot of chunk explorePvals" class="plot" /></div>
<div class="source"><pre class="knitr r">ggplot(pvals, aes(x=method, y=qval)) + geom_violin()
</pre></div>
<div class="rimage default"><img src="figure/explorePvals2.png" title="plot of chunk explorePvals" alt="plot of chunk explorePvals" class="plot" /></div>
<div class="source"><pre class="knitr r">plot(pvals$qval[pvals$method=="Ind"], pvals$qval[pvals$method=="Ex"])
abline(0, 1, col="red")
</pre></div>
<div class="rimage default"><img src="figure/explorePvals3.png" title="plot of chunk explorePvals" alt="plot of chunk explorePvals" class="plot" /></div>
<div class="source"><pre class="knitr r">plot(pvals$qval[pvals$method=="Ind"], pvals$qval[pvals$method=="AR1"])
abline(0, 1, col="red")
</pre></div>
<div class="rimage default"><img src="figure/explorePvals4.png" title="plot of chunk explorePvals" alt="plot of chunk explorePvals" class="plot" /></div>
<div class="source"><pre class="knitr r">ggplot(subset(pvals, method!="Ex"), aes(x=pairID, y=qval, colour=method)) + geom_point()
</pre></div>
<div class="rimage default"><img src="figure/explorePvals5.png" title="plot of chunk explorePvals" alt="plot of chunk explorePvals" class="plot" /></div>
</div></div>


# Region pairs and exons

<div class="chunk" id="pairsExons"><div class="rcode"><div class="source"><pre class="knitr r">load(file.path(ddir, "pairs.Rdata"))
load(file.path(ddir, "exons.Rdata"))
## Per region pair, find which exons they overlap
## Then classify if they overlap the same exon or different ones

ex.info <- lapply(unique(pvals$pairID), function(x) {
	reg <- GRanges(seqnames=rep(chr, 2), ranges=IRanges(start=c(pairs$start1[x], pairs$start2[x]), end=c(pairs$end1[x], pairs$end2[x])))
	ov <- findOverlaps(reg, exons)
	if(!all(c(1, 2) %in% queryHits(ov))) {
		return("no-exons")
	} else if(any(duplicated(subjectHits(ov))) & all(c(1, 2) %in% queryHits(ov))) {
		return("same-exon")
	} else {
		return("diff-exon")
	}
})
names(ex.info) <- unique(pvals$pairID)
ex.info <- unlist(ex.info)
table(ex.info)
</pre></div>
<div class="output"><pre class="knitr r">## ex.info
## diff-exon  no-exons same-exon 
##        14        10       276
</pre></div>
<div class="source"><pre class="knitr r">pvals$exonStatus <- factor(rep(ex.info, each=3), c("no-exons", "diff-exon", "same-exon"))
save(pvals, file=file.path(wdir, "pvals.Rdata"))
</pre></div>
</div></div>


<div class="chunk" id="pairsExonsEDA"><div class="rcode"><div class="source"><pre class="knitr r">ggplot(pvals, aes(x=method, y=pval)) + geom_violin() + facet_grid(exonStatus~.)
</pre></div>
<div class="rimage default"><img src="figure/pairsExonsEDA1.png" title="plot of chunk pairsExonsEDA" alt="plot of chunk pairsExonsEDA" class="plot" /></div>
<div class="source"><pre class="knitr r">ggplot(pvals, aes(x=method, y=qval)) + geom_violin() + facet_grid(exonStatus~.)
</pre></div>
<div class="rimage default"><img src="figure/pairsExonsEDA2.png" title="plot of chunk pairsExonsEDA" alt="plot of chunk pairsExonsEDA" class="plot" /></div>
<div class="source"><pre class="knitr r">
ggplot(subset(pvals, method!="Ex"), aes(x=pairID, y=qval, colour=method)) + geom_point() + facet_grid(exonStatus~.)
</pre></div>
<div class="rimage default"><img src="figure/pairsExonsEDA3.png" title="plot of chunk pairsExonsEDA" alt="plot of chunk pairsExonsEDA" class="plot" /></div>
<div class="source"><pre class="knitr r">
## By exon status

comp.status <- vector("list", 3)
names(comp.status) <- c("no-exons", "diff-exon", "same-exon")
for(ex.status in names(comp.status)) {
	
	## Judging by p-value
	x11 <- with(subset(pvals, exonStatus == ex.status & method != "Ex"), tapply(pvalSig, method, summary))
	casesPval <- do.call(rbind, with(subset(pvals, exonStatus == ex.status & method != "Ex"), tapply(pvalSig, pairID, as.integer)))
	x12 <- table(apply(casesPval, 1, paste, collapse="-"))

	## Judging by q-value
	x21 <- with(subset(pvals, exonStatus == ex.status & method != "Ex"), tapply(qvalSig, method, summary))
	casesQval <- do.call(rbind, with(subset(pvals, exonStatus == ex.status & method != "Ex"), tapply(qvalSig, pairID, as.integer)))
	x22 <- table(apply(casesQval, 1, paste, collapse="-"))
	comp.status[[ex.status]] <- list(pval=list(x11, x12), qval=list(x21, x22))
}

## Agreement between AR1 and Ind by exon status. 0 means that it was not significant, 1 that it was.
## By pvalue
lapply(comp.status, function(x) { x$pval[[2]]})
</pre></div>
<div class="output"><pre class="knitr r">## $`no-exons`
## 
## 0-0 0-1 1-0 1-1 
##   1   3   1   5 
## 
## $`diff-exon`
## 
## 0-0 0-1 1-1 
##   1   1  12 
## 
## $`same-exon`
## 
## 0-0 0-1 1-0 1-1 
##  48  34  31 163
</pre></div>
<div class="source"><pre class="knitr r">## By qvalue
lapply(comp.status, function(x) { x$qval[[2]]})
</pre></div>
<div class="output"><pre class="knitr r">## $`no-exons`
## 
## 0-1 1-0 1-1 
##   1   2   7 
## 
## $`diff-exon`
## 
## 0-1 1-1 
##   1  13 
## 
## $`same-exon`
## 
## 0-0 0-1 1-0 1-1 
##  19  24  23 210
</pre></div>
<div class="source"><pre class="knitr r">
save(comp.status, file=file.path(wdir, "comp.status.Rdata"))
</pre></div>
</div></div>













# Reproducibility

Date the report was generated.

<div class="chunk" id="reproducibility1"><div class="rcode"><div class="output"><pre class="knitr r">## [1] "2013-12-16 13:44:49 EST"
</pre></div>
</div></div>


Wallclock time spent generating the report.

<div class="chunk" id="reproducibility2"><div class="rcode"><div class="output"><pre class="knitr r">## Time difference of 40.94 secs
</pre></div>
</div></div>


`R` session information.

<div class="chunk" id="reproducibility3"><div class="rcode"><div class="output"><pre class="knitr r">## R version 3.0.2 (2013-09-25)
## Platform: x86_64-apple-darwin10.8.0 (64-bit)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] parallel  methods   stats     graphics  grDevices utils     datasets 
## [8] base     
## 
## other attached packages:
##  [1] GenomicRanges_1.14.3 XVector_0.2.0        IRanges_1.20.6      
##  [4] BiocGenerics_0.8.0   qvalue_1.36.0        ggplot2_0.9.3.1     
##  [7] geepack_1.1-6        cvTools_0.3.2        robustbase_0.9-10   
## [10] lattice_0.20-24      knitrBootstrap_0.9.0 getopt_1.20.0       
## 
## loaded via a namespace (and not attached):
##  [1] Cairo_1.5-3        colorspace_1.2-4   dichromat_2.0-0   
##  [4] digest_0.6.4       evaluate_0.5.1     formatR_0.10      
##  [7] grid_3.0.2         gtable_0.1.2       knitr_1.5         
## [10] labeling_0.2       markdown_0.6.3     MASS_7.3-29       
## [13] munsell_0.4.2      plyr_1.8           proto_0.3-10      
## [16] RColorBrewer_1.0-5 reshape2_1.2.2     scales_0.2.3      
## [19] stats4_3.0.2       stringr_0.6.2      tcltk_3.0.2       
## [22] tools_3.0.2
</pre></div>
</div></div>

