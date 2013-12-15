Run GEE with independence working correlation
=============================================

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
## gee
# install.packages("geepack")
library("geepack")

## for mclapply
library("parallel")
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
## '/home/bst/student/lcollado/756final_code/results/derHippo/chr6/geeInd'
## already exists
</pre></div>
<div class="source"><pre class="knitr r">
## Want to save some 'object'? Use:
# save(object, file=file.path(wdir, "object.Rdata"))
</pre></div>
</div></div>



# GEE independence


<div class="chunk" id="rungee"><div class="rcode"><div class="source"><pre class="knitr r">## Load data
load(file.path(ddir, "geeAR1", "covdata.used.Rdata"))
load(file.path(ddir, "geeAR1", "pairs.used.Rdata"))


## Run GEE
idx <- seq_len(length(covdata.used))
## testing:
#idx <- 1:2
myGEE <- function(i, corstr) {
	geeglm(coverage ~ sampleDepth + group + region, id = sample, data = covdata.used[[i]], family = gaussian, corstr = corstr)
}

## GEE AR1
if(opt$verbose) message(paste(Sys.time(), "running GEE with AR1"))
</pre></div>
<div class="message"><pre class="knitr r">## 2013-12-14 18:35:48 running GEE with AR1
</pre></div>
<div class="source"><pre class="knitr r">geeInd <- mclapply(idx, myGEE, corstr="independence", mc.cores=20)
names(geeInd) <- names(covdata.used)[idx]
save(geeInd, file=file.path(wdir, "geeInd.Rdata"))

## Show an example:
summary(geeInd[[1]])
</pre></div>
<div class="output"><pre class="knitr r">## 
## Call:
## geeglm(formula = coverage ~ sampleDepth + group + region, family = gaussian, 
##     data = covdata.used[[i]], id = sample, corstr = corstr)
## 
##  Coefficients:
##               Estimate Std.err   Wald Pr(>|W|)    
## (Intercept)    -5.5445  3.6388   2.32   0.1276    
## sampleDepth     0.4048  0.1285   9.93   0.0016 ** 
## groupCO        -0.8091  0.1584  26.10  3.2e-07 ***
## groupETOH      -0.5068  0.1604   9.98   0.0016 ** 
## regionregionM   0.3421  0.0376  82.70  < 2e-16 ***
## regionregion2   0.7552  0.0730 107.10  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Estimated Scale Parameters:
##             Estimate Std.err
## (Intercept)    0.151  0.0277
## 
## Correlation: Structure = independenceNumber of clusters:   25   Maximum cluster size: 141
</pre></div>
<div class="source"><pre class="knitr r">

## Extract region2 coef estimate, st.error, and Wald stat
myGEE.stat <- function(y) {
	beta <- c(coef(y)["regionregion2"], y$geese$alpha)
	## Assumes that geeglm(std.err="san.se") which is the default value
	vbeta <- sqrt(c(y$geese$vbeta[6, 6], y$geese$valpha))
	wald <- (beta/vbeta)^2
	pval <- 1 - pchisq(wald, df=1)
	df <- data.frame(coef=c("region2", "alpha"), estimate=beta, stderr=vbeta, wald=wald, pval=pval)
	rownames(df) <- seq_len(nrow(df))
	return(df)
}
geeInd.stat <- lapply(geeInd, myGEE.stat)
</pre></div>
<div class="warning"><pre class="knitr r">## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
## Warning: row names were found from a short variable and have been discarded
</pre></div>
<div class="source"><pre class="knitr r">save(geeInd.stat, file=file.path(wdir, "geeInd.stat.Rdata"))

## Show an example:
geeInd.stat[[1]]
</pre></div>
<div class="output"><pre class="knitr r">##      coef estimate stderr wald pval
## 1 region2    0.755  0.073  107    0
## 2   alpha    0.755  0.073  107    0
</pre></div>
</div></div>











# Reproducibility

Date the report was generated.

<div class="chunk" id="reproducibility1"><div class="rcode"><div class="output"><pre class="knitr r">## [1] "2013-12-14 18:36:52 EST"
</pre></div>
</div></div>


Wallclock time spent generating the report.

<div class="chunk" id="reproducibility2"><div class="rcode"><div class="output"><pre class="knitr r">## Time difference of 1.07 mins
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
## [1] parallel  stats     graphics  grDevices utils     datasets  base     
## 
## other attached packages:
## [1] geepack_1.1-6        knitrBootstrap_0.9.0 getopt_1.20.0       
## 
## loaded via a namespace (and not attached):
## [1] evaluate_0.5.1 formatR_0.10   knitr_1.5      markdown_0.6.3
## [5] stringr_0.6.2  tools_3.0.2
</pre></div>
</div></div>
