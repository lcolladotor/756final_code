Run GEE with exchangeable working correlation
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
## '/home/bst/student/lcollado/756final_code/results/derHippo/chr6/geeEx'
## already exists
</pre></div>
<div class="source"><pre class="knitr r">
## Want to save some 'object'? Use:
# save(object, file=file.path(wdir, "object.Rdata"))
</pre></div>
</div></div>



# GEE exchangeable


<div class="chunk" id="rungee"><div class="rcode"><div class="source"><pre class="knitr r">## Load data
load(file.path(ddir, "geeAR1", "covdata.Rdata"))
</pre></div>
<div class="warning"><pre class="knitr r">## Warning: cannot open compressed file
## '/home/bst/student/lcollado/756final_code/results/derHippo/chr6/geeAR1/covdata.Rdata',
## probable reason 'No such file or directory'
</pre></div>
<div class="error"><pre class="knitr r">## Error: cannot open the connection
</pre></div>
<div class="source"><pre class="knitr r">load(file.path(ddir, "geeAR1", "pairs.Rdata"))
</pre></div>
<div class="warning"><pre class="knitr r">## Warning: cannot open compressed file
## '/home/bst/student/lcollado/756final_code/results/derHippo/chr6/geeAR1/pairs.Rdata',
## probable reason 'No such file or directory'
</pre></div>
<div class="error"><pre class="knitr r">## Error: cannot open the connection
</pre></div>
<div class="source"><pre class="knitr r">

## Run GEE
idx <- seq_len(length(covdata.used))
</pre></div>
<div class="error"><pre class="knitr r">## Error: object 'covdata.used' not found
</pre></div>
<div class="source"><pre class="knitr r">## testing:
#idx <- 1:2
myGEE <- function(i, corstr) {
	geeglm(coverage ~ sampleDepth + group + region, id = sample, data = covdata.used[[i]], family = gaussian, corstr = corstr)
}

## GEE AR1
if(opt$verbose) message(paste(Sys.time(), "running GEE with AR1"))
</pre></div>
<div class="message"><pre class="knitr r">## 2013-12-14 18:19:48 running GEE with AR1
</pre></div>
<div class="source"><pre class="knitr r">geeEx <- mclapply(idx, myGEE, corstr="exchangeable", mc.cores=20)
</pre></div>
<div class="error"><pre class="knitr r">## Error: object 'idx' not found
</pre></div>
<div class="source"><pre class="knitr r">names(geeEx) <- names(covdata.used)[idx]
</pre></div>
<div class="error"><pre class="knitr r">## Error: object 'covdata.used' not found
</pre></div>
<div class="source"><pre class="knitr r">save(geeEx, file=file.path(wdir, "geeEx.Rdata"))
</pre></div>
<div class="error"><pre class="knitr r">## Error: object 'geeEx' not found
</pre></div>
<div class="source"><pre class="knitr r">
## Show an example:
summary(geeEx[[1]])
</pre></div>
<div class="error"><pre class="knitr r">## Error: object 'geeEx' not found
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
geeEx.stat <- lapply(geeEx, myGEE.stat)
</pre></div>
<div class="error"><pre class="knitr r">## Error: object 'geeEx' not found
</pre></div>
<div class="source"><pre class="knitr r">save(geeEx.stat, file=file.path(wdir, "geeEx.stat.Rdata"))
</pre></div>
<div class="error"><pre class="knitr r">## Error: object 'geeEx.stat' not found
</pre></div>
<div class="source"><pre class="knitr r">
## Show an example:
geeEx.stat[[1]]
</pre></div>
<div class="error"><pre class="knitr r">## Error: object 'geeEx.stat' not found
</pre></div>
</div></div>











# Reproducibility

Date the report was generated.

<div class="chunk" id="reproducibility1"><div class="rcode"><div class="output"><pre class="knitr r">## [1] "2013-12-14 18:19:48 EST"
</pre></div>
</div></div>


Wallclock time spent generating the report.

<div class="chunk" id="reproducibility2"><div class="rcode"><div class="output"><pre class="knitr r">## Time difference of 0.482 secs
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

