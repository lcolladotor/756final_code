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
## '/home/bst/student/lcollado/756final_code/results/derHippo/chr22/geeEx'
## already exists
</pre></div>
<div class="source"><pre class="knitr r">
## Want to save some 'object'? Use:
# save(object, file=file.path(wdir, "object.Rdata"))
</pre></div>
</div></div>



# GEE exchangeable


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
myGEE.int <- function(i, corstr) {
	geeglm(coverage ~ sampleDepth + group * region, id = sample, data = covdata.used[[i]], family = gaussian, corstr = corstr)
}

## GEE Ex
if(opt$verbose) message(paste(Sys.time(), "running GEE with AR1"))
</pre></div>
<div class="message"><pre class="knitr r">## 2013-12-20 10:18:37 running GEE with AR1
</pre></div>
<div class="source"><pre class="knitr r">geeEx <- mclapply(idx, myGEE, corstr="exchangeable", mc.cores=20)
names(geeEx) <- names(covdata.used)[idx]
save(geeEx, file=file.path(wdir, "geeEx.Rdata"))

## GEE Ex - int
if(opt$verbose) message(paste(Sys.time(), "running GEE with AR1 - interaction"))
</pre></div>
<div class="message"><pre class="knitr r">## 2013-12-20 10:19:18 running GEE with AR1 - interaction
</pre></div>
<div class="source"><pre class="knitr r">geeEx.int <- mclapply(idx, myGEE.int, corstr="exchangeable", mc.cores=20)
names(geeEx.int) <- names(covdata.used)[idx]
save(geeEx.int, file=file.path(wdir, "geeEx.int.Rdata"))

## Show an example:
geeEx[[1]]
</pre></div>
<div class="output"><pre class="knitr r">## 
## Call:
## geeglm(formula = coverage ~ sampleDepth + group + region, family = gaussian, 
##     data = covdata.used[[i]], id = sample, corstr = corstr)
## 
## Coefficients:
##   (Intercept)   sampleDepth       groupCO     groupETOH regionregionM 
##      3.677174      0.054732     -0.166705     -0.150384      0.001525 
## regionregion2 
##      0.007073 
## 
## Degrees of Freedom: 200 Total (i.e. Null);  194 Residual
## 
## Scale Link:                   identity
## Estimated Scale Parameters:  [1] 0.00382
## 
## Correlation:  Structure = exchangeable    Link = identity 
## Estimated Correlation Parameters:
##  alpha 
## 0.9295 
## 
## Number of clusters:   25   Maximum cluster size: 8
</pre></div>
<div class="source"><pre class="knitr r">summary(geeEx[[1]])
</pre></div>
<div class="output"><pre class="knitr r">## 
## Call:
## geeglm(formula = coverage ~ sampleDepth + group + region, family = gaussian, 
##     data = covdata.used[[i]], id = sample, corstr = corstr)
## 
##  Coefficients:
##               Estimate  Std.err  Wald Pr(>|W|)    
## (Intercept)    3.67717  0.72270 25.89  3.6e-07 ***
## sampleDepth    0.05473  0.02475  4.89    0.027 *  
## groupCO       -0.16671  0.03628 21.11  4.3e-06 ***
## groupETOH     -0.15038  0.03835 15.38  8.8e-05 ***
## regionregionM  0.00152  0.00438  0.12    0.728    
## regionregion2  0.00707  0.00623  1.29    0.256    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Estimated Scale Parameters:
##             Estimate Std.err
## (Intercept)  0.00382 0.00123
## 
## Correlation: Structure = exchangeable  Link = identity 
## 
## Estimated Correlation Parameters:
##       Estimate Std.err
## alpha    0.929  0.0265
## Number of clusters:   25   Maximum cluster size: 8
</pre></div>
<div class="source"><pre class="knitr r">geeEx.int[[1]]
</pre></div>
<div class="output"><pre class="knitr r">## 
## Call:
## geeglm(formula = coverage ~ sampleDepth + group * region, family = gaussian, 
##     data = covdata.used[[i]], id = sample, corstr = corstr)
## 
## Coefficients:
##             (Intercept)             sampleDepth                 groupCO 
##                3.676737                0.054732               -0.162149 
##               groupETOH           regionregionM           regionregion2 
##               -0.154147                0.000344                0.011179 
##   groupCO:regionregionM groupETOH:regionregionM   groupCO:regionregion2 
##               -0.003592                0.007731               -0.011040 
## groupETOH:regionregion2 
##               -0.000412 
## 
## Degrees of Freedom: 200 Total (i.e. Null);  190 Residual
## 
## Scale Link:                   identity
## Estimated Scale Parameters:  [1] 0.00381
## 
## Correlation:  Structure = exchangeable    Link = identity 
## Estimated Correlation Parameters:
## alpha 
## 0.931 
## 
## Number of clusters:   25   Maximum cluster size: 8
</pre></div>
<div class="source"><pre class="knitr r">summary(geeEx.int[[1]])
</pre></div>
<div class="output"><pre class="knitr r">## 
## Call:
## geeglm(formula = coverage ~ sampleDepth + group * region, family = gaussian, 
##     data = covdata.used[[i]], id = sample, corstr = corstr)
## 
##  Coefficients:
##                          Estimate   Std.err  Wald Pr(>|W|)    
## (Intercept)              3.676737  0.720861 26.01  3.4e-07 ***
## sampleDepth              0.054732  0.024746  4.89    0.027 *  
## groupCO                 -0.162149  0.034311 22.33  2.3e-06 ***
## groupETOH               -0.154147  0.036286 18.05  2.2e-05 ***
## regionregionM            0.000344  0.009022  0.00    0.970    
## regionregion2            0.011179  0.014733  0.58    0.448    
## groupCO:regionregionM   -0.003592  0.011670  0.09    0.758    
## groupETOH:regionregionM  0.007731  0.010463  0.55    0.460    
## groupCO:regionregion2   -0.011040  0.017388  0.40    0.525    
## groupETOH:regionregion2 -0.000412  0.016141  0.00    0.980    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Estimated Scale Parameters:
##             Estimate Std.err
## (Intercept)  0.00381 0.00124
## 
## Correlation: Structure = exchangeable  Link = identity 
## 
## Estimated Correlation Parameters:
##       Estimate Std.err
## alpha    0.931  0.0251
## Number of clusters:   25   Maximum cluster size: 8
</pre></div>
<div class="source"><pre class="knitr r">

## Extract region2 coef estimate, st.error, and Wald stat
myGEE.stat <- function(y) {
	beta <- c(coef(y)["regionregion2"], y$geese$gamma, y$geese$alpha)
	## Assumes that geeglm(std.err="san.se") which is the default value
	vbeta <- sqrt(c(y$geese$vbeta[6, 6], y$geese$gamma, y$geese$valpha))
	wald <- (beta/vbeta)^2
	pval <- 1 - pchisq(wald, df=1)
	df <- data.frame(coef=c("region2", "gamma", "alpha"), estimate=beta, stderr=vbeta, wald=wald, pval=pval)
	rownames(df) <- seq_len(nrow(df))
	return(df)
}
geeEx.stat <- lapply(geeEx, myGEE.stat)
save(geeEx.stat, file=file.path(wdir, "geeEx.stat.Rdata"))

## Extract region2 coef estimate, interaction coef estiamtes, st.error, and Wald stat
myGEE.int.stat <- function(y) {
	beta <- c(coef(y)[c("regionregion2", "groupCO:regionregion2", "groupETOH:regionregion2")], y$geese$gamma, y$geese$alpha)
	## Assumes that geeglm(std.err="san.se") which is the default value
	vbeta <- sqrt(c(y$geese$vbeta[6, 6], y$geese$vbeta[9, 9], y$geese$vbeta[10, 10], y$geese$vgamma, y$geese$valpha))
	wald <- (beta/vbeta)^2
	pval <- 1 - pchisq(wald, df=1)
	df <- data.frame(coef=c("region2", "groupCO:region2", "groupETOH:region2", "gamma", "alpha"), estimate=beta, stderr=vbeta, wald=wald, pval=pval)
	rownames(df) <- seq_len(nrow(df))
	return(df)
}

geeEx.int.stat <- lapply(geeEx.int, myGEE.int.stat)
save(geeEx.int.stat, file=file.path(wdir, "geeEx.int.stat.Rdata"))


## Show an example:
geeEx.stat[[1]]
</pre></div>
<div class="output"><pre class="knitr r">##      coef estimate  stderr     wald  pval
## 1 region2  0.00707 0.00623 1.29e+00 0.256
## 2   gamma  0.00382 0.06181 3.82e-03 0.951
## 3   alpha  0.92946 0.02653 1.23e+03 0.000
</pre></div>
<div class="source"><pre class="knitr r">geeEx.int.stat[[1]]
</pre></div>
<div class="output"><pre class="knitr r">##                coef  estimate  stderr     wald    pval
## 1           region2  0.011179 0.01473 5.76e-01 0.44800
## 2   groupCO:region2 -0.011040 0.01739 4.03e-01 0.52548
## 3 groupETOH:region2 -0.000412 0.01614 6.50e-04 0.97966
## 4             gamma  0.003814 0.00124 9.53e+00 0.00202
## 5             alpha  0.931185 0.02515 1.37e+03 0.00000
</pre></div>
</div></div>











# Reproducibility

Date the report was generated.

<div class="chunk" id="reproducibility1"><div class="rcode"><div class="output"><pre class="knitr r">## [1] "2013-12-20 10:20:21 EST"
</pre></div>
</div></div>


Wallclock time spent generating the report.

<div class="chunk" id="reproducibility2"><div class="rcode"><div class="output"><pre class="knitr r">## Time difference of 1.73 mins
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

