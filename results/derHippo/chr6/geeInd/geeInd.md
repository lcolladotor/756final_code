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
myGEE.int <- function(i, corstr) {
	geeglm(coverage ~ sampleDepth + group * region, id = sample, data = covdata.used[[i]], family = gaussian, corstr = corstr)
}

## GEE Ind
if(opt$verbose) message(paste(Sys.time(), "running GEE with AR1"))
</pre></div>
<div class="message"><pre class="knitr r">## 2013-12-20 10:19:39 running GEE with AR1
</pre></div>
<div class="source"><pre class="knitr r">geeInd <- mclapply(idx, myGEE, corstr="independence", mc.cores=20)
names(geeInd) <- names(covdata.used)[idx]
save(geeInd, file=file.path(wdir, "geeInd.Rdata"))

## GEE Ind - int
if(opt$verbose) message(paste(Sys.time(), "running GEE with AR1 - interaction"))
</pre></div>
<div class="message"><pre class="knitr r">## 2013-12-20 10:20:42 running GEE with AR1 - interaction
</pre></div>
<div class="source"><pre class="knitr r">geeInd.int <- mclapply(idx, myGEE.int, corstr="independence", mc.cores=20)
names(geeInd.int) <- names(covdata.used)[idx]
save(geeInd.int, file=file.path(wdir, "geeInd.int.Rdata"))

## Show an example:
geeInd[[1]]
</pre></div>
<div class="output"><pre class="knitr r">## 
## Call:
## geeglm(formula = coverage ~ sampleDepth + group + region, family = gaussian, 
##     data = covdata.used[[i]], id = sample, corstr = corstr)
## 
## Coefficients:
##   (Intercept)   sampleDepth       groupCO     groupETOH regionregionM 
##       -5.5445        0.4048       -0.8091       -0.5068        0.3421 
## regionregion2 
##        0.7552 
## 
## Degrees of Freedom: 3525 Total (i.e. Null);  3519 Residual
## 
## Scale Link:                   identity
## Estimated Scale Parameters:  [1] 0.151
## 
## Correlation:  Structure = independence  
## Number of clusters:   25   Maximum cluster size: 141
</pre></div>
<div class="source"><pre class="knitr r">summary(geeInd[[1]])
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
<div class="source"><pre class="knitr r">geeInd.int[[1]]
</pre></div>
<div class="output"><pre class="knitr r">## 
## Call:
## geeglm(formula = coverage ~ sampleDepth + group * region, family = gaussian, 
##     data = covdata.used[[i]], id = sample, corstr = corstr)
## 
## Coefficients:
##             (Intercept)             sampleDepth                 groupCO 
##                  -5.641                   0.405                  -0.583 
##               groupETOH           regionregionM           regionregion2 
##                  -0.460                   0.416                   0.982 
##   groupCO:regionregionM groupETOH:regionregionM   groupCO:regionregion2 
##                  -0.175                  -0.033                  -0.520 
## groupETOH:regionregion2 
##                  -0.123 
## 
## Degrees of Freedom: 3525 Total (i.e. Null);  3515 Residual
## 
## Scale Link:                   identity
## Estimated Scale Parameters:  [1] 0.147
## 
## Correlation:  Structure = independence  
## Number of clusters:   25   Maximum cluster size: 141
</pre></div>
<div class="source"><pre class="knitr r">summary(geeInd.int[[1]])
</pre></div>
<div class="output"><pre class="knitr r">## 
## Call:
## geeglm(formula = coverage ~ sampleDepth + group * region, family = gaussian, 
##     data = covdata.used[[i]], id = sample, corstr = corstr)
## 
##  Coefficients:
##                         Estimate Std.err  Wald Pr(>|W|)    
## (Intercept)              -5.6410  3.6463  2.39  0.12185    
## sampleDepth               0.4048  0.1285  9.93  0.00162 ** 
## groupCO                  -0.5828  0.1173 24.67  6.8e-07 ***
## groupETOH                -0.4597  0.1165 15.56  8.0e-05 ***
## regionregionM             0.4156  0.0796 27.27  1.8e-07 ***
## regionregion2             0.9820  0.1098 80.05  < 2e-16 ***
## groupCO:regionregionM    -0.1750  0.0896  3.81  0.05089 .  
## groupETOH:regionregionM  -0.0330  0.0966  0.12  0.73272    
## groupCO:regionregion2    -0.5204  0.1447 12.93  0.00032 ***
## groupETOH:regionregion2  -0.1233  0.1444  0.73  0.39322    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Estimated Scale Parameters:
##             Estimate Std.err
## (Intercept)    0.147  0.0274
## 
## Correlation: Structure = independenceNumber of clusters:   25   Maximum cluster size: 141
</pre></div>
<div class="source"><pre class="knitr r">

## Extract region2 coef estimate, st.error, and Wald stat
myGEE.stat <- function(y) {
	beta <- c(coef(y)["regionregion2"], y$geese$gamma)
	## Assumes that geeglm(std.err="san.se") which is the default value
	vbeta <- sqrt(c(y$geese$vbeta[6, 6], y$geese$vgamma))
	wald <- (beta/vbeta)^2
	pval <- 1 - pchisq(wald, df=1)
	df <- data.frame(coef=c("region2", "gamma"), estimate=beta, stderr=vbeta, wald=wald, pval=pval)
	rownames(df) <- seq_len(nrow(df))
	return(df)
}
geeInd.stat <- lapply(geeInd, myGEE.stat)
save(geeInd.stat, file=file.path(wdir, "geeInd.stat.Rdata"))

## Extract region2 coef estimate, interaction coef estiamtes, st.error, and Wald stat
myGEE.int.stat <- function(y) {
	beta <- c(coef(y)[c("regionregion2", "groupCO:regionregion2", "groupETOH:regionregion2")], y$geese$gamma)
	## Assumes that geeglm(std.err="san.se") which is the default value
	vbeta <- sqrt(c(y$geese$vbeta[6, 6], y$geese$vbeta[9, 9], y$geese$vbeta[10, 10], y$geese$vgamma))
	wald <- (beta/vbeta)^2
	pval <- 1 - pchisq(wald, df=1)
	df <- data.frame(coef=c("region2", "gamma", "groupCO:region2", "groupETOH:region2"), estimate=beta, stderr=vbeta, wald=wald, pval=pval)
	rownames(df) <- seq_len(nrow(df))
	return(df)
}

geeInd.int.stat <- lapply(geeInd.int, myGEE.int.stat)
save(geeInd.int.stat, file=file.path(wdir, "geeInd.int.stat.Rdata"))


## Show an example:
geeInd.stat[[1]]
</pre></div>
<div class="output"><pre class="knitr r">##      coef estimate stderr  wald     pval
## 1 region2    0.755 0.0730 107.1 0.00e+00
## 2   gamma    0.151 0.0277  29.8 4.75e-08
</pre></div>
<div class="source"><pre class="knitr r">geeInd.int.stat[[1]]
</pre></div>
<div class="output"><pre class="knitr r">##                coef estimate stderr   wald     pval
## 1           region2    0.982 0.1098 80.046 0.00e+00
## 2             gamma   -0.520 0.1447 12.931 3.23e-04
## 3   groupCO:region2   -0.123 0.1444  0.729 3.93e-01
## 4 groupETOH:region2    0.147 0.0274 28.928 7.51e-08
</pre></div>
</div></div>











# Reproducibility

Date the report was generated.

<div class="chunk" id="reproducibility1"><div class="rcode"><div class="output"><pre class="knitr r">## [1] "2013-12-20 10:21:45 EST"
</pre></div>
</div></div>


Wallclock time spent generating the report.

<div class="chunk" id="reproducibility2"><div class="rcode"><div class="output"><pre class="knitr r">## Time difference of 2.11 mins
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

