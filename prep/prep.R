## Prepare files from a given data set, a given run, and a given chromosome

## Load libraries

## Available from http://cran.r-project.org/web/packages/getopt/index.html
# install.packages("getopt")
library("getopt")

## Available from http://cran.at.r-project.org/web/packages/knitrBootstrap/index.html
# install.packages("knitrBootstrap")
library("knitrBootstrap")

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

## Specify parameters
spec <- matrix(c(
	'project', 'p', 1, "character", "name of project, for example 'derSnyder'",
	'dirResult', 'd', 1, "character", "path to the results directory, for example '/home/bst/student/lcollado/756final_code/results'",
	'chrnum', 'c', 1, "character", "Chromosome under analysis. Use X instead of chrX.",
	'run', 'r', 1, "character", "Name of the run, for example run1-v0.0.42",
	'verbose' , 'v', 2, "logical", "Print status updates",
	'help' , 'h', 0, "logical", "Display help"
), byrow=TRUE, ncol=5)
opt <- getopt(spec)

## Testing the script
test <- FALSE
if(test) {
	## Speficy it using an interactive R session and testing
	test <- TRUE
}

## Test values
if(test){
	opt <- NULL
	opt$project <- "derHippo"
	opt$dirResult <- "/home/bst/student/lcollado/756final_code/results"
	opt$chrnum <- "22"
	opt$run <- "run1-v0.0.42"
	opt$verbose <- NULL
}

## if help was asked for print a friendly message
## and exit with a non-zero error code
if (!is.null(opt$help)) {
	cat(getopt(spec, usage=TRUE))
	q(status=1)
}

## Default value for verbose = TRUE
if (is.null(opt$verbose)) opt$verbose <- TRUE
	
## Save start time for getting the total processing time
startTime <- Sys.time()
	
## Create prep report
prepdir <- file.path(opt$dirResult, opt$project, paste0("chr", opt$chrnum))
dir.create(prepdir, recursive=TRUE)

codedir <- getwd()
setwd(prepdir)
knit_bootstrap(file.path(codedir, "prep.Rmd"), output=file.path(prepdir, "prep.html"), code_style='Brown Paper', chooser=c('boot', 'code'), show_code=TRUE)


## Done
if(opt$verbose) {
	print(proc.time())
	print(sessionInfo(), locale=FALSE)
}
