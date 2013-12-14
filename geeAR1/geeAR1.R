## --- Basic description of what this step does

## Example usage:
# Rscript geeAR1.R  -p 'derHippo' -d '/Users/lcollado/enigma2/756final_code/results' -c '22' -r 'geeAR1' -v TRUE

## Load libraries

## Available from http://cran.r-project.org/web/packages/getopt/index.html
# install.packages("getopt")
library("getopt")

## Available from http://cran.at.r-project.org/web/packages/knitrBootstrap/index.html
# install.packages("knitrBootstrap")
library("knitrBootstrap")

# --- Specify any other libraries you need
## gee
# install.packages("geepack")
library("geepack")


## Specify parameters
spec <- matrix(c(
	'project', 'p', 1, "character", "name of project, for example 'derSnyder'",
	'dirResult', 'd', 1, "character", "path to the main results directory, for example '/home/bst/student/lcollado/756final_code/results'",
	'chrnum', 'c', 1, "character", "Chromosome under analysis. Use X instead of chrX.",
	'results', 'r', 1, "character", "Name of the directory where you want to save the results, for example 'step1'",
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
	opt$results <- "geeAR1"
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
	
## Create report dir
reportdir <- file.path(opt$dirResult, opt$project, paste0("chr", opt$chrnum), opt$results)
dir.create(reportdir, recursive=TRUE)

codedir <- getwd()
setwd(reportdir)
knit_bootstrap(file.path(codedir, "geeAR1.Rmd"), output=file.path(reportdir, "geeAR1.html"), code_style='Brown Paper', chooser=c('boot', 'code'), show_code=TRUE) # show_code=FALSE is specially useful for results with interpretation

## Done
if(opt$verbose) {
	print(proc.time())
	print(sessionInfo(), locale=FALSE)
}
