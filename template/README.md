template instructions
=====================


1. Copy the __template__ directory
1. Rename it, say __step1__
1. Rename __template.R__ to __step1.R__, and similarly for __template.Rmd__ and __template.sh__

## template.R

1. Add any libraries that need to be loaded. Please specify where to download it in case that it is not from CRAN.
1. Change where it says __template.Rmd__ to __step1.Rmd__ (or whatever you called it)
1. Change the name of the report to something you like. Default is __reportName.html__

Optional: change the _test values_.

## template.sh

1. Change the `SHORT` name. In particular, replace __template__ by something short that doesn't start with a digit (the cluster doesn't like job names that start with a digit).
1. Change the job memory setting if needed. Default ones might be too high or too low.
1. Change the example usage. It should work by just copy pasting it.

Note that this file is only used for submitting jobs in the cluster. Otherwise it is not needed.

## template.Rmd

1. Add any libraries that need to be loaded. The easy way is just to copy paste the libraries from __step1.R__
1. Change the title (first line)
1. Add R code chunks and edit as you wish where it says `Edit this part` (leave the `Reproducibility` section intact).

## Add it to github

```bash
## Enter the step1 directory
$ cd step1
## Check that there is nothing new on GitHub
$ git pull 
$ git status

## Add the new step
$ git add *
$ git commit -am "Describe briefly what the new step does"

## Send it to github
$ git push
```

## Running on the cluster?

```bash
## Login to the cluster
$ ssh username@enigma2.biostat.jhsph.edu

## Locate your cluster copy of the project
$ cd final756_code

## Pull new changes
$ git pull

## Go to the new step
$ cd step1

## Run it
$ sh step1.sh ... ## as stated in the example usage
```

Once the results are done, add them to GitHub (unless they are big in size)


```bash
## Check that there is nothing new on GitHub
$ git pull
$ git status

## Add the results
$ git add *
$ git commit -am "Describe briefly the results"

## Send it to github
$ git push
```


