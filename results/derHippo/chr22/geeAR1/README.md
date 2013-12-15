GEE with AR-1 w.cor
========================

# Explanation of files

## covdata.used.Rdata

A subset of [covdata.Rdata](../covdata.Rdata) with the pairs that had __region1__ and __region2__ width greater than 1 base and __regionM__ width less than 250 bases. This was done to filter out short pairs where there are no "repeated measurements" on regions 1 and 2; while also filtering out very long regions due to a long __regionM__ that take a very long time to fit. The names of __covdata.used__ are the pair ids from the original set.

## pairs.used.Rdata

The corresponding subset of [pairs.Rdata](../pairs.Rdata). The rownames of __pairs.used__ match the names of __covdata.used__.

## geeAR1.Rdata

A list with the result from fitting a GEE with AR-1 working correlation. There is one element for each pair that passed the filtering selection.

## geeAR1.stat.Rdata

A list with the __region2__ and __alpha__ coefficients extracted from the output. Each element is a data frame and contains information on the estimate, it's robust standard error and Wald p-value.


