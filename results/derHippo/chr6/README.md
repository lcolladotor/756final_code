derHippo-chr6
=============

View pre-processing report [here](http://rawgithub.com/lcolladotor/756final_code/master/results/derHippo/chr6/prep.html)


## Files

### covdata.Rdata

Contains the long format of the data. It is a list with one element per pair, and each element is a data.frame. Index matches the one from __pairs.Rdata__.

### dirs.Rdata

Contains a list of directories from where the raw data was copied from and where the files were dumped. Just for accounting purposes.

### exons.Rdata

Contains the list of exons from the chromosome under study. It is in saved in a __GRanges__ object ([GenomicRanges](http://www.bioconductor.org/packages/release/bioc/html/GenomicRanges.html) package).

### ov.Rdata

Contains two objects: __ov__ and __ov.mat__. They both represent the information of matching the individual regions to the exons. __ov.mat__ is a regular matrix.

### pairs.Rdata

Contains a data.frame with the pair start, end, width and cluster information.

### regions.Rdata

A __GRanges__ object with the regions. Has a lot of other data in it too.

### regsub.Rdata

A __GRanges__ object which is a subset from __regions.Rdata__. It contains the individual regions that were used to form __pairs.Rdata__.
