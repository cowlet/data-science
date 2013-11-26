# Instructions for usage

[Explanation of some of these scripts can be found on my weblog.](http://cowlet.org) Below is the quick guide to getting them running.


## `basic_feature_extraction.R`

1.  Download the [open bearing dataset](http://ti.arc.nasa.gov/c/3/).

2.  Move the `bearing_IMS` directory to the same level as the `bearing_snippets` directory OR
    
    Modify the first line of the script to point basedir to the `bearing_IMS/1st_test` directory.

3.  Run `basic_feature_extraction.R`!


## `basic_feature_graphing.R`

1.  For the first time through, run `basic_feature_extraction.R` to generate the features. Thereafter, the features are written to file and you can go straight to step 2.

2.  Run `basic_feature_graphing.R`!


## `more_features.R`

1.  Perform steps 1 and 2 of `basic_feature_extraction.R`.

2.  Run `more_features.R`!


## `feature_correlation.R`

1.  Run `more_features.R`, so the features are stored in files.

2.  Run `feature_correlation.R`, to see features with high correlation.

## `optimise.rb`

1.  Run `feature_correlation.R` to output sets of features with high correlation.

2.  Run `optimise.rb` to select the minimal set of uncorrelated features.


## `feature_information.R`

1.  Run `more_features.R`, so the features are stored in files.

2.  If desired, modify line 25 of `feature_information.R` to include only the features you are interested in (e.g. after running `optimise.rb` and finding a different minimal set).

3.  Run `feature_information.R` to generate an interesting graph!


