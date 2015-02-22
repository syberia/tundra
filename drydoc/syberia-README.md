## Tundra

**[Tundra](http://github.com/robertzk/tundra)** - Training a model and having the correct
settings during prediction can involve a lot of separate pieces of configuration.
To solve this problem, a `tundraContainer` is an object that has two methods:
`train` and `predict`, which take a data set, and run a "model" on that data
set (for example, logistic regression or GBM). One can also think of a tundraContainer as 
a wrapper around both the native model object and the pre-processing methods used to generate the model

However, this is only half of the story. When making predictions in a production
environment, we have already pointed out that the data coming into the algorithm
must look identical to the type of data the model was trained on. Therefore,
we hereby define a *model* as being the union of both the actual mathematical
algorithms that end up producing numerical outcomes **and** the data preparation
procedure itself (which is highly customized to one specific data set).

This sacrifices the generality of the classifier, since it must be fed very
specific kind of data (namely, the kind of raw data the model was trained on
before any preprocessing steps). However, it enables a more powerful procedure:
given any raw unadulterated production data (whether historical / training, or 
live / production), we can instantly ask for its predicted values by passing
the data to the `tundraContainer`'s `predict` method. There is no need to
preprocess the data (this is done by the `tundraContainer`), or to give model
prediction parameters (e.g., whether we're requesting probability or log odds).
These have been fixed when training the classifier, as its sole purpose is to
take raw data and produce a final score in a production environment without any
further input.

For more information on how to wrap your existing model scripts into `tundraContainers`,
check out the [interactive tundra tutorial](http://en.wikipedia.org/wiki/Vaporware).
(**TODO**: Make this.)
