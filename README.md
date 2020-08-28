# mlr3multioutput

<!-- badges: start -->
[![tic](https://github.com/mlr-org/mlr3multioutput/workflows/tic/badge.svg?branch=master)](https://github.com/mlr-org/mlr3multioutput/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/mlr3multioutput)](https://CRAN.R-project.org/package=mlr3multioutput)
[![codecov](https://codecov.io/gh/mlr-org/mlr3multioutput/branch/master/graph/badge.svg)](https://codecov.io/gh/mlr-org/mlr3multioutput)
[![StackOverflow](https://img.shields.io/badge/stackoverflow-mlr3-orange.svg)](https://stackoverflow.com/questions/tagged/mlr3)
<!-- badges: end -->

Storing and working with multi-output Tasks in `mlr3`.
Multi-output Tasks are tasks with multiple targets of possibly different `task_types` that share the same features (and therefore observations).

## Installation

Install the development version from GitHub:

```r
remotes::install_github("mlr-org/mlr3multioutput")
```

## Example

Instantiate an example `Task` "linnerud", which has 3 regression targets.

```r
library(mlr3)
library(mlr3multioutput)
t = tsk("linnerud")
t
```

    ## <TaskMultiOutput>linnerud> (20 x 6)
    ## * Target: Pulls, Squats, Jumps
    ## * Properties: -
    ## * Features (4):
    ##   - int (3): Pulse, Waist, Weight

And afterwards we can **train** and **predict**:

```r
learner <- lrn("multiout.featureless")
learner$train(t)
learner$predict(t)
```

## Design

The current goal of the package is to implement multi-output Tasks for
`mlr`. Such Tasks can be either modeled using a separate learner for each class (via `mlr3pipelines`), using `Classification` and `Regression` chains (again, via ` mlr3pipelines`) or alternatively using `Learners` that can directly handle and work with multiple outputs.
Several design decisions are not made yet, so input is highly appreciated.

### Currently implemented Learners and Strategies

`mlr3multioutput` implements Learners as well as reduction strategies for multioutput Tasks.

| Learner | Details | Reference |
|---|---|---|
| [multiout.featureless]()   | A featureless learner based on ("regr." or "classif").featureless | --  |


| PipeOp |Details | Reference |
|---|---|---|
| [po("multioutsplit")]()   | Split up a MultiOutput Task in several `classif`, `regr` or `...` tasks | --  |
| [po("multioutunite")]()   | Unite `classif`, `regr` or `...` predictions into a `PredictionMultiOutput` | --  |
| [po("multilrn")]()        | Apply a list of per-task-type learners to a `TaskMultiOutput` | --  |


## Long-term Goals

- Implement sub-class "Multilabel". This should cover almost all aspects of multi-label modelling.
- Implement classifier and regressor chaining strategies via `mlr3pipelines`.

## Resources

* There is a [book](https://mlr3book.mlr-org.com/) on `mlr3` and its ecosystem, but it is still unfinished.
* [Reference Manual](https://mlr3.mlr-org.com/reference/)
* [Extension packages](https://github.com/mlr-org/mlr3/wiki/Extension-Packages).
* [useR2019 talks](https://github.com/mlr-org/mlr-outreach/tree/master/2019_useR)
