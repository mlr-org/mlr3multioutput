# #' @title Create a multioutput learner pipeline
# #' @name mlr_graphs_multiout
# #' @description
# #' Creates a [`Graph`] that reduces a [`TaskMultiOutput`] into several [`Tasks`][mlr3::Task] for a list of supplied
# #' `Graph`s (or `Learner`s).
# #' This is done as follows:
# #' * Split the `Task` into sub-tasks using `PipeOpSplitMultiOut`.
# #' * Applies the correct `Learner` for each
# #' @param graph [`PipeOp`] | [`Graph`] \cr
# #'   A [`PipeOpLearner`] or [`Graph`] to create a robustifying pipeline for.
# #'   Outputs from the replicated `graph`s are connected with the `averager`.
# #' @param iterations `integer(1)` \cr
# #'   Number of bagging iterations. Defaults to 10.
# #' @param frac `numeric(1)` \cr
# #'   Percentage of rows to keep during subsampling. See [`PipeOpSubsample`] for
# #'   more information. Defaults to 0.7.
# #' @param averager [`PipeOp`] | [`Graph`] \cr
# #'   A [`PipeOp`] or [`Graph`] that averages the predictions from the
# #'   replicated and subsampled graph's.
# #'   In the simplest case, `po("classifavg")` and `po("regravg")` can be used
# #'   in order to perform simple averaging of classification and regression
# #'   predictions respectively.`
# #'   If `NULL` (default), no averager is added to the end of the graph.
# #' @return [`Graph`]
# #' @export
# #' @examples
# #' library(mlr3)
# #' lrn_po = po("learner", lrn("regr.rpart"))
# #' task = mlr_tasks$get("boston_housing")
# #' gr = pipeline_bagging(lrn_po, 3, averager = po("regravg"))
# #' resample(task, GraphLearner$new(gr), rsmp("holdout"))
# pipeline_multilearner = function(graph, iterations = 10, frac = 0.7, averager = NULL) {
#   assert_count(iterations)
#   assert_number(frac, lower = 0, upper = 1)
#   graph = as_graph(graph)
#   if (!is.null(averager)) averager = as_graph(averager)

#   subs = po("subsample", param_vals = list(frac = frac)) %>>% graph
#   subs_repls = pipeline_greplicate(subs, iterations)

#   subs_repls %>>% averager
# }

# mlr_graphs$add("bagging", pipeline_bagging)

