# #' @include MeasureMultiOutput.R
# MeasureMultiOutputInternal = R6Class("MeasureClustInternal",
#   inherit = MeasureMultiOutput,
#   public = list(
#     crit = NULL,
#     initialize = function(name) {
#       info = measures[[name]]
#       super$initialize(
#         id = paste0("multiout.", name),
#         range = c(info$lower, info$upper),
#         minimize = info$minimize,
#         predict_type = info$predict_type,
#         packages = "clusterCrit",
#         properties = "requires_task",
#         man = paste0("mlr3cluster::mlr_measures_clust.", name)
#       )
#       self$crit = info$crit
#     }
#   ),
#   private = list(
#     .score = function(prediction, task, ...) {
#       X = as.matrix(task$data(rows = prediction$row_ids))
#       if (!is.double(X)) { # clusterCrit does not convert lgls/ints
#         storage.mode(X) = "double"
#       }
#       intCriteria(X, prediction$partition, self$crit)[[1L]]
#     }
#   )
# )