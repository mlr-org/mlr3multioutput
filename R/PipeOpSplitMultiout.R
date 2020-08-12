#' @title PipeOpSplitMultiout
#'
#' @usage NULL
#' @name mlr_pipeops_splitmultiout
#' @format [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Splits a [multioutput Task][TaskMultiOutput] into several [Tasks][mlr3::Task].
#'
#' For each target_type in the original [multioutput Task][TaskMultiOutput], a new [Task][mlr3::Task]
#' containing the respective target and task_type is constructed.
#'
#' This [`PipeOp`] creates a [`Multiplicity`], which means that subsequent [`PipeOp`]s are executed
#' multiple times.
#'
#' Note that [`Multiplicity`] is currently an experimental feature and the implementation or UI
#' may change.
#'
#' @section Construction:
#' ```
#' PipeOpSplitMultiout$new(id = "splitmultiout", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of the resulting object, default `"ovrsplit"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' [`PipeOpSplitMultiout`] has one input channel named `"input"` taking a [`TaskMultiOutput`][TaskMultiOutput]
#' both during training and prediction.
#'
#' [`PipeOpSplitMultiout`] has one output channel named `"output"` returning a [`Multiplicity`] of
#' [`Tasks`][mlr3::Task]s both during training and prediction.
#'
#' @section State:
#' The `$state` contains the original task_types of the [`TaskMultiOutput`] supplied
#' during training.
#'
#' @section Parameters:
#' [`PipeOpSplitMultiout`] has no parameters.
#'
#' @section Internals:
#'
#' The names of the element of the output [`Multiplicity`] are given by the levels of the target.
#'

#' Should be used in combination with [`PipeOpPredictionMultiOutUnite`].
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#' @family PipeOps
#' @family Multiplicity PipeOps
#' @family Experimental Features
#' @export
#' @examples
#' library(mlr3)
#' task = tsk("linnerud")
#' po = po("spllitmultiout")
#' po$train(list(task))
#' po$predict(list(task))
PipeOpSplitMultiout = R6Class("PipeOpSplitMultiout",
  inherit = mlr3pipelines::PipeOp,
  public = list(
    initialize = function(id = "targetsplit", param_vals = list()) {
      super$initialize(id, param_vals = param_vals,
        input = data.table(name = "input", train = "TaskMultiOutput", predict = "TaskMultiOutput"),
        output = data.table(name = "output", train = "[Task]", predict = "[Task]"),
        tags = "multiplicity"
      )
    }
  ),
  private = list(
    .train = function(inputs) {
      self$state = list(task_types = inputs[[1]]$task_types)
      list(as.Multiplicity(convert_to_basic_tasks(inputs[[1]])))
    },
    .predict = function(inputs) {
      list(as.Multiplicity(convert_to_basic_tasks(inputs[[1]])))
    }
  )
)

mlr3pipelines::mlr_pipeops$add("splitmultiout", PipeOpSplitMultiout)


#' @title PipeOpPredictionMultiOutUnite
#'
#' @usage NULL
#' @name mlr_pipeops_multioutunite
#' @format [`R6Class`] inheriting from [`PipeOpEnsemble`]/[`PipeOp`].
#'
#' @description
#' Unite a set of "classif", "regr"' [`Predictions`][mlr3::Prediction] into a [`PredictionMultiOutput`].
#'
#' This [`PipeOp`] uses a [`Multiplicity`] input, which is created by [`PipeOpMultiplicityImply`] or other
#' [`PipeOp`]s that implicate a [`Multiplicity`].
#'
#' Note that [`Multiplicity`] is currently an experimental feature and the implementation or UI
#' may change.
#'
#' @section Construction:
#' ```
#' PipeOpPredictionMultiOutUnite$new(id = "multioutunite", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of the resulting object, default `"multioutunite"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpEnsemble`]. Instead of a
#' [`PipeOpEnsemble`]'s `collect` parameter is initialized
#' with `TRUE` to allow for collecting a [`Multiplicity`] input.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' The parameters are the parameters inherited from the [`PipeOpEnsemble`].
#'
#' @section Internals:
#' Inherits from [`PipeOpEnsemble`] by implementing the `private$.predict()` method.
#'
#' Should be used in combination with [`PipeOpSplitMultiOut`].
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpEnsemble`]/[`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpEnsemble`]/[`PipeOp`].
#' @family PipeOps
#' @family Ensembles
#' @family Multiplicity PipeOps
#' @family Experimental Features
#' @export
PipeOpPredictionMultiOutUnite = R6Class("PipeOpPredictionMultiOutUnite",
  inherit = mlr3pipelines::PipeOpEnsemble,
  public = list(
    initialize = function(id = "multioutunite", param_vals = list()) {
      super$initialize(0, TRUE, id, param_vals = param_vals, prediction_type = "Prediction", tags = "multiplicity")
    }
  ),
  private = list(
    .predict = function(inputs) {
      if (private$.collect) {
        inputs = unclass(inputs[[1]])
      }
      list(PredictionMultiOutput$new(row_ids = inputs[[1]]$row_ids, predictions = inputs))
    }
  )
)

mlr3pipelines::mlr_pipeops$add("multioutunite", PipeOpPredictionMultiOutUnite)