#' @title PipeOpSplitMultiout
#'
#' @usage NULL
#' @name mlr_pipeops_multioutsplit
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
#' PipeOpSplitMultiout$new(id = "multioutsplit", param_vals = list())
#' ```
#' * `id` :: `character(1)`\cr
#'   Identifier of the resulting object, default `"ovrsplit"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise
#'   be set during construction. Default `list()`.
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
#' po = po("multioutsplit")
#' po$train(list(task))
#' po$predict(list(task))
PipeOpSplitMultiout = R6Class("PipeOpSplitMultiout",
  inherit = mlr3pipelines::PipeOp,
  public = list(
    #' @description
    #' Initialize a new R6 class.
    #'
    #' @param id `character(1)`\cr
    #'   Identifier of the resulting  object, internally defaulting "targetsplit".
    #' @param param_vals named `list`\cr
    #'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set
    #' during construction. Default `list()`.
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

mlr3pipelines::mlr_pipeops$add("multioutsplit", PipeOpSplitMultiout)


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
    #' @description
    #' Initialize a new R6 class.
    #'
    #' @param id `character(1)`\cr
    #'   Identifier of the resulting  object, defaults to "multioutunite".
    #' @param param_vals named `list`\cr
    #'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
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

#' @title PipeOpMultiLearner
#'
#' @usage NULL
#' @name mlr_pipeops_multilrn
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' Wraps a `list` of [`mlr3::Learner`]s into a [`PipeOp`].
#'
#' Inherits the `$param_set`s (and therefore `$param_set$values`) from all [`Learner`][mlr3::Learner]s it is constructed from.
#'
#' @section Input and Output Channels:
#' [`PipeOpMultiLearner`] has one input channel named `"input"`, taking a [`Task`][mlr3::Task] specific to the [`Learner`][mlr3::Learner]
#' type given to `learner` during construction; both during training and prediction.
#'
#' [`PipeOpMultiLearner`] has one output channel named `"output"`, producing `NULL` during training and a [`Multiplicity`][mlr3pipelines::Multiplicity] of [`Predictions`][mlr3::Prediction]
#' during prediction; this subclass is specific to the [`Learner`][mlr3::Learner] type given to `learner` during construction.
#'
#' The output during prediction is a [`Multiplicity`][mlr3pipelines::Multiplicity] of [`Predictions`][mlr3::Prediction] on the input data, produced by the [`Learners`][mlr3::Learner]
#' trained on the training input data.
#'
#' @section State:
#' The `$state` is set to the `$state` slot of the [`Learner`][mlr3::Learner] object. It is a named `list` with members:
#' of states for each separate `Task` provided via the incoming `Multiplicity`.\
#' Each element contains the following slots:
#' * `model` :: `any`\cr
#'   Model created by the [`Learner`][mlr3::Learner]'s `$.train()` function.
#' * `train_log` :: [`data.table`] with columns `class` (`character`), `msg` (`character`)\cr
#'   Errors logged during training.
#' * `train_time` :: `numeric(1)`\cr
#'   Training time, in seconds.
#' * `predict_log` :: `NULL` | [`data.table`] with columns `class` (`character`), `msg` (`character`)\cr
#'   Errors logged during prediction.
#' * `predict_time` :: `NULL` | `numeric(1)`
#'   Prediction time, in seconds.
#'
#' @section Parameters:
#' The parameters are exactly the parameters of the [`Learners`][mlr3::Learner] wrapped by this object.
#'
#' @section Internals:
#' The `$state` is currently not updated by prediction, so the `$state$predict_log` and `$state$predict_time` will always be `NULL`.
#'
#' @section Methods:
#' Methods inherited from [`PipeOp`].
#'
#' @family PipeOps
#' @family Multiplicity PipeOps
#' @family Experimental Features
#' @export
#' @examples
#' library("mlr3")
#' library("mlr3pipelines")
#'
#' task = tsk("linnerud")
#' learners = list(
#'   classif = lrn("classif.rpart", cp = 0.1),
#'   regr = lrn("regr.rpart")
#' )
#' lrn_po = mlr_pipeops$get("multilrn", learners)
#'
#' # Train the graph
#' gr = po("multioutsplit") %>>% lrn_po
#' gr$train(task)
#' gr$predict(task)
PipeOpMultiLearner = R6Class("PipeOpMultiLearner",
  inherit = mlr3pipelines::PipeOp,
  public = list(
    #' @description
    #' Initialize a new R6 class.
    #'
    #' @param learners `list()`\cr
    #'   List of [`Learner`][mlr3::Learner] | `character(1)`, either:
    #'   * One learner for each `task_type`
    #'   * One learner for each `target`, requires list to be named with the Task's `target_names`.
    #' @param id `character(1)`\cr
    #'   Identifier of the resulting  object, internally defaulting to the combined `ids` of the
    #'   [`Learner`][mlr3::Learner] being wrapped.
    #' @param param_vals named `list`\cr
    #'   List of hyperparameter settings, overwriting the hyperparameter settings that would
    #'   otherwise be set during construction. Default `list()`.
    initialize = function(learners, id = NULL, param_vals = list()) {
      private$.learners = map(learners, as_learner, clone = TRUE)
      id = id %??% paste0(map_chr(private$.learners, "id"), collapse = "_")
      task_type = mlr_reflections$task_types[get("type") == private$.learner$task_type][order(get("package"))][1L]$task      # nolint
      out_type = mlr_reflections$task_types[get("type") == private$.learner$task_type][order(get("package"))][1L]$prediction # nolint
      ps = paradox::ParamSetCollection$new(imap(learners, function(x, i) {x$param_set$set_id = i; x$param_set}))
      super$initialize(id, param_vals = param_vals,
        param_set = ps,
        input = data.table(name = "input", train = "[Task]", predict = "[Task]"),
        output = data.table(name = "output", train = "NULL", predict = "Prediction"),
        tags = "learner"
      )
    }
  ),
  active = list(
    #' @field id (`character(1)`)\cr
    #' Access or set the `id`.
    id = function(val) {
      if (!missing(val)) {
        private$.id = val
        private$.param_set$set_id = val
      }
      private$.id
    },
    #' @field learners (`list()`)\cr
    #' Access the stored learners.
    learners = function(val) {
      if (!missing(val)) {
        if (!identical(val, private$.learners)) {
          stop("$learner is read-only.")
        }
      }
      private$.learners
    },
    #' @field learner_models (`list()`)\cr
    #' Access the trained learners.
    learner_models = function(val) {
      if (!missing(val)) {
        if (!identical(val, private$.learners)) {
          stop("$learner_models is read-only.")
        }
      }
      if (is.null(self$state) || is_noop(self$state)) {
        private$.learners
      } else {
        multiplicity_recurse(self$state, clone_with_state, learner = private$.learners)
      }
    },
    #' @field predict_types (`list()`)\cr
    #' Access the predict_types.
    predict_types = function(val) {
      if (!missing(val)) {
        assert_subset(val, names(mlr_reflections$learner_predict_types[[private$.learner$task_type]]))
        private$.learner$predict_type = val
      }
      private$.learner$predict_type
    }
  ),
  private = list(
    .learners = NULL,

    .train = function(inputs) {
      on.exit(private$.reset_learner_states())
      self$state = list()
      map(unclass(inputs[[1]]), private$.train_per_task)
      list(NULL)
    },

    .predict = function(inputs) {
      on.exit(private$.reset_learner_states())
      prds = map(unclass(inputs[[1]]), function(x) {
        tn = x$target_names
        private$.learners[[x$task_type]]$state = self$state[[tn]]
        private$.learners[[x$task_type]]$predict(x)
      })
      list(PredictionMultiOutput$new(row_ids = prds[[1]]$row_ids, predictions = prds))
    },

    .train_per_task = function(task) {
      self$state[[task$target_names]] = private$.learners[[task$task_type]]$clone()$train(task)$state
    },

    .reset_learner_states = function() {
      map(private$.learners, function(x) x$state = NULL)
      return(NULL)
    }

  )
)

mlr3pipelines::mlr_pipeops$add("multilrn", PipeOpMultiLearner)
