#' @title Conditional Random Forest Multioutput Learner
#'
#' @name mlr_learners_multioutput.cforest
#' @include LearnerMultioutput.R
#'
#' @description
#' A [LearnerMultioutput] implementation of the random forest and bagging ensemble
#' algorithms utilizing conditional inference trees as base learners.
#' Supports multilabel classification.
#'
#' @templateVar id Multioutputput.cforest
#' @template section_dictionary_learner
#'
#' @export
LearnerMultioutputCForest = R6Class("LearnerMultioutputCForest",
  inherit = LearnerMultioutput,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {

      ps = ParamSet$new(list(
        # missing: weights (see bottom), subset, strata
        # (see FIXME: below), na.action, ytrafo
        ParamInt$new("ntree", default = 500L, lower = 1L, tags = "train"),
        # replace and fraction go in perturb (named list)
        ParamLgl$new("replace", default = FALSE, tags = "train"),
        ParamDbl$new("fraction", default = 0.632, lower = 0, upper = 1,
          tags = "train"),
        ParamInt$new("mtry", lower = 0L, special_vals = list(Inf),
          tags = "train"), # default actually "ceiling(sqrt(nvar))"
        ParamUty$new("applyfun", tags = c("train", "importance")),
        ParamInt$new("cores", default = NULL, special_vals = list(NULL),
          tags = c("train", "importance")),
        ParamLgl$new("trace", default = FALSE, tags = "train"),
        ParamUty$new("offset", tags = "train"),
        ParamUty$new("cluster", tags = "train"),
        ParamUty$new("scores", tags = "train"),

        # all in ctree_control(); missing: mtry, applyfun, cores
        # (see above, passed directly)
        ParamFct$new("teststat", default = "quadratic",
          levels = c("quadratic", "maximum"), tags = "train"),
        ParamFct$new("splitstat", default = "quadratic",
          levels = c("quadratic", "maximum"), tags = "train"),
        ParamLgl$new("splittest", default = FALSE, tags = "train"),
        ParamFct$new("testtype", default = "Univariate",
          levels = c("Bonferroni", "MonteCarlo", "Univariate", "Teststatistic"),
          tags = "train"),
        ParamUty$new("nmax", tags = "train"),

        # pargs arguments as a list for GenzBretz() within ctree_control:
        # maxpts, abseps, releps
        ParamUty$new("pargs", tags = "train"),

        ParamDbl$new("alpha", default = 0.05, lower = 0, upper = 1,
          tags = "train"),
        ParamDbl$new("mincriterion", default = 0, lower = 0, upper = 1,
          tags = "train"),
        ParamDbl$new("logmincriterion", default = 0, tags = "train"),
        ParamInt$new("minsplit", lower = 1L, default = 20L, tags = "train"),
        ParamInt$new("minbucket", lower = 1L, default = 7L, tags = "train"),
        ParamDbl$new("minprob", default = 0.01, lower = 0, upper = 1,
          tags = "train"),
        ParamLgl$new("stump", default = FALSE, tags = "train"),
        ParamLgl$new("lookahead", default = FALSE, tags = "train"),
        ParamLgl$new("MIA", default = FALSE, tags = "train"),
        ParamInt$new("nresample", default = 9999L, lower = 1L, tags = "train"),
        ParamDbl$new("tol", default = sqrt(.Machine$double.eps), lower = 0,
          tags = "train"),
        ParamInt$new("maxsurrogate", default = 0L, lower = 0L, tags = "train"),
        ParamLgl$new("numsurrogate", default = FALSE, tags = "train"),
        ParamInt$new("maxdepth", default = Inf, lower = 0L,
          special_vals = list(Inf), tags = "train"),
        ParamLgl$new("multiway", default = FALSE, tags = "train"),
        ParamInt$new("splittry", default = 2L, lower = 0L, tags = "train"),
        ParamLgl$new("intersplit", default = FALSE, tags = "train"),
        ParamLgl$new("majority", default = FALSE, tags = "train"),
        ParamLgl$new("caseweights", default = TRUE, tags = "train"),
        ParamLgl$new("saveinfo", default = FALSE, tags = "train"),
        ParamLgl$new("update", default = FALSE, tags = "train"),
        ParamFct$new("splitflavour", default = "ctree",
          levels = c("ctree", "exhaustive"), tags = "train"),

        # predict; missing FUN and simplify (not needed here)
        ParamLgl$new("OOB", default = FALSE, tags = c("predict", "importance")),
        ParamLgl$new("simplify", default = TRUE, tags = "predict"),
        ParamLgl$new("scale", default = TRUE, tags = "predict"),

        # importance; OOB see predict, applyfun, cores see train
        ParamInt$new("nperm", default = 1L, lower = 0L,
          tags = c("train", "importance")),
        ParamFct$new("risk", default = "loglik",
          levels = c("loglik", "misclassification"),
          tags = c("train", "importance")),
        ParamLgl$new("conditional", default = FALSE,
          tags = c("train", "importance")),
        ParamDbl$new("threshold", default = 0.2,
          tags = c("train", "importance"))
      ))

      ps$add_dep("nresample", on = "testtype",
        cond = CondEqual$new("MonteCarlo"))
      ps$add_dep("nperm", on = "conditional", cond = CondEqual$new(TRUE))
      ps$add_dep("threshold", on = "conditional", cond = CondEqual$new(TRUE))

      # set the cforest specific ctree_control parameters
      ps$values$teststat = "quadratic"
      ps$values$testtype = "Univariate"
      ps$values$mincriterion = 0
      ps$values$saveinfo = FALSE

      super$initialize(
        id = "multiout.cforest",
        feature_types = mlr_reflections$task_feature_types,
        predict_types = c("response", "prob"),
        param_set = ParamSet$new(),
        properties = c("missings", "twoclass", "multiclass", "multioutput", "multilabel"),
        packages = c("partykit", "sandwich", "coin")
      )
    }
  ),

  private = list(
    .train = function(task) {
      browser()
      pars = self$param_set$get_values(tags = "train")
      pars_control = pars[which(names(pars) %in%
        setdiff(methods::formalArgs(partykit::ctree_control),
          c("mtry", "applyfun", "cores")
        ))] # see ctree_control
      pars = pars[names(pars) %nin%
        c("replace", "fraction", names(pars_control))]
      control = mlr3misc::invoke(partykit::ctree_control, .args = pars_control)
      # perturb parameters need special handling; FIXME: easier way?
      perturb = list(replace = FALSE, fraction = 0.632)
      if (!is.null(self$param_set$values$replace)) {
        perturb$replace = self$param_set$values$replace
      }
      if (!is.null(self$param_set$values$fraction)) {
        perturb$fraction = self$param_set$values$fraction
      }

      mlr3misc::invoke(partykit::cforest,
        formula = as.formula(sprintf("%s ~ %s", paste0(task$target_names, collapse = " + "), paste0(task$feature_names, collapse = " + "))), #FIXME: use task$formula() after multi target implementation in mlr3misc
        data = task$data(),
        weights = task$weights$weight, # weights are handled here
        # FIXME: strata handling
        control = control,
        perturb = perturb,
        .args = pars
      )
    },

    .predict = function(task) {
      browser()
      pars = self$param_set$get_values(tags = "predict")
      newdata = task$data(cols = task$feature_names)
      preds = mlr3misc::invoke(predict, object = self$model, newdata = newdata,
        type = self$predict_type, .args = pars)

      if (self$predict_type == "response") {
        p = map(task$target_names, function(t) {
          list(
            row_ids = task$row_ids,
            truth = task$truth[t],
            response = preds[t]
          )
        })
        names(p) = task$target_names
        as_prediction(as.PredictionDataMultioutput(p, task$task_types))
      } else {
        p = map(task$target_names, function(t) {
          list(
            row_ids = task$row_ids,
            truth = task$truth[t],
            prob = preds[t] # FIXME
          )
        })
        names(p) = task$target_names
        as_prediction(as.PredictionDataMultioutput(p, task$task_types))
      }
    }
  )
)

