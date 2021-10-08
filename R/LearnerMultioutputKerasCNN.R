#' @title Keras Convolutionary Neural Network for Classification
#'
#' @usage NULL
#' @aliases mlr_learners_multioutput.kerascnn
#' @format [R6::R6Class()] inheriting from [mlr3multioutput::LearnerMultioutputKeras].
#'
#' @section Construction:
#' ```
#' LearnerMultioutputKerasCNN$new()
#' mlr3::mlr_learners$get("multioutput.kerascnn")
#' mlr3::lrn("multioutput.kerascnn")
#' ```
#' @template learner_methods
#' @template seealso_learner
#' @templateVar learner_name multioutput.kerascnn
#' @template example
#' @export
LearnerMultioutputKerasCNN = R6::R6Class("LearnerMultioutputKerasCNN",
  inherit = LearnerMultioutputKeras,
  public = list(
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("hidden_dims", lower = 1, upper = Inf, default = 128, tags = "train"),
        # ParamDbl$new("regularization", lower = 0, upper = 1, default = 0.01, tags = "train"),
        ParamUty$new("regularizer", default = "regularizer_l1_l2()", tags = "train"),
        ParamLgl$new("use_dropout", default = TRUE, tags = "train"),
        ParamDbl$new("dropout", lower = 0, upper = 1, default = 0.2, tags = "train"),
        ParamDbl$new("input_dropout", lower = 0, upper = 1, tags = "train"),
        ParamDbl$new("learning_rate", lower = 0, upper = 1, default = 3*10^-4, tags = "train"),
        ParamUty$new("alphabet", default = "abcdefghijklmnopqrstuvwxyzäöüßABCDEFGHIJKLMNOPQRSTUVWXYZÜÖÄ -_"),
        ParamLgl$new("use_embedding", default = TRUE, tags = c("train", "predict")),
        ParamLgl$new("embed_char_level", default = TRUE, tags = c("train", "predict")),
        ParamDbl$new("embed_dropout", default = 0, lower = 0, upper = 1, tags = "train"),
        ParamDbl$new("embed_size", default = NULL, lower = 1, upper = Inf, tags = "train", special_vals = list(NULL)),
        ParamInt$new("embed_max_char_len", default = 20, lower = 1, upper = Inf, tags = c("train", "predict")),
        ParamInt$new("embed_output_dim", lower = 1, upper = Inf, default = 8, tags = "train"),
        ParamUty$new("layer_units", default = c(32, 32, 32), tags = "train"),
        ParamInt$new("n_conv_layers", lower = 1, upper = Inf, default = 3, tags = "train"),
        ParamInt$new("kernel_size", lower = 1, upper = Inf, default = 32, tags = "train"),
        ParamUty$new("initializer", default = "initializer_glorot_uniform()", tags = "train"),
        ParamInt$new("filters", lower = 1, upper = Inf, default = 128, tags = "train"),
        ParamFct$new("activation", default = "relu", tags = "train",
          levels = c("elu", "relu", "selu", "tanh", "sigmoid","PRelU", "LeakyReLu", "linear")),
        ParamLgl$new("use_batchnorm", default = TRUE, tags = "train"),
        ParamFct$new("loss", default = "binary_crossentropy", tags = "train", levels = keras_reflections$loss$classif),
        ParamUty$new("metrics", default = "accuracy", tags = "train"),
        ParamFct$new("output_activation", default = "sigmoid", levels = c("softmax", "linear", "sigmoid"), tags = "train"),
        ParamUty$new("optimizer", default = "optimizer_adam()", tags = "train")
      ))
      ps$values = list(
        use_embedding = TRUE, embed_dropout = 0, embed_size = NULL,
        embed_char_level = TRUE, embed_max_char_len = 20L,
        alphabet = "abcdefghijklmnopqrstuvwxyzäöüßABCDEFGHIJKLMNOPQRSTUVWXYZÜÖÄ -_",
        activation = "relu",
        # layer_units = c(32, 32, 32),
        initializer = initializer_glorot_uniform(),
        optimizer = optimizer_adam(lr = 3*10^-4),
        regularizer = regularizer_l1_l2(),
        use_batchnorm = FALSE,
        use_dropout = FALSE, dropout = 0, input_dropout = 0,
        loss = "binary_crossentropy",
        metrics = "accuracy",
        output_activation = "sigmoid",
        n_layers = 3L,
        kernel_size = 32,
        filters = 128
      )
      arch = KerasArchitectureCNN$new(build_arch_fn = build_keras_1D_cnn_model, param_set = ps)
      super$initialize(
        feature_types = c("integer", "numeric", "factor", "ordered"),
        # man = "mlr3keras::mlr_learners_classif.keras",
        architecture = arch
      )
    }
  )
)


#' @title Keras Neural Network Feed Forward architecture
#' @rdname KerasArchitecture
#' @family KerasArchitectures
#' @export
KerasArchitectureCNN = R6::R6Class("KerasArchitectureCNN",
  inherit = mlr3keras::KerasArchitecture,
  public = list(
    initialize = function(build_arch_fn, param_set) {
      super$initialize(build_arch_fn = build_arch_fn, param_set = param_set,
        x_transform = function(features, pars) {
          if (pars$use_embedding) reshape_data_embedding(features, factors_jointly = pars$factors_jointly)$data
          else as.matrix(model.matrix(~. - 1, features))
      })
    }
  )
)


# Builds a Keras Feed Forward Neural Network
# @param task [`Task`] \cr
#   A mlr3 Task.
# @param pars [`list`] \cr
#   A list of parameter values from the Learner(Regr|Classif)KerasFF param_set.
# @template kerasff_description
# @return A compiled keras model
build_keras_1D_cnn_model = function(task, pars) {
  output_shape = length(task$target_names)

  if (pars$use_embedding) {
    embd = make_text_embedding(task, pars$embed_size, pars$embed_dropout,
      pars$alphabet, pars$embed_char_level, pars$embed_max_char_len)
    model = embd$layers
  } else {
    model = keras_model_sequential()
  }

  max_layers = 0
  input_length_rem = input_length
  while (input_length_rem > 0) {
    input_length_rem = (input_length_rem - kernel_size) / 2
    max_layers = max_layers + 1
  }

  if (kernel_size != 1L) {
    max_layers = max_layers - 1
  }

  # get n_layers
  n_layers = min(n_layers, max_layers)
  # set kernel_size and filters
  kernel_size = rep(kernel_size, n_layers)
  filters = rep(filters, n_layers)

  for (i in seq_len(n_layers)) {
    model %>%
      layer_conv_1d(
        filters[i], kernel_size[i],
        padding = "valid",
        activation = activation,
        strides = 1,
        kernel_initializer = "he_uniform"
      )
    if (i < n_layers)
      model %>% layer_max_pooling_1d() # pool_size = 3L
    else
      model %>% layer_global_max_pooling_1d()
  }
  # Add a vanilla hidden layer:
  model %>%
    layer_dense(hidden_dims, kernel_initializer = "he_uniform") %>%
    # Apply x% layer dropout
    layer_dropout(dropout) %>%
    layer_activation("relu") %>%
    # Project onto a single unit output layer, and squash it with a sigmoid
    layer_dense(num_classes) %>%
    # sigmoid for multilabel, softmax for multiclass
    layer_activation("sigmoid")

  # Compile model
  model %>% compile(
    loss = "binary_crossentropy", #binary_crossentropy for multilabel, categorical_crossentropy for multiclass
    optimizer = optimizer_adam(lr),
    metrics = "accuracy"
  )
  return(model)
}