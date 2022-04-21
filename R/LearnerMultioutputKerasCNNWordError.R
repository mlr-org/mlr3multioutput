#' @title Special Keras Convolutionary Neural Network for text error detection
#'
#' @usage NULL
#' @aliases mlr_learners_multioutput.kerascnnworderror
#' @format [R6::R6Class()] inheriting from [mlr3multioutput::LearnerMultioutputKeras].
#'
#' @section Construction:
#' ```
#' LearnerMultioutputKerasCNNWordError$new()
#' mlr3::mlr_learners$get("multioutput.kerascnnworderror")
#' mlr3::lrn("multioutput.kerascnnworderror")
#' ```
#' @template learner_methods
#' @template seealso_learner
#' @templateVar learner_name multioutput.kerascnnworderror
#' @template example
#' @export
LearnerMultioutputKerasCNNWordError = R6::R6Class("LearnerMultioutputKerasCNNWordError",
  inherit = LearnerMultioutputKeras,
  public = list(
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("hidden_dims", lower = 1, upper = Inf, default = 128, tags = "train"),
        ParamDbl$new("regularization", lower = 0, upper = 1, default = 0.01, tags = "train"),
        ParamUty$new("regularizer", default = "regularizer_l1_l2()", tags = "train"),
        ParamUty$new("char_colnames", tags = c("train", "predict")),
        ParamLgl$new("use_dropout", default = TRUE, tags = "train"),
        ParamDbl$new("factor_embed_dropout", lower = 0, upper = 1, default = 0.2, tags = "train"),
        ParamDbl$new("char_embed_dropout", lower = 0, upper = 1, default = 0.2, tags = "train"),
        ParamDbl$new("dropout", lower = 0, upper = 1, default = 0.2, tags = "train"),
        ParamDbl$new("input_dropout", lower = 0, upper = 1, tags = "train"),
        ParamDbl$new("learning_rate", lower = 0, upper = 1, default = 3*10^-4, tags = "train"),
        ParamLgl$new("use_embedding", default = TRUE, tags = c("train", "predict")),
        ParamLgl$new("factors_jointly", default = TRUE, tags = c("train", "predict")),
        ParamDbl$new("factor_embed_size", default = NULL, lower = 1, upper = Inf, tags = "train", special_vals = list(NULL)),
        ParamDbl$new("char_embed_size", default = NULL, lower = 1, upper = Inf, tags = "train", special_vals = list(NULL)),
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
        use_embedding = FALSE, factors_jointly = TRUE, factor_embed_dropout = 0, factor_embed_size = NULL,
        char_embed_dropout = 0.1, char_embed_size = NULL,
        activation = "relu",
        char_colnames = c("word", "input"),
        # layer_units = c(32, 32, 32),
        initializer = initializer_he_uniform(),
        optimizer = optimizer_adam(lr = 3*10^-4),
        regularizer = regularizer_l1_l2(),
        regularization = 0.01,
        use_batchnorm = FALSE,
        use_dropout = TRUE, dropout = 0.1, input_dropout = 0,
        loss = "binary_crossentropy",
        metrics = "categorical_accuracy",
        output_activation = "sigmoid",
        n_conv_layers = 3L,
        hidden_dims = 128L,
        kernel_size = 3L,
        filters = 128L
      )
      arch = KerasArchitectureCNNWordError$new(build_arch_fn = build_keras_1D_multilabel_cnn_model_word_error, param_set = ps)
      super$initialize(
        id = "multioutput.kerascnnworderror",
        feature_types = c("integer", "numeric", "factor", "ordered"),
        # man = "mlr3multioutput::mlr_learners_multioutput.kerascnnworderror",
        architecture = arch
      )
    }
  )
)




#' @title Special Keras Convolutionary Neural Network for text error detection
#'
#' @usage NULL
#' @aliases mlr_learners_multioutput.kerascnnworderrorsimple
#' @format [R6::R6Class()] inheriting from [mlr3multioutput::LearnerMultioutputKeras].
#'
#' @section Construction:
#' ```
#' LearnerMultioutputKerasCNNWordErrorSimple$new()
#' mlr3::mlr_learners$get("multioutput.kerascnnworderrorsimple")
#' mlr3::lrn("multioutput.kerascnnworderrorsimple")
#' ```
#' @template learner_methods
#' @template seealso_learner
#' @templateVar learner_name multioutput.kerascnnworderrorsimple
#' @template example
#' @export
LearnerMultioutputKerasCNNWordErrorSimple = R6::R6Class("LearnerMultioutputKerasCNNWordErrorSimple",
  inherit = LearnerMultioutputKeras,
  public = list(
    initialize = function() {
      ps = ParamSet$new(list(
        ParamInt$new("hidden_dims", lower = 1, upper = Inf, default = 128, tags = "train"),
        ParamDbl$new("regularization", lower = 0, upper = 1, default = 0.01, tags = "train"),
        ParamUty$new("regularizer", default = "regularizer_l1_l2()", tags = "train"),
        ParamUty$new("char_colnames", tags = c("train", "predict")),
        ParamLgl$new("use_dropout", default = TRUE, tags = "train"),
        ParamDbl$new("factor_embed_dropout", lower = 0, upper = 1, default = 0.2, tags = "train"),
        ParamDbl$new("char_embed_dropout", lower = 0, upper = 1, default = 0.2, tags = "train"),
        ParamDbl$new("dropout", lower = 0, upper = 1, default = 0.2, tags = "train"),
        ParamDbl$new("input_dropout", lower = 0, upper = 1, tags = "train"),
        ParamDbl$new("learning_rate", lower = 0, upper = 1, default = 3*10^-4, tags = "train"),
        ParamLgl$new("use_embedding", default = TRUE, tags = c("train", "predict")),
        ParamLgl$new("factors_jointly", default = TRUE, tags = c("train", "predict")),
        ParamDbl$new("factor_embed_size", default = NULL, lower = 1, upper = Inf, tags = "train", special_vals = list(NULL)),
        ParamDbl$new("char_embed_size", default = NULL, lower = 1, upper = Inf, tags = "train", special_vals = list(NULL)),
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
        use_embedding = FALSE, factors_jointly = TRUE, factor_embed_dropout = 0, factor_embed_size = NULL,
        char_embed_dropout = 0.1, char_embed_size = NULL,
        activation = "relu",
        char_colnames = c("word", "input"),
        # layer_units = c(32, 32, 32),
        initializer = initializer_he_uniform(),
        optimizer = optimizer_adam(lr = 3*10^-4),
        regularizer = regularizer_l1_l2(),
        regularization = 0.01,
        use_batchnorm = FALSE,
        use_dropout = TRUE, dropout = 0.1, input_dropout = 0,
        loss = "binary_crossentropy",
        metrics = "categorical_accuracy",
        output_activation = "sigmoid",
        n_conv_layers = 3L,
        hidden_dims = 128L,
        kernel_size = 3L,
        filters = 128L
      )
      arch = KerasArchitectureCNNWordErrorSimple$new(build_arch_fn = build_keras_1D_multilabel_cnn_model_word_error_simple, param_set = ps)
      super$initialize(
        id = "multioutput.kerascnnworderrorsimple",
        feature_types = c("integer", "numeric", "factor", "ordered"),
        # man = "mlr3multioutput::mlr_learners_multioutput.kerascnnworderror",
        architecture = arch
      )
    }
  )
)


#' @title Special Keras Neural Network CNN architecture for writing error detection
#' @rdname KerasArchitecture
#' @family KerasArchitectures
#' @export
KerasArchitectureCNNWordError = R6::R6Class("KerasArchitectureCNN",
  inherit = mlr3keras::KerasArchitecture,
  public = list(
    initialize = function(build_arch_fn, param_set) {
      super$initialize(build_arch_fn = build_arch_fn, param_set = param_set,
        x_transform = function(features, pars) {
          word = grepl(pars$char_colnames[1], colnames(features))
          error = grepl(pars$char_colnames[2], colnames(features))
          if (pars$use_embedding) {
            x = reshape_data_embedding(features, factors_jointly = TRUE)$data$continuous
          } else {
            x = as.matrix(model.matrix(~. - 1, features))
          }
          x1 = x[, word]
          x2 = x[, error]

          return(list(word_input = x1, error_input = x2))
      })
    }
  )
)


#' @title Special Keras Neural Network CNN architecture for writing error detection
#' @rdname KerasArchitecture
#' @family KerasArchitectures
#' @export
KerasArchitectureCNNWordErrorSimple = R6::R6Class("KerasArchitectureCNN",
  inherit = mlr3keras::KerasArchitecture,
  public = list(
    initialize = function(build_arch_fn, param_set) {
      super$initialize(build_arch_fn = build_arch_fn, param_set = param_set,
        x_transform = function(features, pars) {
          if (pars$use_embedding) {
            x = reshape_data_embedding(features, factors_jointly = TRUE)$data$continuous
          } else {
            x = as.matrix(model.matrix(~. - 1, features))
          }
          return(x)
      })
    }
  )
)


# Builds a Keras 1D-Convulutionary Neural Network
# @param task [`Task`] \cr
#   A mlr3 Task.
# @param pars [`list`] \cr
#   A list of parameter values from the Learner(Regr|Classif)KerasCNN param_set.
# @template kerascnn_description
# @return A compiled keras model
build_keras_1D_multilabel_cnn_model_word_error = function(task, pars) {
  require(keras)
  require(magrittr)
  if (pars$use_embedding) {
    embd = mlr3keras::make_embedding(task, pars$factor_embed_size, pars$factor_embed_dropout, pars$factors_jointly)
    factor_layers = embd$layers
  } else {
    factor_layers = NULL
  }

  char_cols = grepl(pars$char_colnames[1], task$feature_names) | grepl(pars$char_colnames[2], task$feature_names)
  inputs = task$data(cols = task$feature_names[char_cols])
  browser()
  n_cat = sum(map_dbl(inputs, function(x){
    if (is.factor(x)) length(levels(x)) else length(unique(x))
  }))


  char_input_length = ncol(inputs) / 2
  max_layers = 0
  input_length_rem = char_input_length
  while (input_length_rem > 0) {
    input_length_rem = (input_length_rem - pars$kernel_size) / 2
    max_layers = max_layers + 1
  }

  if (pars$kernel_size != 1L) {
    max_layers = max_layers - 1
  }

  # get n_layers
  n_layers = min(pars$n_conv_layers, max_layers)
  # set kernel_size and filters
  kernel_size = rep(pars$kernel_size, n_layers)
  filters = rep(pars$filters, n_layers)

  if (is.null(pars$char_embed_size))
    char_embed_size = min(600L, round(1.6 * n_cat^0.56))

  embedding = layer_embedding(
    name = "embed_words",
    input_length = char_input_length,
    input_dim = as.numeric(n_cat),
    output_dim = as.numeric(char_embed_size),
    embeddings_initializer = initializer_he_uniform()
  )

 dropout1 = layer_dropout(rate = pars$char_embed_dropout, input_shape = as.numeric(char_embed_size))

  for (i in seq_len(n_layers)) {
    assign(
      x = paste0("conv", i),
      value = layer_conv_1d(
        filters = filters[i],
        kernel_size = kernel_size[i],
        padding = "valid",
        activation = "relu",
        strides = 1,
        kernel_initializer = "he_uniform"
      )
    )
  }
  maxpool1 = layer_max_pooling_1d()
  maxpool2 = layer_max_pooling_1d()
  globalmaxpool = layer_global_max_pooling_1d()

  word_input = layer_input(shape = char_input_length, name = "word_input")
  error_input = layer_input(shape = char_input_length, name = "error_input")

  word_layers = word_input %>%
    embedding %>%
    dropout1 %>%
    conv1 %>%
    maxpool1 %>%
    conv2 %>%
    maxpool2 %>%
    conv3 %>%
    globalmaxpool

  error_layers = error_input %>%
    embedding %>%
    dropout1 %>%
    conv1 %>%
    maxpool1 %>%
    conv2 %>%
    maxpool2 %>%
    conv3 %>%
    globalmaxpool


  output = layer_concatenate(c(word_layers, error_layers))

  output = output %>%
    layer_dense(pars$hidden_dims, kernel_initializer = "he_uniform") %>%
    layer_dropout(pars$dropout) %>%
    layer_dense(units = pars$hidden_dims, activation = 'relu') %>%
    layer_dense(length(task$target_names), activation = "sigmoid", name = "output")


  model = keras_model(
    inputs = c(word_input, error_input),
    outputs = output
  )

  # Compile model
  model %>% compile(
    optimizer = pars$optimizer,
    loss = pars$loss, #binary_crossentropy for multilabel, categorical_crossentropy for multiclass
    metrics = pars$metrics
  )

  return(model)
}


build_keras_1D_multilabel_cnn_model_word_error_simple = function(task, pars) {
  require(keras)
  require(magrittr)
  if (pars$use_embedding) {
    embd = mlr3keras::make_embedding(task, pars$factor_embed_size, pars$factor_embed_dropout, pars$factors_jointly)
    factor_layers = embd$layers
  } else {
    factor_layers = NULL
  }

  char_cols = grepl(pars$char_colnames[1], task$feature_names) | grepl(pars$char_colnames[2], task$feature_names)
  inputs = task$data(cols = task$feature_names[char_cols])
  n_cat = sum(map_dbl(inputs, function(x){
    if (is.factor(x)) length(levels(x)) else length(unique(x))
  }))


  char_input_length = ncol(inputs)

  max_layers = 0
  input_length_rem = char_input_length
  while (input_length_rem > 0) {
    input_length_rem = (input_length_rem - pars$kernel_size) / 2
    max_layers = max_layers + 1
  }

  if (pars$kernel_size != 1L) {
    max_layers = max_layers - 1
  }

  # get n_layers
  n_layers = min(pars$n_conv_layers, max_layers)
  # set kernel_size and filters
  kernel_size = rep(pars$kernel_size, n_layers)
  filters = rep(pars$filters, n_layers)

  if (is.null(pars$char_embed_size))
    char_embed_size = min(600L, round(1.6 * n_cat^0.56))

  model = keras_model_sequential() %>%
    layer_embedding(
      name = "embed_words",
      input_length = char_input_length,
      input_dim = as.numeric(n_cat),
      output_dim = as.numeric(char_embed_size),
      embeddings_initializer = initializer_he_uniform(),
      embeddings_regularizer = regularizer_l2(l = pars$regularization)
    ) %>%
    layer_dropout(rate = pars$char_embed_dropout, input_shape = as.numeric(char_embed_size))

  for (i in 1:(n_layers - 1)) {
    model = model %>%
      layer_conv_1d(
        filters = filters[i],
        kernel_size = kernel_size[i],
        padding = "valid",
        activation = "relu",
        strides = 1,
        kernel_initializer = "he_uniform"
      ) %>% layer_max_pooling_1d()
  }

  model = model %>%
    layer_conv_1d(
      filters = filters[n_layers],
      kernel_size = kernel_size[n_layers],
      padding = "valid",
      activation = "relu",
      strides = 1,
      kernel_initializer = "he_uniform"
    ) %>%
    layer_global_max_pooling_1d() %>%
    layer_dense(pars$hidden_dims, kernel_initializer = "he_uniform") %>%
    layer_dropout(pars$dropout) %>%
    layer_dense(units = pars$hidden_dims, activation = 'relu') %>%
    layer_dense(length(task$target_names), activation = "sigmoid", name = "output")

  # Compile model
  model %>% compile(
    optimizer = pars$optimizer,
    loss = pars$loss, #binary_crossentropy for multilabel, categorical_crossentropy for multiclass
    metrics = pars$metrics
  )

  return(model)
}