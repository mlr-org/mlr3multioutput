# make_text_embedding = function(task, embed_size = NULL, embed_dropout = 0,
#   embed_text_char_level = TRUE, embed_text_max_len = 20, embed_text_oov_token = "UNK") {
#   assert_task(task)
#   assert_numeric(embed_size, null.ok = TRUE)
#   assert_number(embed_dropout)

#   typedt = task$feature_types
#   data = as.matrix(task$data(cols = task$feature_names))
#   if ("multilabel %in% task$properties")
#   target = as.matrix(task$data(cols = task$target_names))

#   embed_vars = typedt[typedt$type == "character", ]$id
#   n_cont = nrow(typedt) - length(embed_vars)

#   # Embeddings for character variables:
#   # - create text tokenizer
#   # - for each variable:
#   # - create sequences from words
#   # - create a layer_input
#   # - create an embedding
#   # - apply dropout

#   tk = text_tokenizer(
#     lower = FALSE,
#     char_level = char_level,
#     oov_token = oov_token
#   )

#   dict =  unlist(strsplit(alphabet, split = "", fixed = TRUE))
#   word_index = named_list(c(dict, oov_token), NULL)
#   word_index = imap(word_index, function(x, i) which(names(word_index) == i))
#   tk$word_index = word_index

#   embds = list()
#   if (length(embed_vars) > 0) {
#     embds = map(.f = function(feat_name) {
#       x = data[, feat_name]
#       tk %>% fit_text_tokenizer(x)
#       x = tk$texts_to_sequences(x)
#       x = pad_sequences(x, maxlen = max_char_len, padding = "post")

#       vocab_dim = length(dict)
#       input_dim = ncol(x)
#       # Use heuristic from fast.ai https://github.com/fastai/fastai/blob/master/fastai/tabular/data.py
#       # or a user supplied value
#       if (length(embed_size) >= 2) embed_size = embed_size[feat_name]
#       if (length(embed_size) == 0) embed_size = min(600L, round(1.6 * input_dim^0.56))

#       input = layer_input(shape = nrow(x), dtype = "int32", name = feat_name)
#       layers = input %>%
#         layer_embedding(
#           input_dim = as.numeric(vocab_dim),
#           output_dim = as.numeric(embed_size),
#           input_length = input_dim,
#           name = paste0("embed_", feat_name),
#           embeddings_regularizer = regularizer_l2(l = regularization)
#         ) %>%
#         layer_dropout(embed_dropout, input_shape = as.numeric(embed_size)) %>%
#         layer_flatten()
#       return(list(input = input, layers = layers))
#     }, embed_vars)
#   }

#   # Layer for the continuous variables
#   # - apply batchnorm
#   if (n_cont > 0) {
#     input = layer_input(shape = n_cont, dtype = "float32", name = "continuous")
#     layers = input %>% layer_batch_normalization(input_shape = n_cont, axis = 1)
#     embds = c(embds, list(cont = list(input = input, layers = layers)))
#   }

#   # Concatenate all layers
#   if (length(embds) >= 2)
#     layers = layer_concatenate(unname(lapply(embds, function(x) x$layers)))
#   else
#     layers = unname(embds[[1]]$layers)
#   return(list(inputs = lapply(embds, function(x) x$input), layers = layers))
# }
