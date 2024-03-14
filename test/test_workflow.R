prepare_session()

# 1: user makes input
input <- dummy_input(customer_type = "private")

# 2: read user input
model_input <- read_input(input)

# 3: model input
model_input <- preprocess_data(model_input)

# 4: get relevant segment
segment <- get_relevant_segment(input = read_input(input))

# 5. get relevant model
model <- linear_regression_get()

# 6. predict using relevant model
linear_regression_predict(model, model_input)

# 6.1 test default behaviour
stopifnot(linear_regression_predict(model, model_input) == linear_regression_predict(data = model_input))
