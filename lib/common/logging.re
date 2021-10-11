let get_set_log_level = (logger_name, level) =>
  Easy_logging.Logging.get_logger(logger_name)  |> (logger) =>
  logger#set_level(level);

let set_logging_level = (level) => 
  get_set_log_level("Mesh", level);