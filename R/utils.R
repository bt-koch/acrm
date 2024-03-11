load_all <- function() {
  for (script in list.files("R", full.names = T)) {
    source(script)
  }
}

reset_workspace <- function() {
  rmv_objects <- ls(envir = .GlobalEnv)[sapply(ls(envir = .GlobalEnv), function(i) class(get(i))) != "function"]
  rm(list = rmv_objects, pos = .GlobalEnv)
}

prepare_session <- function() {
  load_all()
  reset_workspace()
}
