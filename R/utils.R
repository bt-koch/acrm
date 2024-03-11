load_all <- function() {
  for (script in list.files("R", full.names = T)) {
    source(script)
  }
}