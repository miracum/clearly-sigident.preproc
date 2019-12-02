global_env_hack <- function(key,
                            val,
                            pos) {
  assign(
    key,
    val,
    envir = as.environment(pos)
  )
}
