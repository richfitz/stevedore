## All the argument shenanigans go in this file so that I can
## eventually rationalise things until they make sense.

## This is *not* a general purpose partialling function (c.f.,
## purrr::partial) but one designed to provide an interface via the
## function arguments of the generated function suitable for end
## users.  So preserving argument names etc is important.  It's also
## very likely that the functions being partialled are themselves
## generated and I want to make sure that is found in the right place.
## This relies on the name of the partialled argument not being the
## same as the function being partialled.
partial1 <- function(FUN, x, env = parent.frame(),
                     name = deparse(substitute(FUN))) {
  args <- formals(FUN)

  env <- new.env(parent = env)
  env[[name]] <- FUN
  env[[names(args)[[1]]]] <- x

  body <- as.call(lapply(c(name, names(args)), as.name))
  as.function(c(args[-1], body), env)
}

drop_args <- function(FUN, names, env = parent.frame(),
                      name = deparse(substitute(FUN))) {
  ## This is the beginning of work to make nice functions for the user
  ## facing part of the package - first thing is to drop the
  ## arguments.  That's not going to be the lot though because we'll
  ## need to add additional ones (or rather concatenate functions
  ## argument lists and direct them difference places).
  args <- formals(FUN)
  args_keep <- args[setdiff(names(args), names)]
  env <- new.env(parent = env)
  env[[name]] <- FUN
  body <- as.call(lapply(c(name, names(args_keep)), as.name))
  as.function(c(args_keep, body), env)
}

modify_args <- function(FUN, drop = NULL, fix = NULL, after = NULL,
                        env = parent.frame(),
                        name = deparse(substitute(FUN))) {
  env <- new.env(parent = env)
  ## NOTE: this introduces the restriction that the name must not
  ## appear in the argument list - that's reasonable in our cases but
  ## not in general.  Also untested is that 'drop' is a character
  ## vector, 'fix' is a named list, and that drop and names(fix) are
  ## not intersecting (but that both are subsets of the real
  ## arguments).
  env[[name]] <- FUN
  if (!is.null(fix)) {
    list2env(fix, env)
  }

  args <- formals(FUN)
  args_call <- setdiff(names(args), drop)
  body <- as.call(lapply(c(name, set_names(args_call, args_call)), as.name))
  if (!is.null(after)) {
    env[["after"]] <- after
    body <- as.call(c(list(quote(after)), body))
  }
  args_keep <- args[setdiff(names(args), c(drop, names(fix)))]
  as.function(c(args_keep, body), env)
}
