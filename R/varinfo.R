#All functions, that inform about variable properties

is_with_labels<-function(var) {
  if('data.frame' %in% class(var)) {
    return(purrr::map_lgl(var, is_with_labels))
  }

  if(is.factor(var)) {
    return(TRUE)
  }
  if(!is.null(labelled::val_labels(var))) {
    return(TRUE)
  }
  if('numeric' %in% class(var)) {
    return(!is.null(attr(var, 'labels')))
  }
  return(FALSE)
}

#' Gets canonical list of variables. Returns NULL when no labels
#'
#' @param var Input vector.
#'
#' @return Returns named vector of the same type as the input vector that lists the labels.
get_typeLabels<-function(var, flag_use_na=FALSE){
  if('data.frame' %in% class(var)) {
    return(purrr::map(var, get_typeLabels, flag_use_na=TRUE))
  }

  if('factor' %in% class(var)) {
    l<-attr(var, 'levels', exact = TRUE)
    return(setNames(as.integer(seq_along(l)), l))
  }
  if(any(c('labelled', 'numeric') %in% class(var))) {
    l<-attr(var, 'labels', exact=TRUE)
    if(!is.null(l)) {
      checkmate::assert_class(l, 'numeric')
      l<-c(sort(l[!is.na(l)]), l[is.na(l)][order(haven::na_tag(l[is.na(l)]))])
    } else if (flag_use_na) {
      l<-NA
    }
    return(l)
  }
  if (flag_use_na) {
    return(NA)
  } else {
    return(NULL)
  }
}

get_theoretical_min<-function(var, flag_use_na=FALSE) {
  if('data.frame' %in% class(var)) {
    return(purrr::map(var, get_theoretical_min, flag_use_na=TRUE))
  }

  a<-attr(var, 'theoretical_min', exact = TRUE)
  if(!is.null(a)) {
    checkmate::assert_class(a, class(unclass(var)))
  } else if (flag_use_na) {
    return(NA)
  }
  return(a)
}

get_theoretical_max<-function(var, flag_use_na=FALSE) {
  if('data.frame' %in% class(var)) {
    return(purrr::map(var, get_theoretical_max, flag_use_na=TRUE))
  }

  a<-attr(var, 'theoretical_max', exact = TRUE)
  if(!is.null(a)) {
    checkmate::assert_class(a, class(unclass(var)))
  } else if (flag_use_na) {
    return(NA)
  }
  return(a)
}

get_force_integers<-function(var) {
  if('data.frame' %in% class(var)) {
    return(purrr::map_lgl(var, get_force_integers))
  }

  if(any(c('factor', 'integer') %in% class(var))) {
    return(TRUE)
  }
  a<-attr(var, 'force_integers', exact = TRUE)
  if(!is.null(a)) {
    checkmate::assert_flag(a)
    return(a)
  }
  return(FALSE)
}

get_required<-function(var) {
  if('data.frame' %in% class(var)) {
    return(purrr::map_lgl(var, get_required))
  }

  a<-attr(var, 'required', exact = TRUE)
  if(!is.null(a)) {
    checkmate::assert_flag(a)
    return(a)
  }
  return(FALSE)
}

get_limit_to_labels<-function(var) {
  if('data.frame' %in% class(var)) {
    return(purrr::map_lgl(var, get_limit_to_labels))
  }

  if ('factor' %in% class(var)) {
    return(TRUE)
  }
  a<-attr(var, 'limit_to_labels', exact = TRUE)
  if(!is.null(a)) {
    checkmate::assert_flag(a)
    return(a)
  }
  return(FALSE)
}

