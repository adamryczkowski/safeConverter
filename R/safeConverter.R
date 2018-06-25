#' Returns object that uniquelly determines compatiblity class of the variable.
#'
#' It encapsulates variable's class and labels with levels and tagged NAs. It ignores whether the variable is ordinal or nominal, because it can be
#' sensitive to the statistical context in which the variable is used.
#'
#' @param var - Variable to asses.
#'
#' @return - String hash of the object, that consists of the letter denoting its type + hash of the labels (if applicable)
#'
#' @export
get_typeDigest<-function(var) {
  typeInfo<-get_typeInfo(var)
  return(digest::digest(typeInfo))
}

#' Returns object that uniquelly determines compatiblity class of the variable.
#'
#' It encapsulates variable's class and labels with levels and tagged NAs. It ignores whether the variable is ordinal or nominal, because it can be
#' sensitive to the statistical context in which the variable is used.
#'
#' @param var - Variable to asses.
#'
#' @return - Object that include variable's class, its labels and other limits. It includes
#' \describe{
#' \item{typeClass}{Information about the data type of the vector}
#' \item{labels}{Named list of labels, if there are any. Supported of \code{factor}, \code{labelled} and \code{numeric}}
#' \item{theoretical_min, theoretical_min}{For numerical variables (integer, numeric and dates) minimal and maximal legal value. Value of the same type as the class of the vector.}
#' \item{force_integers}{Boolean. Are fractional values legal?}
#' \item{required}{Boolean. Are missing values legal?}
#' \item{limit_to_labels}{Boolean. Are non-labelled values legal (makes sense only for \code{labelled} and \code{numeric} with labels.}
#' }
#'
#'
#' @export
get_typeInfo<-function(var) {
  browser()
  #TODO
  typeClass<-get_typeClass(var)
  labels<-get_typeLabels(var)
  theoretical_min<-get_theoretical_min(var)
  theoretical_max<-get_theoretical_max(var)
  force_integers<-get_force_integers(var)
  required<-get_required(var)
  limit_to_labels<-get_limit_to_labels(var)
  return(list(typeClass=typeClass, labels=labels,
              theoretical_min=theoretical_min, theoretical_max=theoretical_max,
              force_integers=force_integers,required=required,
              limit_to_labels=limit_to_labels))
}


attr2label<-function(attrname) {
  checkmate::assert_string(attrname)
  attr_labels=list(labels='value dictionary (full format)',
                   levels='value dictionary (abbreviated format)',
                   theoretical_min='minimal value specification',
                   theoretical_max='maximal value specification',
                   required='whether missing values are forbidden',
                   limit_to_labels='whether to allow values outside of dictionary',
                   force_integers='whether to allow only integer (whole) numbers')

  if(attrname %in% names(attr_labels)) {
    return(attr_labels[[attrname]])
  }
  browser() #Value outside from allowed attributes
}

#' Returns a letter that represents a class type.
#'
#' @param var - Variable to asses.
#'
#' @return - Single character letter denoting the class of the var
get_typeClass<-function(var) {
  if('data.frame' %in% class(var)) {
    return(purrr::map_chr(as.list(var), class2vartype))
  } else {
    has_labels<-is_with_labels(var)
    class2vartype_str(class(var), all(is.na(var)), has_labels=has_labels)
  }
}



#' Converts typeClass (one letter) into the textual representation
#'
#' @export
typeClass2class <- function(typeClass)
{
  assert_typeClass(typeClass)
  return(all_classTypes[[typeClass]])
}



all_classTypes<-c(
  'B'='logical',
  'D'='Date',
  'F'='factor',
  'I'='integer',
  'L'='labelled',
  'N'='numeric',
  'S'='character',
  'T'='POSIXct')

#Returns a letter that encodes data type.
class2vartype_str<-function(classes, all_is_na, has_labels=NA)
{
  is_labelled<-'labelled' %in% classes
  if(is_labelled) {
    if(!is.na(has_labels)) {
      if(!has_labels)  {
        is_labelled<-FALSE
      }
    }
  }
  classes <- setdiff(classes, 'labelled')
  classes_sorted <- paste0(sort(classes), collapse=',')

  if(classes_sorted %in% c('factor', 'factor,ordered'))
  {
    return('F')
  } else if(classes_sorted == 'integer')
  {
    return('I')
  } else if(classes_sorted == 'numeric')
  {
    if(is.na(has_labels) || !has_labels) {
      return('N')
    } else {
      return('L')
    }
  } else if(classes_sorted %in% c('Date'))
  {
    return('D')
  } else if(classes_sorted %in% c("POSIXct,POSIXt","POSIXlt,POSIXt"))
  {
    return('T')
  } else if(classes_sorted %in% c('character','character,labelled'))
  {
    return('S')
  } else if(classes_sorted %in% c('logical', 'labelled,logical'))
  {
    if(all_is_na) {
      return('0')
    } else {
      return('B')
    }
  } else if(is_labelled)
  {
    return('L')
  } else {
    browser()
    return('')
    #    stop(paste0("Unkown class: ", classes_sorted))
    #    browser()
  }
}
