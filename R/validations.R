#' Makes sure all the attributes are valid and match the reality of the variable.
#'
#' It validates the following attributes:
#' \describe{
#' \item{\strong{labels}, \strong{levels}}{makes sure they match the variable's class}
#' \item{\strong{theoretical_min}, \strong{theoretical_max}}{makes sure they exist only on numerical type, and that they are actually observed}
#' \item{\strong{force_integers}}{makes sure it is only on numerical/character type and that it is observed}
#' \item{\strong{required}}{makes sure there are no NAs in the values, and in the labels (in case of the labelled)}
#' \item{\strong{limit_to_labels}}{Makes sure there are labels, and that there are no values from outside the labels.}
#' }
#'
#' @param reporter Function that gets the variable reports

validateAttributes<-function(var, reporter=NULL){

  UseMethod("validateAttributes", var, reporter=reporter, flagValidate=flagValidate)
}


get_reportAttributes.forbidden_attribute<-function(reportClass, ...)  {

  formatter<-function(varcase_txt, attrname, typeClass) {
    classname<-typeClass2class(typeClass)
    attr_label<-attr2label(attrname)
    paste0(varcase_txt, " do not support attribute `", attrname, "` (", attr_label,"), because it is ", classname)
  }
  dbcasereport::typeReporter_factory(reportClass = reportClass, type = 'reportAttributes.forbidden_attribute',
                                     type_caption = "Forbidden variable attributes for given variable type",
                                     extra_parameters = list(attrvalue=quote(expr = )),
                                     formatters=formatter, flag_use_case=FALSE, ...)
}


get_reportAttributes.wrong_attribute_class<-function(reportClass, ...)  {

  formatter<-function(varcase_txt, attrname, attrvalue, typeClass, subset_df) {
    classname<-typeClass2class(typeClass)
    attr_label<-attr2label(attrname)
    if(ncol(subset_df)>1) {
      s='s'
    } else {
      s=''
    }
    paste0(varcase_txt, " attribute ", attr_label, "` (", attrname, "`) cannot have value", s, " of type ", classname)
  }
  dbcasereport::typeReporter_factory(reportClass = reportClass, type = 'reportAttributes.wrong_attribute_class',
                                     type_caption = "Bad variable attribute class for given variable type",
                                     formatters=formatter, flag_use_case=FALSE, ...)
}

get_reportAttributes.invalid_attribute<-function(reportClass, ...)  {

  formatter<-function(varcase_txt, attrname, attrvalue, typeClass) {
    classname<-typeClass2class(typeClass)
    attr_label<-attr2label(attrname)
    class_numeral<-itemNaming::English_numeral$new(singular='class', plural='classes', flag_skip_one=FALSE)
    attr_classes<-itemNaming::vector_formatter_df_gen(class_numeral)(class(attrvalue))
    if(length(class(attrvalue))>1) {
      attr_classes<-paste0(class_numeral(2), " ", attr_classes)
    } else {
      attr_classes<-paste0(class_numeral(1), " ", attr_classes)
    }
    paste0(varcase_txt, " attribute `", attrname, "` (", attr_label, ") cannot have value of ", attr_classes)
  }
  dbcasereport::typeReporter_factory(reportClass = reportClass, type = 'reportAttributes.invalid_attribute',
                                     type_caption = "Bad variable attribute values for given variable type",
                                     formatters=formatter, flag_use_case=FALSE, ...)
}

get_reportAttributes.missing_attribute<-function(reportClass, ...)  {

  formatter<-function(varcase_txt, attrname, subset_df, typeClass, ...) {
    classname<-typeClass2class(typeClass)
    paste0(varcase_txt, (if(ncol(subset_df)>1) " do" else " does"),
           " not have attribute", (if(ncol(subset_df)>1) "" else "s"),
           attrname, ", which ", (if(ncol(subset_df)>1) " are" else " is"),
           " required for variables of type ", classname)
  }
  dbcasereport::typeReporter_factory(reportClass = reportClass, type = 'reportAttributes.missing_attribute',
                                     type_caption = "Missing variable attributes",
                                     formatters=formatter, flag_use_case=FALSE, ...)
}



get_reportAttributes.requred<-function(reportClass, ...)  {

  formatter<-function(varcase_txt, subset_df) {
    if(ncol(subset_df)>1) {
      s='s are'
    } else {
      s=' is'
    }
    paste0(varcase_txt, " are missing, but the variable",s, " not allowed to have missing observations")
  }
  dbcasereport::typeReporter_factory(reportClass = reportClass, type = 'reportAttributes.requred',
                                     type_caption = "Missing observations of the variable that should not have missing",
                                     formatters=formatter, flag_use_case=TRUE, ...)
}


get_reportAttributes.minmax<-function(reportClass, ...)  {

  formatter<-function(varcase_txt, attrname,  attrvalue, subset_df, ...) {
    if(nrow(subset_df)>1 || ncol(subset_df)>1) {
      valtxt<-'values'
      exc<-'exceed'
    } else {
      valtxt<-'value'
      exc<-'exceeds'
    }
    if(attrname=='theoretical_min') {
      attr_txt<-paste0('minimal ', valtxt)
    } else {
      attr_txt<-paste0('maximal ', valtxt)
    }
    paste0(attr_txt," of ", varcase_txt, " ", exc, " ", itemNaming::format_values(attrvalue))
  }

  dbcasereport::typeReporter_factory(reportClass = reportClass, type = 'reportAttributes.minmax',
                                     type_caption = "Values that exceed minimal/maximal value specification",
                                     formatters=formatter, flag_use_case=TRUE, ...)
}

get_reportAttributes.not_found_minmax<-function(reportClass, ...)  {

  formatter<-function(varcase_txt, attrname, attrvalue, ...) {
    classname<-typeClass2class(typeClass)
    if(attrname=='theoretical_min') {
      attr_txt<-'minimal value'
    } else {
      attr_txt<-'maximal value'
    }
    paste0(attr_txt, "`", attrvalue, "` of ", varcase_txt, " does not exist among known labels")
  }

  dbcasereport::typeReporter_factory(reportClass = reportClass, type = 'reportAttributes.not_found_minmax',
                                     type_caption = "Existance of minimal/maximal values in dictionary, if provided by label rather than by value",
                                     formatters=formatter, flag_use_case=FALSE, ...)
}


get_reportAttributes.force_integers<-function(reportClass, ...)  {

  formatter<-function(varcase_txt, value, subset_df, ...) {
    if(nrow(context_df)>1 || ncol(context_df)>1) {
      paste0(varcase_txt, " have non-integer value of ", itemNaming::format_values(value))
    } else {
      paste0(varcase_txt, " has non-integer value of ", itemNaming::format_values(value))
    }
  }

  dbcasereport::typeReporter_factory(reportClass = reportClass, type = 'reportAttributes.force_integers',
                                     type_caption = "Non integer cases in variables required to be integer",
                                     formatters=formatter, flag_use_case=TRUE, ...)
}


get_reportAttributes.limit_to_labels<-function(reportClass, ...)  {

  formatter<-function(varcase_txt, value) {
    paste0(varcase_txt, " has value ", itemNaming::format_values(value), " that is missing from the dictionary of labels" )
  }

  dbcasereport::typeReporter_factory(reportClass = reportClass, type = 'reportAttributes.limit_to_labels',
                                     type_caption = "Values outside the variables' dictionaries",
                                     formatters=formatter, flag_use_case=TRUE, ...)
}



#' Validates all asumptions around the character variable.
validateAttributes.character<-function(var, reporter){
  delayedAssign('forbidden_attribute', get_reportAttributes.forbidden_attribute(reportClass = reporter, typeClass='S'))
  delayedAssign('invalid_attribute', get_reportAttributes.invalid_attribute(reportClass = reporter, typeClass='S'))
  delayedAssign('requred', get_reportAttributes.requred(reportClass = reporter))

  if(!is.null(attr(var, 'labels'))) {
    forbidden_attribute(attrname='labels',  attrvalue=attr(var, 'labels'))
  }
  if(!is.null(attr(var, 'levels'))) {
    forbidden_attribute(attrname='levels',  attrvalue=attr(var, 'levels'))
  }
  if(!is.null(attr(var, 'theoretical_min'))) {
    forbidden_attribute(attrname='theoretical_min',  attrvalue=attr(var, 'theoretical_min'))
  }
  if(!is.null(attr(var, 'theoretical_max'))) {
    forbidden_attribute(attrname='theoretical_max',  attrvalue=attr(var, 'theoretical_max'))
  }
  if(!is.null(attr(var, 'force_integers'))) {
    forbidden_attribute(attrname='force_integers',  attrvalue=attr(var, 'force_integers'))
  }
  if(!is.null(attr(var, 'required'))) {
    if(!checkmate::check_flag(attr(var, 'required'))) {
      invalid_attribute(attrname='required',  attrvalue=attr(var, 'required'))
    }
    nas<-which(is.na(var))
    if(length(nas)>0) {
      required(case=nas)
    }
  }
  if(!is.null(attr(var, 'limit_to_labels'))) {
    forbidden_attribute(attrname='limit_to_labels',  attrvalue=attr(var, 'limit_to_labels'))
  }
}

validateAttributes.logical<-function(var, reporter){
  delayedAssign('forbidden_attribute', get_reportAttributes.forbidden_attribute(reportClass = reporter, typeClass='B'))
  delayedAssign('invalid_attribute', get_reportAttributes.invalid_attribute(reportClass = reporter, typeClass='B'))
  delayedAssign('requred', get_reportAttributes.requred(reportClass = reporter))

  if(!is.null(attr(var, 'labels'))) {
    forbidden_attribute(attrname='labels',  attrvalue=attr(var, 'labels'))
  }
  if(!is.null(attr(var, 'levels'))) {
    forbidden_attribute(attrname='levels',  attrvalue=attr(var, 'levels'))
  }
  if(!is.null(attr(var, 'theoretical_min'))) {
    forbidden_attribute(attrname='theoretical_min',  attrvalue=attr(var, 'theoretical_min'))
  }
  if(!is.null(attr(var, 'theoretical_max'))) {
    forbidden_attribute(attrname='theoretical_max',  attrvalue=attr(var, 'theoretical_max'))
  }
  if(!is.null(attr(var, 'force_integers'))) {
    forbidden_attribute(attrname='force_integers',  attrvalue=attr(var, 'force_integers'))
  }
  if(!is.null(attr(var, 'required'))) {
    if(!checkmate::check_flag(attr(var, 'required'))) {
      invalid_attribute(attrname='required',  attrvalue=attr(var, 'required'))
    }
    nas<-which(is.na(var))
    if(length(nas)>0) {
      requred(case=nas)
    }
  }
  if(!is.null(attr(var, 'limit_to_labels'))) {
    forbidden_attribute(attrname='limit_to_labels',  attrvalue=attr(var, 'limit_to_labels'))
  }
}

validateAttributes.Date<-function(var, reporter){
  delayedAssign('forbidden_attribute', get_reportAttributes.forbidden_attribute(reportClass = reporter, typeClass='D'))
  delayedAssign('wrong_attribute_class', get_reportAttributes.wrong_attribute_class(reportClass = reporter,typeClass='D'))
  delayedAssign('requred', get_reportAttributes.requred(reportClass = reporter))
  delayedAssign('minmax', get_reportAttributes.minmax(reportClass = reporter))

  if(!is.null(attr(var, 'labels'))) {
    forbidden_attribute(attrname='labels',  attrvalue=attr(var, 'labels'))
  }
  if(!is.null(attr(var, 'levels'))) {
    forbidden_attribute(attrname='levels',  attrvalue=attr(var, 'levels'))
  }
  var_t<-get_typeClass(var)
  if(!is.null(attr(var, 'theoretical_min'))) {
    min_v<-attr(var, 'theoretical_min')
    min_t<-get_typeClass(min_v)
    if(min_t != var_t) {
      wrong_attribute_class(attrname='theoretical_min',  attrvalue=min_t, typeClass=var_t)
    } else {
      cases_min<-which(var < min_v)
      if(length(cases_min)>0) {
        minmax(case=cases_min, attrname='theoretical_min',  attrvalue=min_t)
      }
    }
  }
  if(!is.null(attr(var, 'theoretical_max'))) {
    max_v<-attr(var, 'theoretical_max')
    max_t<-get_typeClass(max_v)
    if(max_t != var_t) {
      wrong_attribute_class(attrname='theoretical_max',  attrvalue=max_t, typeClass=var_t)
    } else {
      cases_max<-which(var > max_v)
      if(length(cases_max)>0) {
        minmax(case=cases_min, attrname='theoretical_max',  attrvalue=min_t)
      }
    }
  }
  if(!is.null(attr(var, 'force_integers'))) {
    forbidden_attribute(attrname='force_integers',  attrvalue=attr(var, 'force_integers'))
  }
  if(!is.null(attr(var, 'required'))) {
    nas<-which(is.na(var))
    if(length(nas)>0) {
      requred(case=nas)
    }
  }
  if(!is.null(attr(var, 'limit_to_labels'))) {
    forbidden_attribute(attrname='limit_to_labels',  attrvalue=attr(var, 'limit_to_labels'))
  }
}

validateAttributes.POSIXct<-function(var, reporter){
  delayedAssign('forbidden_attribute', get_reportAttributes.forbidden_attribute(reportClass = reporter, typeClass='T'))
  delayedAssign('wrong_attribute_class', get_reportAttributes.wrong_attribute_class(reportClass = reporter))
  delayedAssign('requred', get_reportAttributes.requred(reportClass = reporter))
  delayedAssign('minmax', get_reportAttributes.minmax(reportClass = reporter))


  if(!is.null(attr(var, 'labels'))) {
    forbidden_attribute(attrname='labels',  attrvalue=attr(var, 'labels'))
  }
  if(!is.null(attr(var, 'levels'))) {
    forbidden_attribute(attrname='levels',  attrvalue=attr(var, 'levels'))
  }
  var_t<-get_typeClass(var)
  if(!is.null(attr(var, 'theoretical_min'))) {
    min_v<-attr(var, 'theoretical_min')
    min_t<-get_typeClass(min_v)
    if(min_t != var_t) {
      wrong_attribute_class(attrname='theoretical_min',  attrvalue=min_t, typeClass=var_t)
    } else {
      cases_min<-which(var < min_v)
      if(length(cases_min)>0) {
        minmax(case=cases_min, attrname='theoretical_min',  attrvalue=min_t)
      }
    }
  }
  if(!is.null(attr(var, 'theoretical_max'))) {
    max_v<-attr(var, 'theoretical_max')
    max_t<-get_typeClass(max_v)
    if(max_t != var_t) {
      wrong_attribute_class(attrname='theoretical_max',  attrvalue=max_t, typeClass=var_t)
    } else {
      cases_max<-which(var > max_v)
      if(length(cases_max)>0) {
        minmax(case=cases_max, attrname='theoretical_max',  attrvalue=max_t)
      }
    }
  }
  if(!is.null(attr(var, 'force_integers'))) {
    forbidden_attribute(attrname='force_integers',  attrvalue=attr(var, 'force_integers'))
  }
  if(!is.null(attr(var, 'required'))) {
    nas<-which(is.na(var))
    if(length(nas)>0) {
      requred(case=nas)
    }
  }
  if(!is.null(attr(var, 'limit_to_labels'))) {
    forbidden_attribute(attrname='limit_to_labels',  attrvalue=attr(var, 'limit_to_labels'))
  }
}


validateAttributes.factor<-function(var, reporter){
  delayedAssign('forbidden_attribute', get_reportAttributes.forbidden_attribute(reportClass = reporter, typeClass='F'))
  delayedAssign('missing_attribute', get_reportAttributes.missing_attribute(reportClass = reporter, typeClass='F'))
  delayedAssign('invalid_attribute', get_reportAttributes.invalid_attribute(reportClass = reporter, typeClass='F'))
  delayedAssign('wrong_attribute_class', get_reportAttributes.wrong_attribute_class(reportClass = reporter))

  delayedAssign('requred', get_reportAttributes.requred(reportClass = reporter))
  delayedAssign('not_found_minmax', get_reportAttributes.not_found_minmax(reportClass = reporter))
  delayedAssign('minmax', get_reportAttributes.minmax(reportClass = reporter))


  if(!is.null(attr(var, 'labels'))) {
    forbidden_attribute(attrname='labels',  attrvalue=attr(var, 'labels'))
  }
  if(is.null(attr(var, 'levels'))) {
    missing_attribute(attrname='levels')
  } else {
    attr_v<-attr(var, 'levels')
    if(!checkmate::check_character(attr_v)) {
      invalid_attribute(attrname='levels',  attrvalue=attr(var, 'levels'))
    }
  }
  var_t<-get_typeClass(var)
  if(!is.null(attr(var, 'theoretical_min'))) {
    min_v<-attr(var, 'theoretical_min')
    min_t<-get_typeClass(var)
    cases_min<-NULL
    if(length(min_t)!=1) {
      wrong_attribute_class(attrname='theoretical_min',  attrvalue=min_t, typeClass=var_t)
    } else if (min_t == 'S') {
      labels<-attr(var, 'levels')
      if(!min_t %in% labels) {
        not_found_minmax(attrname='theoretical_min',  attrvalue=min_t)
      }
      level<-which(labels == min_t)
      cases_min<-which(as.integer(var)<level)
    } else if (any(min_t %in% c('I', 'N'))) {

      if(!min_t %in% seq_along(labels)) {
        not_found_minmax(attrname='theoretical_min',  attrvalue=min_t)
      }
      cases_min<-which(as.integer(var)<min_t)
    } else {
      wrong_attribute_class(attrname='theoretical_min',  attrvalue=min_t, typeClass=var_t)
    }
    if(!is.null(cases_min)) {
      if(length(cases_min)>0) {
        minmax(case=cases_min, attrname='theoretical_min',  attrvalue=min_t)
      }
    }
  }
  if(!is.null(attr(var, 'theoretical_max'))) {
    max_v<-attr(var, 'theoretical_max')
    max_t<-get_typeClass(var)
    cases_max<-NULL
    if(length(max_t)!=1) {
      wrong_attribute_class(attrname='theoretical_max',  attrvalue=max_t, typeClass=var_t)
    } else if (max_t == 'S') {
      labels<-attr(var, 'levels')
      if(!max_t %in% labels) {
        not_found_minmax(attrname='theoretical_max',  attrvalue=mAx_t)
      }
      level<-which(labels == max_t)
      cases_max<-which(as.integer(var)>level)
    } else if (any(max_t %in% c('I', 'N'))) {

      if(!max_t %in% seq_along(labels)) {
        not_found_minmax(attrname='theoretical_max',  attrvalue=max_t)
      }
      cases_max<-which(as.integer(var)<max_t)
    } else {
      wrong_attribute_class(attrname='theoretical_max',  attrvalue=max_t, typeClass=var_t)
    }
    if(!is.null(cases_max)) {
      if(length(cases_max)>0) {
        minmax(case=cases_max, attrname='theoretical_max',  attrvalue=max_t)
      }
    }
  }
  if(!is.null(attr(var, 'force_integers'))) {
    attr_v<-attr(var, 'force_integers')
    if(!checkmate::check_flag(attr_v)) {
      invalid_attribute(attrname='theoretical_max',  attrvalue=max_t)
    }
  }
  if(!is.null(attr(var, 'required'))) {
    nas<-which(is.na(var))
    if(length(nas)>0) {
      required(case=nas)
    }
  }
  if(!is.null(attr(var, 'limit_to_labels'))) {
    forbidden_attribute(attrname='limit_to_labels',  attrvalue=attr(var, 'limit_to_labels'))
  }
}


validateAttributes.integer<-function(var, reporter){
  delayedAssign('forbidden_attribute', get_reportAttributes.forbidden_attribute(reportClass = reporter, typeClass='I'))
  delayedAssign('wrong_attribute_class', get_reportAttributes.wrong_attribute_class(reportClass = reporter, typeCLass='I'))
  delayedAssign('invalid_attribute', get_reportAttributes.invalid_attribute(reportClass = reporter, typeClass='I'))

  delayedAssign('requred', get_reportAttributes.requred(reportClass = reporter))
  delayedAssign('not_found_minmax', get_reportAttributes.not_found_minmax(reportClass = reporter))
  delayedAssign('minmax', get_reportAttributes.minmax(reportClass = reporter))

  if(!is.null(attr(var, 'labels'))) {
    forbidden_attribute(attrname='labels',  attrvalue=attr(var, 'labels'))
  }
  if(!is.null(attr(var, 'levels'))) {
    forbidden_attribute(attrname='levels',  attrvalue=attr(var, 'levels'))
  }
  if(!is.null(attr(var, 'theoretical_min'))) {
    min_v<-attr(var, 'theoretical_min')
    min_t<-get_typeClass(var)
    cases_min<-NULL
    if(length(min_t)!=1) {
      wrong_attribute_class(attrname='theoretical_min',  attrvalue=min_t)
    } else if (any(min_t %in% c('I', 'N'))) {
      if(!min_t %in% seq_along(labels)) {
        not_found_minmax(attrname='theoretical_min',  attrvalue=min_t)
      }
      cases_min<-which(var<min_v)
    } else {
      wrong_attribute_class(attrname='theoretical_min',  attrvalue=min_t)
    }
    if(!is.null(cases_min)) {
      if(length(cases_min)>0) {
        minmax(case=cases_min, attrname='theoretical_min',  attrvalue=min_t)
      }
    }
  }
  if(!is.null(attr(var, 'theoretical_max'))) {
    max_v<-attr(var, 'theoretical_max')
    max_t<-get_typeClass(var)
    cases_max<-NULL
    if(length(max_t)!=1) {
      wrong_attribute_class(attrname='theoretical_max',  attrvalue=max_t)
    } else if (any(max_t %in% c('I', 'N'))) {
      if(!max_t %in% seq_along(labels)) {
        not_found_minmax(attrname='theoretical_max',  attrvalue=max_t)
      }
      cases_max<-which(var>max_v)
    } else {
      wrong_attribute_class(attrname='theoretical_max',  attrvalue=max_t)
    }
    if(!is.null(cases_max)) {
      if(length(cases_max)>0) {
        minmax(case=cases_max, attrname='theoretical_max',  attrvalue=max_t)
      }
    }
  }
  if(!is.null(attr(var, 'force_integers'))) {
    attr_val<-attr(var, 'force_integers')
    if(!checkmate::check_flag(attr_val)) {
      invalid_attribute(attrname='force_integers',  attrvalue=attr_val)
    } else if (attr_val!=TRUE) {
      invalid_attribute(attrname='force_integers',  attrvalue=attr_val)
    }
  }
  if(!is.null(attr(var, 'required'))) {
    if(!checkmate::check_flag(attr(var, 'required'))) {
      invalid_attribute(attrname='required',  attrvalue=attr(var, 'required'))
    }
    nas<-which(is.na(var))
    if(length(nas)>0) {
      requred(case=nas)
    }
  }
  if(!is.null(attr(var, 'limit_to_labels'))) {
    attr_val<-attr(var, 'limit_to_labels')
    if(!checkmate::check_flag(attr_val)) {
      invalid_attribute(attrname='limit_to_labels',  attrvalue=attr_val)
    } else if (attr_val!=TRUE) {
      invalid_attribute(attrname='limit_to_labels',  attrvalue=attr_val)
    }
  }
}

validateAttributes.numeric<-function(var, reporter){
  delayedAssign('forbidden_attribute', get_reportAttributes.forbidden_attribute(reportClass = reporter, typeClass='N'))
  delayedAssign('wrong_attribute_class', get_reportAttributes.wrong_attribute_class(reportClass = reporter, typeCLass='N'))
  delayedAssign('invalid_attribute', get_reportAttributes.invalid_attribute(reportClass = reporter, typeClass='N'))

  delayedAssign('requred', get_reportAttributes.requred(reportClass = reporter))
  delayedAssign('not_found_minmax', get_reportAttributes.not_found_minmax(reportClass = reporter))
  delayedAssign('minmax', get_reportAttributes.minmax(reportClass = reporter))
  delayedAssign('force_integers', get_reportAttributes.force_integers(reportClass = reporter))



  if(!is.null(attr(var, 'labels'))) {
    forbidden_attribute(attrname='labels',  attrvalue=attr(var, 'labels'))
  }
  if(!is.null(attr(var, 'levels'))) {
    forbidden_attribute(attrname='levels',  attrvalue=attr(var, 'levels'))
  }
  if(!is.null(attr(var, 'theoretical_min'))) {
    min_v<-attr(var, 'theoretical_min')
    min_t<-get_typeClass(var)
    cases_min<-NULL
    if(length(min_t)!=1) {
      wrong_attribute_class(attrname='theoretical_min',  attrvalue=min_t)
    } else if (any(min_t %in% c('I', 'N'))) {
      if(!min_t %in% seq_along(labels)) {
        not_found_minmax(attrname='theoretical_min',  attrvalue=min_t)
      }
      cases_min<-which(var<min_t)
    } else {
      wrong_attribute_class(attrname='theoretical_min',  attrvalue=min_t)
    }
    if(!is.null(cases_min)) {
      if(length(cases_min)>0) {
        minmax(case=cases_min, attrname='theoretical_min',  attrvalue=min_t)
      }
    }
  }
  if(!is.null(attr(var, 'theoretical_max'))) {
    max_v<-attr(var, 'theoretical_max')
    max_t<-get_typeClass(var)
    cases_max<-NULL
    if(length(max_t)!=1) {
      wrong_attribute_class(attrname='theoretical_max',  attrvalue=max_t)
    } else if (any(max_t %in% c('I', 'N'))) {
      if(!max_t %in% seq_along(labels)) {
        not_found_minmax(attrname='theoretical_max',  attrvalue=max_t)
      }
      cases_max<-which(var>max_t)
    } else {
      wrong_attribute_class(attrname='theoretical_max',  attrvalue=max_t)
    }
    if(!is.null(cases_max)) {
      if(length(cases_max)>0) {
        minmax(case=cases_max, attrname='theoretical_max',  attrvalue=max_t)
      }
    }
  }

  if(!is.null(attr(var, 'force_integers'))) {
    attr_val<-attr(var, 'force_integers')
    if(!checkmate::check_flag(attr_val)) {
      invalid_attribute(attrname='force_integers',  attrvalue=attr_val)
    } else if (attr_val==TRUE) {
      var_int<-as.integer(var)
      num_nint<-which(var_int != var)
      if(length(num_nint)>0) {
        for(i in num_nint) {
          force_integers(value=var[[i]], case=i)
        }
      }
    }
  }
  if(!is.null(attr(var, 'required'))) {
    if(!checkmate::check_flag(attr(var, 'required'))) {
      invalid_attribute(attrname='required',  attrvalue=attr(var, 'required'))
    }
    nas<-which(is.na(var))
    if(length(nas)>0) {
      requred(case=nas)
    }
  }
  if(!is.null(attr(var, 'limit_to_labels'))) {
    attr_val<-attr(var, 'limit_to_labels')
    if(!checkmate::check_flag(attr_val)) {
      invalid_attribute(attrname='limit_to_labels',  attrvalue=attr_val)
    } else if (attr_val!=TRUE) {
      invalid_attribute(attrname='limit_to_labels',  attrvalue=attr_val)
    }
  }
}

validateAttributes.labelled<-function(var, reporter){
  delayedAssign('missing_attribute', get_reportAttributes.missing_attribute(reportClass = reporter, typeClass='L'))
  delayedAssign('forbidden_attribute', get_reportAttributes.forbidden_attribute(reportClass = reporter, typeClass='L'))
  delayedAssign('wrong_attribute_class', get_reportAttributes.wrong_attribute_class(reportClass = reporter, typeCLass='L'))
  delayedAssign('invalid_attribute', get_reportAttributes.invalid_attribute(reportClass = reporter, typeClass='L'))

  delayedAssign('requred', get_reportAttributes.requred(reportClass = reporter))
  delayedAssign('not_found_minmax', get_reportAttributes.not_found_minmax(reportClass = reporter))
  delayedAssign('minmax', get_reportAttributes.minmax(reportClass = reporter))
  delayedAssign('force_integers', get_reportAttributes.force_integers(reportClass = reporter))
  delayedAssign('limit_to_labels', get_reportAttributes.limit_to_labels(reportClass = reporter))



  if(is.null(attr(var, 'labels'))) {
    missing_attribute(attrname='labels')
  } else {
    attr_v<-attr(var, 'labels')
    attr_typeClass <-get_typeClass(attr_v)
    if(attr_typeClass!='N')
    {
      invalid_attribute(attrname='labels',  attrvalue=attr(var, 'labels'))
    }
  }
  if(!is.null(attr(var, 'levels'))) {
    forbidden_attribute(attrname='levels',  attrvalue=attr(var, 'levels'))
  }
  var_t<-get_typeClass(var)
  if(!is.null(attr(var, 'theoretical_min'))) {
    min_v<-attr(var, 'theoretical_min')
    min_t<-get_typeClass(var)
    cases_min<-NULL
    if(length(min_t)!=1) {
      wrong_attribute_class(attrname='theoretical_min',  attrvalue=min_t, typeCLass=var_t)
    } else if (min_t == 'S') {
      labels<-attr(var, 'levels')
      if(!min_t %in% names(labels)) {
        not_found_minmax(attrname='theoretical_min',  attrvalue=min_t)
      }
      level<-which(labels == min_t)
      cases_min<-which(as.integer(var)<level)
    } else if (any(min_t %in% c('I', 'N'))) {

      if(!min_t %in% seq_along(labels)) {
        not_found_minmax(attrname='theoretical_min',  attrvalue=min_t)
      }
      cases_min<-which(as.integer(var)<min_t)
    } else {
      wrong_attribute_class(attrname='theoretical_min',  attrvalue=min_t, typeCLass=var_t)
    }
    if(!is.null(cases_min)) {
      if(length(cases_min)>0) {
        minmax(case=cases_min, attrname='theoretical_min',  attrvalue=min_t)
      }
    }
  }
  if(!is.null(attr(var, 'theoretical_max'))) {
    max_v<-attr(var, 'theoretical_max')
    max_t<-get_typeClass(var)
    cases_max<-NULL
    if(length(max_t)!=1) {
      wrong_attribute_class(attrname='theoretical_max',  attrvalue=max_t, typeCLass=var_t)
    } else if (max_t == 'S') {
      labels<-attr(var, 'levels')
      if(!max_t %in% labels) {
        not_found_minmax(attrname='theoretical_max',  attrvalue=mAx_t)
      }
      level<-which(labels == max_t)
      cases_max<-which(as.integer(var)>level)
    } else if (any(max_t %in% c('I', 'N'))) {

      if(!max_t %in% seq_along(labels)) {
        not_found_minmax(attrname='theoretical_max',  attrvalue=max_t)
      }
      cases_max<-which(as.integer(var)<max_t)
    } else {
      wrong_attribute_class(attrname='theoretical_max',  attrvalue=max_t, typeCLass=var_t)
    }
    if(!is.null(cases_max)) {
      if(length(cases_max)>0) {
        minmax(case=cases_max, attrname='theoretical_max',  attrvalue=max_t)
      }
    }
  }
  if(!is.null(attr(var, 'force_integers'))) {
    attr_val<-attr(var, 'force_integers')
    if(!checkmate::check_flag(attr_val)) {
      invalid_attribute(attrname='force_integers',  attrvalue=attr_val)
    } else if (attr_val==TRUE) {
      var_int<-as.integer(var)
      num_nint<-which(var_int != var)
      if(length(num_nint)>0) {
        for(i in num_nint) {
          force_integers(value=var[[i]], case=i)
        }
      }
    }
  }
  if(!is.null(attr(var, 'required'))) {
    nas<-which(is.na(var))
    if(length(nas)>0) {
      requred(case=nas)
    }
  }
  if(!is.null(attr(var, 'limit_to_labels'))) {
    attr_val<-attr(var, 'limit_to_labels')
    if(!checkmate::check_flag(attr_val)) {
      invalid_attribute(attrname='limit_to_labels',  attrvalue=attr_val)
    }
    if(attr_val) {
      values<-unique(var)
      invalid_values<-which(values %in% attr_val)
      if(length(invalid_values)>0) {
        for(iv in invalid_values) {
          value<-values[[iv]]
          limit_to_labels(value=value, case = which(value==var))
        }
      }
    }
  }
}

