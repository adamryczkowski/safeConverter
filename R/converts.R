convertToCharacter<-function(var, reportClass=NULL) {
  UseMethod("convertToCharacter", var, reportClass=reportClass, flagValidate=flagValidate)
}

convertToCharacter.character<-function(var, reportClass=NULL) {
  return(var)
}

convertToCharacter.default<-function(var, reportClass=NULL) {
  return(as.character(var))
}

report_convertToCharacter.labelled<-function(reportClass, possible_problem_values) {
  if(!reportClass$type_exists('safeConverter.char2labelled')) {
    formatter<-function()
    reportClass$declare_type('safeConverter.char2labelled',
                             caption = "Character values that may be aliased with character representation of missing values when converting from labelled type.",
                             parlist='possible_problem_values', default_formatter_name='default',
                             defualt_formatter=formatter, flag_requires_cases=TRUE)
  }
}

convertToCharacter.labelled<-function(var, reportClass=NULL, prefix_NA="*", suffix_NA="*") {
  delayedAssign('formatter', read.table(text = "a b  \n 1 2", header=TRUE))
  browser()
  if(get_limit_to_labels(var)) {
    out<-as.character(haven::as_factor(var))
  } else {
    out<-as.character(as.numeric(var))
  }
  nas<-which(is.na(var))
  if(length(nas)>0) {
    where_na_tags<-which(!is.na(haven::na_tag(var)))
    if(length(where_na_tags)>0) {
      na_tags<-haven::na_tag(var)[where_na_tags]
      labels<-attr(var, 'labels')
      labelled_na_tags<-labels[!is.na(haven::na_tag(labels))]
      labelled_na_tags<-setNames(names(labelled_na_tags), haven::na_tag(labelled_na_tags))
      labelled_na_tags<-as.character(labelled_na_tags[na_tags])
      labelled_na_tags[is.na(labelled_na_tags)]<-na_tags[is.na(labelled_na_tags)]
      tags_to_include<-paste0(prefix_NA, "NA(", labelled_na_tags, ")", suffix_NA)
      for(tag_to_include in unique(tags_to_include)) {
        if(sum(var==tag_to_include)>0) {
          reportClass$add_element(type=)
        }
      }
      out[where_na_tags]<-paste0(prefix_NA, "NA(", labelled_na_tags, ")", suffix_NA)
    }
    where_na_tags<-which(is.na(haven::na_tag(var)) & is.na(var))
    if(length(where_na_tags)>0) {
      labels<-attr(var, 'labels')
      na_label<-which(is.na(labels) & is.na(haven::na_tag(labels)))
      if(is.na(na_label)) {
        out[where_na_tags]<-paste0(prefix_NA, "NA(", names(na_label), ")", suffix_NA)
      } else {
        out[where_na_tags]<-paste0(prefix_NA, "NA", suffix_NA)
      }
    }
  }
  return(out)
}



 # var <- labelled::labelled(c(1:3, haven::tagged_na("a", "a", "b", "c", "z", "a", "c"), NA, NA, 4:1),
 #                         c("Agreement" = 1, "Disagreement" = 4, "First" = haven::tagged_na("c"),
 #                           "Refused" = haven::tagged_na("a"), "Not home" = haven::tagged_na("z"), "Just don't know"=NA))
