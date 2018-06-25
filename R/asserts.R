assert_typeClass<-function(typeClass) {
  checkmate::assert_string(typeClass)
  checkmate::assert_choice(typeClass, names(all_classTypes), null.ok = FALSE)
}
