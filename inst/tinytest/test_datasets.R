library(tinytest)

# Dataset structure checks are intentionally strict and fast to guard package data API.
data(chasselas, package = "sapid")
expect_true(is.data.frame(chasselas))
expect_identical(nrow(chasselas), 1314L)
expect_identical(ncol(chasselas), 6L)
expect_identical(names(chasselas), c("date", "jury", "product", "session", "taste", "value"))
expect_true(inherits(chasselas$date, "POSIXct"))
expect_identical(typeof(chasselas$jury), "character")
expect_identical(typeof(chasselas$product), "character")
expect_identical(typeof(chasselas$session), "character")
expect_identical(typeof(chasselas$taste), "character")
expect_identical(typeof(chasselas$value), "double")
expect_false(anyNA(chasselas$date))
expect_false(anyNA(chasselas$jury))
expect_false(anyNA(chasselas$product))
expect_false(anyNA(chasselas$session))

data(concentration_afc, package = "sapid")
expect_true(is.data.frame(concentration_afc))
expect_identical(nrow(concentration_afc), 76L)
expect_identical(ncol(concentration_afc), 6L)
expect_identical(
  names(concentration_afc),
  c("concentration", "jury", "taste", "value", "afc_correct", "afc_total")
)
expect_identical(typeof(concentration_afc$concentration), "double")
expect_identical(typeof(concentration_afc$jury), "character")
expect_identical(typeof(concentration_afc$taste), "character")
expect_identical(typeof(concentration_afc$value), "double")
expect_identical(typeof(concentration_afc$afc_correct), "integer")
expect_identical(typeof(concentration_afc$afc_total), "integer")
expect_false(anyNA(concentration_afc$jury))
expect_false(anyNA(concentration_afc$taste))
expect_false(anyNA(concentration_afc$afc_correct))
expect_false(anyNA(concentration_afc$afc_total))

data(profiles, package = "sapid")
expect_true(is.data.frame(profiles))
expect_identical(nrow(profiles), 1665L)
expect_identical(ncol(profiles), 6L)
expect_identical(
  names(profiles),
  c("fraction", "session", "jury", "taste_original", "taste_harmonized", "value")
)
expect_identical(typeof(profiles$fraction), "character")
expect_identical(typeof(profiles$session), "character")
expect_identical(typeof(profiles$jury), "character")
expect_identical(typeof(profiles$taste_original), "character")
expect_identical(typeof(profiles$taste_harmonized), "character")
expect_identical(typeof(profiles$value), "double")
expect_false(anyNA(profiles$fraction))
expect_false(anyNA(profiles$session))
expect_false(anyNA(profiles$jury))

data(napping_coordinates, package = "sapid")
expect_true(is.data.frame(napping_coordinates))
expect_identical(nrow(napping_coordinates), 64L)
expect_true(ncol(napping_coordinates) >= 4L)
expect_identical(names(napping_coordinates)[1:2], c("fraction", "session"))
expect_false(anyNA(napping_coordinates$fraction))
expect_false(anyNA(napping_coordinates$session))

data(napping_descriptors, package = "sapid")
expect_true(is.data.frame(napping_descriptors))
expect_identical(nrow(napping_descriptors), 1146L)
expect_identical(ncol(napping_descriptors), 6L)
expect_identical(
  names(napping_descriptors),
  c(
    "fraction",
    "session",
    "jury",
    "taste_original",
    "taste_intermediate",
    "taste_harmonized"
  )
)
expect_false(anyNA(napping_descriptors$fraction))
expect_false(anyNA(napping_descriptors$session))
expect_false(anyNA(napping_descriptors$jury))

data(fractions, package = "sapid")
expect_true(is.data.frame(fractions))
expect_identical(nrow(fractions), 94L)
expect_identical(ncol(fractions), 3L)
expect_identical(names(fractions), c("parent", "label", "mass"))
expect_identical(typeof(fractions$parent), "character")
expect_identical(typeof(fractions$label), "character")
expect_identical(typeof(fractions$mass), "double")
expect_false(anyNA(fractions$label))

data(groups, package = "sapid")
expect_true(is.data.frame(groups))
expect_identical(nrow(groups), 54L)
expect_identical(ncol(groups), 2L)
expect_identical(names(groups), c("group", "rowname"))
expect_identical(typeof(groups$group), "character")
expect_false(anyNA(groups$group))
expect_false(anyNA(groups$rowname))

data(dictionary_generic, package = "sapid")
expect_true(is.data.frame(dictionary_generic))
expect_identical(nrow(dictionary_generic), 64L)
expect_identical(ncol(dictionary_generic), 3L)
expect_identical(
  names(dictionary_generic),
  c("original", "translated", "translated_simple")
)
expect_identical(typeof(dictionary_generic$original), "character")
expect_identical(typeof(dictionary_generic$translated), "character")
expect_identical(typeof(dictionary_generic$translated_simple), "character")
expect_false(anyNA(dictionary_generic$original))

data(dictionary_napping, package = "sapid")
expect_true(is.data.frame(dictionary_napping))
expect_identical(nrow(dictionary_napping), 45L)
expect_identical(ncol(dictionary_napping), 2L)
expect_identical(names(dictionary_napping), c("original", "translated"))
expect_identical(typeof(dictionary_napping$original), "character")
expect_identical(typeof(dictionary_napping$translated), "character")
expect_false(anyNA(dictionary_napping$original))

data(dictionary_specific, package = "sapid")
expect_true(is.data.frame(dictionary_specific))
expect_identical(nrow(dictionary_specific), 196L)
expect_identical(ncol(dictionary_specific), 3L)
expect_identical(
  names(dictionary_specific),
  c("original", "translated", "translated_simple")
)
expect_identical(typeof(dictionary_specific$original), "character")
expect_identical(typeof(dictionary_specific$translated), "character")
expect_identical(typeof(dictionary_specific$translated_simple), "character")
expect_false(anyNA(dictionary_specific$original))

