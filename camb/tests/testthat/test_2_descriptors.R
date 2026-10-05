context("Testing Descriptor Generation")

test_that("GeneratePadelDescriptors provides outputs consistent with reference outputs", {
  descriptor.types <- c("2D") 
  descriptors <- GeneratePadelDescriptors(standardised.file = "standardised.sdf", types=descriptor.types, threads = 1)
  descriptors <- RemoveStandardisedPrefix(descriptors)
  reference <- readRDS("reference_descriptors.rds")

  # This reference was serialized with factor columns under an older R
  # default. Compare the values while allowing R's factor/character/numeric
  # representation to follow the current runtime.
  for (n in names(reference)) {
    expected <- reference[[n]]
    actual <- descriptors[[n]]
    if (is.factor(expected) && !is.factor(actual)) {
      expected <- if (is.numeric(actual)) as.numeric(as.character(expected)) else as.character(expected)
    }
    if (is.factor(actual) && !is.factor(expected)) {
      actual <- if (is.numeric(expected)) as.numeric(as.character(actual)) else as.character(actual)
    }
    reference[[n]] <- expected
    descriptors[[n]] <- actual
  }

  if (!isTRUE(all.equal(reference, descriptors))) {
    class_mismatches <- names(reference)[!vapply(names(reference), function(n) identical(class(reference[[n]]), class(descriptors[[n]])), logical(1))]
    value_mismatches <- names(reference)[!vapply(names(reference), function(n) isTRUE(all.equal(reference[[n]], descriptors[[n]])), logical(1))]
    cat("Descriptor diagnostic: dimensions reference/current =", paste(dim(reference), collapse = "x"), "/", paste(dim(descriptors), collapse = "x"), "\n")
    cat("Descriptor diagnostic: class-mismatched columns =", paste(class_mismatches, collapse = ", "), "\n")
    cat("Descriptor diagnostic: value-mismatched columns =", length(value_mismatches), "\n")
    cat("Descriptor diagnostic: value mismatches outside class changes =", paste(head(setdiff(value_mismatches, class_mismatches), 50), collapse = ", "), "\n")
    for (n in head(class_mismatches, 12)) {
      cat("Class diagnostic", n, ": reference", paste(class(reference[[n]]), collapse = "/"), "current", paste(class(descriptors[[n]]), collapse = "/"), "\n")
      cat("  reference head:", paste(as.character(head(reference[[n]], 4)), collapse = " | "), "\n")
      cat("  current head:  ", paste(as.character(head(descriptors[[n]], 4)), collapse = " | "), "\n")
      if (is.factor(reference[[n]]) || is.factor(descriptors[[n]])) {
        cat("  reference levels:", paste(head(levels(reference[[n]]), 6), collapse = " | "), "\n")
        cat("  current levels:  ", paste(head(levels(descriptors[[n]]), 6), collapse = " | "), "\n")
      }
    }
    for (n in head(setdiff(value_mismatches, class_mismatches), 12)) {
      cat("Value diagnostic", n, ": reference", paste(as.character(head(reference[[n]], 4)), collapse = " | "), "current", paste(as.character(head(descriptors[[n]], 4)), collapse = " | "), "\n")
      if (is.numeric(reference[[n]]) && is.numeric(descriptors[[n]])) {
        differing_rows <- which(reference[[n]] != descriptors[[n]])
        cat("  differing row count:", length(differing_rows), "first rows:", paste(head(differing_rows, 10), collapse = ", "), "\n")
        if (length(differing_rows)) {
          rows <- head(differing_rows, 10)
          cat("  differing values:", paste(paste0(rows, ":", reference$Name[rows], "=", reference[[n]][rows], "->", descriptors[[n]][rows]), collapse = " | "), "\n")
        }
      }
    }
  }
  expect_equal(reference, descriptors)
})
