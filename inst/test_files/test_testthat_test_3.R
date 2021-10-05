# This file is used with `test_testthat_app_2.R`

#! load_script("test_testthat_test_3_dependency.R")
#! load_script("test_testthat_test_3_dependency_2.R")
console::log("This is test 3.")
console::log("If the loading works properly, you should see the message from the dependency files above.")
console::log("Running test...")
testthat::expect_equal(x, 10)
console::log("Done.")
