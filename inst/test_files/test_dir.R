#| config(debug = T)
#| load_script("inst/test_files/test_dir/")
console::log("Loaded 2 R files and 1 JS file: " %+% (file_1 && file_2 && file_3))
