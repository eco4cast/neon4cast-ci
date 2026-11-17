# Names used inside duckdb queries: date_diff() is a duckdb SQL function that
# dbplyr passes through, and the columns are referenced inside it. Declaring
# them here keeps R CMD check quiet without hiding real undefined names
# elsewhere.
# `observation` appears inside fable model formulas, which fable evaluates
# against the data.
utils::globalVariables(c("date_diff", "datetime", "reference_datetime", "observation"))
