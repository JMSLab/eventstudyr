# eventstudyr

## Development Workflow

1. Create a function using `usethis::use_r("functionName")`
2. Add `if(fail_condition){stop("message here")}` or `if(fial_condition){warning("message here")}` statements that perform checks on all of the functions' arguments and throw an appropriate error or warning
3. Use `devtools::load_all()` (`Ctrl + Shift + L` on Windows) to informally test drive the function
4. Write documentation for the function using the language from the [Design Document](https://github.com/JMSLab/EventStudyR/blob/105846629de1f1979eca01c8b6809249a4111199/issue1/DesignDocument.pdf). Create a working example if the function is going to be user facing (this will also help you to run informal tests more quickly)
5. Recompile function documentation using `devtools::document()` (`Ctrl + Shift + D on Windows`) to make sure the documentation is up to date
6. Run `usethis::use_test("functionName")` (should use the same functionName as in step 1). Add tests
7. Run the tests using `devtools::test()` (`Ctrl + Shift + T` on Windows) to make sure that all of the tests are passing, addressing any warnings and errors
8. Run `devtools::check()` (`Ctrl + Shift + E` on Windows) and address any notes, errors, or warnings
9. Commit changes
10. Push your code **if it is passing all tests**. Address any warnings/errors
11. When you open a pull request, uncomment `.github/workflows/R-CMD-check.yaml` to check your code, and then comment out again. Do the
 same before you are ready to wrap-up the pull request to ensure that the changes during the pull did not introduce any bugs.
