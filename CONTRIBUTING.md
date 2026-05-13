# Contributing to CAWET

Thank you for your interest in contributing to **CAWET**! We welcome feedback,
bug reports, and contributions to improve the package.

This guide outlines how to get involved and describes our contribution process
and coding standards.

Repository URL: [https://forge.inrae.fr/umr-g-eau/cawet](https://forge.inrae.fr/umr-g-eau/cawet)

---

## 📌 Before you start

- Please **open an issue** to discuss any new feature or bug fix before starting
development. This allows for discussion and coordination.
- Check existing issues and merge requests to avoid duplicating work.

---

## 💡 How to contribute

1. **Open an issue** on GitLab describing the bug, enhancement, or new feature.
Please provide enough context and, if applicable, a reproducible example.

2. Once the issue is **triaged and assigned** to you (or you are the assignee),
create a Merge Request (MR) and a new branch from the issue page and assign a reviewer.

3. **Make your changes** following the [tidyverse style guide](https://style.tidyverse.org/). Ensure you:
   - Add or update documentation using `roxygen2`
   - Add or update tests in `tests/testthat/`
   - Run `devtools::check()` and fix any issues

4. **Commit and Push your changes** to your branch:
   - Try to keep your commit as consistent as possible to simple tasks (feature, test, documentation...)
   - comment your commits using [Conventional Commit Specifications](https://www.conventionalcommits.org/en/v1.0.0/#summary)

7. **When the work is done, trigger a review**:
   - Remove the "Draft" state on your MR and eventually contact the
   reviewer for triggering the review
   - Respond to code reviews and adjust your MR as needed until it is approved and merged.

---

## 🎨 Coding style

We follow the [tidyverse style guide](https://style.tidyverse.org/)

Key points include:

- Use `snake_case` for object and function names.
- Use `<-` for assignment.
- Place spaces after commas, around operators, and after `#` in comments.
- Limit lines to 80 characters where possible.

You can use the [`styler`](https://styler.r-lib.org/) and
[`lintr`](https://lintr.r-lib.org/) packages to check and format your code.

---

## 🧪 Testing

All new features and bug fixes should include **unit tests** using the
[`testthat`](https://testthat.r-lib.org/) framework.

Add tests in `tests/testthat/`, ideally in files named after the function or
feature being tested.

---

## 📚 Documentation

- Use `roxygen2` to document exported functions.
- Run `devtools::document()` to update the `NAMESPACE` and `.Rd` files.
- Keep examples simple, minimal, and reproducible.

---

## 📦 Building and checking

Before submitting your merge request, please run: `devtools::check()`

Try to resolve all errors, warnings, and notes. Neither errors nor warnings are
accepted on CI check.

---

## ✅ Merge request checklist

- [ ] My code follows the tidyverse style guide.
- [ ] I have added or updated documentation.
- [ ] I have added or updated tests.
- [ ] The package passes `devtools::check()` without errors nor warnings.

---

## 📜 Code of Conduct

We are committed to providing a friendly and respectful environment.
By contributing, you agree to follow the
[Contributor Covenant](https://www.contributor-covenant.org/).

## Version release procedure

- [ ] List all issues linked to the milestone related to the version to release.
- [ ] Check that all issues are solved and assigned to the developer who solved it.
- [ ] Export the issues in CSV format and use an IA agent with the following prompt:

```
# Change log writer

You’re going to write the code of a NEWS.md file of an R package. The CSV contains the list of issues solved for the version to deploy. I’re going to create this NEWS.md by grouping the issues between sections like “New features”, “Bug fixes”, “Internal changes”… And for each issue give the title of the issue, the name of the developer assigned to it, and the issue number prefixed by a hash.
```

- [ ] Copy the result on the top of `NEWS.md` and add the release date to the version
- [ ] Check spell and URLs used in the package (See https://github.com/ThinkR-open/prepare-for-cran).
- [ ] Upgrade version number, create the version tag, and push it with `usethis::use_version(which = c("patch", "minor", "major", "dev")[2], push = TRUE)`.
- [ ] Create a MR from dev to main branch entitled "Release vX.Y.Z".
- [ ] Merge the MR and checkout the main branch.
- [ ] Create the release in gitlab with a tag on main
- [ ] If any new commit has been push on main branch, create a MR from main to dev branch entitled "Merge release vX.Y.Z back into dev" and merge it.
- [ ] Checkout dev branch and upgrade version number: with `usethis::use_version(which = c("patch", "minor", "major", "dev")[4], push = TRUE)`

---

Thanks again for helping make **CAWET** better!
