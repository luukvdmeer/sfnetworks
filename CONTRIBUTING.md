# How to contribute?

We look very much forward to contributions to the package. This can be done in several ways:

- Create a pull request with a specific feature implementation or bug fix. Please direct them to the `develop` branch.
- Open an issue in the [issue tracker](https://github.com/luukvdmeer/sfnetworks/issues) to request a specific feature or report a bug. Please use the respective issue templates for this.
- Share more general ideas you have for the package by starting a new discussion in the [discussion room](https://github.com/luukvdmeer/sfnetworks/discussions), using the [ideas category](https://github.com/luukvdmeer/sfnetworks/discussions?discussions_q=category%3AIdeas).
- Share the experiences you have with using the package in your work by starting a new discussion in the [discussion room](https://github.com/luukvdmeer/sfnetworks/discussions), using the [show and tell category](https://github.com/luukvdmeer/sfnetworks/discussions?discussions_q=category%3A%22Show+and+tell%22). Sharing this will help the package developers to see how the package is used and improve things accordingly, and other users to learn more about the package and get inspiration for their work!
- Ask questions by starting a new discussion in the [discussion room](https://github.com/luukvdmeer/sfnetworks/discussions), using the [Q&A category](https://github.com/luukvdmeer/sfnetworks/discussions?discussions_q=category%3AQ%26A), or [StackOverflow](https://stackoverflow.com/questions/tagged/sfnetwork) with the sfnetwork tag. Asking your questions openly helps other users that struggle with the same problems! 

## Code style

We strive to follow the [tidyverse styleguide](https://style.tidyverse.org/) in the source code of the package. An exception to that is the assignment operator: we use `=` instead of `<-` (see [here](https://github.com/Robinlovelace/geocompr/issues/319) for some reasons why).

## Structured commit messages

When commiting changes with `git commit` we try to use structured commit messages, adapted from https://www.conventionalcommits.org/. The first line of commit message should have the following format:

```
<type>: <summary>
```

The summary should be short (preferably < 50 characters), starting with an upper case, and written in present tense. If the commit references a specific issue, include `Refs #<issue number>` in the summary. If the issue is a bug report, you may also use `Fix #<issue number>` such that the issue gets closed automatically.

The type should be one of the defined types listed below. If you feel artistic, you can end the commit message with the emoji belonging to the type :sunglasses:.

- **feat**: Implementation of a new feature. `:gift:` :gift:
- **fix**: A bug fix. `:wrench:` :wrench:
- **style**: Changes to code formatting. No change to program logic. `:art:` :art:
- **refactor**: Changes to code which do not change behaviour, e.g. renaming variables or splitting functions. `:construction:` :construction:
- **docs**: Adding, removing or updating user documentation or to code comments. `:books:` :books:
- **logs**: Adding, removing or updating log messages. `:sound:` :sound:
- **test**: Adding, removing or updating tests. No changes to user code. `:test_tube:` :test_tube:
- **cicd**: Adding, removing or updating CI/CD workflows. No changes to user code. `:robot:` :robot:
- **deps**: Adding, removing or updating dependencies. `:couple:` :couple:
- **release**: Preparing a release, e.g. updating version numbers. `:bookmark` :bookmark:
- **repo**: Changes to the repository that do not involve code/documentation, e.g. adding templates or community files. `:package:` :package:

Example commit messages are:

```
git commit -m 'feat: Add bar parameter to foo(). Refs #10 :gift:'
git commit -m 'fix: Include type checking in foo(). Fix #12 :wrench:'
```

## Code of conduct

This project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.
