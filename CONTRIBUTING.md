# Contributing to SuperC
> Thanks for your interest in contributing!
The project is still evolving, so contributions of all kinds are welcome - from bug reports to compiler internals and documentation.

## Code of Conduct
Please note that this project has a [Code of Conduct](CODE_OF_CONDUCT.md). By participating, you agree to abide by its terms.

## Good first contributions
If you're new, consider:
- Fixing typos in documentation
- Adding small examples
- Writing better tests for existing features

## Ways to contribute
- [Compiler development](#compiler-development)
- [Documentation pages](#documentation-pages)
- [Testing](#testing)
- [Issues](#manual-testing)
  - Bug reports
  - Feature requests
  - Design discussions

# Compiler development
This is the most heavyweight part of the project.

## Quick start
```sh
# Clone
git clone https://github.com/bruneo32/superc.git
cd superc
# Build
make
# Run tests
make test
```

There are some files you might want to modify:
- `superc.h` - common declarations
- `parse.c` - language frontend
- `codegen.c` - language backend
- `main.c` - entry point, also calls the linker

## Usual workflow
- Clone repository
- Make changes
- Push changes to your fork
- Create a [pull request](<https://github.com/bruneo32/superc/pulls>)

## Pull request guidelines
- **Link** the related issue (if any)
- Clearly **describe** the changes
- Include **examples**, **code** snippets, or **screenshots** if helpful
- **Wait** for review

## Requirements for merging
- All tests must pass (`make test`)
- Follow existing code style
- No unnecessary complexity
- Major changes must be discussed first

## Notes
Tip for debugging with *gdb*.
File `~/.gdbinit`:
```
set disable-randomization on
set follow-fork-mode child
catch syscall exit
catch syscall exit_group
catch signal SIGSEGV
catch signal SIGABRT
break error
```

# Documentation pages
```sh
# Clone github pages brach
git clone -b gh-pages --single-branch https://github.com/bruneo32/superc.git superc_pages
cd superc_pages
```

- Read the [README.md](<https://github.com/bruneo32/superc/blob/gh-pages/README.md>) for full instructions.
- Modify or add [docs](<https://github.com/bruneo32/superc/blob/gh-pages/docs/>) in `docs/` folder.

### Test locally before pushing
The usual workflow when testing pages is:
```sh
eval "$(rbenv init -)" # ~/.bashrc
./_script/bootstrap
bundle exec jekyll serve
```

# Testing
Testing makes **SuperC** more stable, secure and reliable, preventing regressions.

## Manual testing
You can open an [issue](<https://github.com/bruneo32/superc/issues>), if you encounter any problem from a **typo** in the documentation to **bugs** while testing the compiler .

Make sure to:
- **Describe** the problem in detail.
- Use the correct **labels** (*bug*, *feature*, *documentation*, *...*).
- Add steps to **reproduce** the issue, **code** and/or **images** are appreciated.

> **Search for existing issue first, to avoid duplicating another issue!**

## Automated tests
Helps us making the compiler reliable by writing [automated tests](<https://www.geeksforgeeks.org/software-testing/automation-testing-software-testing>).

- The process is the same as [compiler development](#compiler-development). But modify or add tests in the `test/` folder.

```sh
# Run tests
make test
```

## Design discussions
Changes that affect language syntax or behavior should be discussed before implementation.

- Open an [issue](<https://github.com/bruneo32/superc/issues>) with a clear proposal
- Explain motivation and trade-offs
- Provide examples of usage, if possible, pseudo-assembly code or C-like code.
