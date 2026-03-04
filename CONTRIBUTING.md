# Contributing to PostgREST

## Issues

For questions on how to use PostgREST, please use
[GitHub discussions](https://github.com/PostgREST/postgrest/discussions).

### Reporting an Issue

* Make sure you test against the latest [stable release](https://github.com/PostgREST/postgrest/releases/latest)
  and also against the latest [devel release](https://github.com/PostgREST/postgrest/releases/tag/devel).
  It is possible we already fixed the bug you're experiencing.

* Provide steps to reproduce the issue, including your OS version and
  the specific database schema that you are using.

* Please include SQL logs for issues involving runtime problems. To obtain logs first
  [enable logging all statements](http://www.microhowto.info/howto/log_all_queries_to_a_postgresql_server.html),
  then [find your logs](http://blog.endpoint.com/2014/11/dear-postgresql-where-are-my-logs.html).

* If your database schema has changed while the PostgREST server is running,
  [send the server a `SIGUSR1` signal](http://postgrest.org/en/latest/admin.html#schema-reloading) or restart it to ensure the schema cache
  is not stale. This sometimes fixes apparent bugs.

## Code

We have a fully nix-based development environment with many tools for a smooth development workflow available.
Check the [development docs](https://github.com/PostgREST/postgrest/blob/main/nix/README.md) on how to set it up and use it.

* All contributions must pass the tests before being merged. When
  you create a pull request your code will automatically be tested.

* All fixes or features must have a test proving the improvement.

* All code must also pass a [linter](http://community.haskell.org/~ndm/hlint/) and [styler](https://github.com/jaspervdj/stylish-haskell)
  with no warnings. This helps enforce a uniform style for all committers. Continuous integration will check this as well on every
  pull request. There are useful tools in the nix-shell that help with checking this locally. You can run `postgrest-check` to do this manually but
  we recommend adding it to `.git/hooks/pre-commit` as `nix-shell --run postgrest-check` to automatically check this before doing a commit.

### Running Tests

For instructions on running tests, see the [development docs](https://github.com/PostgREST/postgrest/blob/main/nix/README.md#testing).

### Structuring commits in pull requests

To simplify reviews, make it easy to split pull requests if deemed necessary, and to maintain clean and meaningful history of changes, you will be asked to update your PR if it does not follow the below rules:

* It must be possible to merge the PR branch into target using `git merge --ff-only`, ie. the source branch must be rebased on top of target.
* No merge commits in the source branch.
* All commits in the source branch must be self contained, meaning: it should be possible to treat each commit as a separate PR.
* Commits in the source branch must contain only related changes (related means the changes target a single problem/goal). For example, any refactorings should be isolated from the actual change implementation into separate commits.
* Tests, documentation, and changelog updates should be contained in the same commits as the actual code changes they relate to. An exception to this rule is when test or documentation changes are made in separate PR.
* Commit messages must be prefixed with one of the prefixes defined in [the list used by commit verification scripts](https://github.com/PostgREST/postgrest/blob/main/nix/tools/gitTools.nix#L11).
* Commit messages should contain a longer description of the purpose of the changes contained in the commit and, for non-trivial changes, a description of the changes themselves.

## AI Policy

We adhere to [Gentoo's AI policy](https://wiki.gentoo.org/wiki/Project:Council/AI_policy):

> It is expressly forbidden to contribute [...] any content that has been created with the assistance of Natural Language Processing artificial intelligence tools. This motion can be revisited, should a case been made over such a tool that does not pose copyright, ethical and quality concerns.

### Rationale

> 1. Copyright concerns. At this point, the regulations concerning copyright of generated contents are still emerging worldwide. Using such material could pose a danger of copyright violations, but it could also weaken Gentoo claims to copyright and void the guarantees given by copyleft licensing.
> 2. Quality concerns. Popular LLMs are really great at generating plausibly looking, but meaningless content. They are capable of providing good assistance if you are careful enough, but we can't really rely on that. At this point, they pose both the risk of lowering the quality of Gentoo projects, and of requiring an unfair human effort from developers and users to review contributions and detect the mistakes resulting from the use of AI.
> 3. Ethical concerns. The business side of AI boom is creating serious ethical concerns. Among them:
>   + Commercial AI projects are frequently indulging in blatant copyright violations to train their models.
>   + Their operations are causing concerns about the huge use of energy and water.
>   + The advertising and use of AI models has caused a significant harm to employees and reduction of service quality.
>   + LLMs have been empowering all kinds of spam and scam efforts.
