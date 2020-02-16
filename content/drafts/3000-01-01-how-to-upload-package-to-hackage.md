---
title: How to Upload Package to Hackage
description: todo
tags: haskell, hackage
tableOfContents: true
---

Few months ago I decided to learn [Haskell][web:haskell] and from very first pieces of _Hello World_ like code snippets I progressed to writting my very first real world utility called [Headroom][github:headroom] (more about it in next blog post). So now, it's available on [Github][web:github], but it's more common for Haskell projects to be also available on [Hackage][web:hackage]. In this blog post, I'll summary my journey and write down all important steps, both as reminder for myself and hopefully as help for someone else.

<!-- MORE -->

# Prerequisities
Before uploading the package to Hackage, it's necessary to check that the package contains all required configuration, metadata and that it builds well and passes tests.

## Project Configuration
Your package needs to contain some metadata configuration that are required by Hackage, so now it's the time to fill them in to either `package.yaml` or `.cabal` file. You can easily check what's missing by following command:

```txt
$ cabal check
```

Then just go through all the error reports and fix them. This step will make sure that Hackage will accept your package.

## Version Bounds
Every package upload to Hackage must have correctly set _version bounds_ for its dependencies. If you use [hpack][hackage:hpack] instead of `.cabal` file for your package configuration, then the generated `.cabal` file will miss these version restrictions. However, you can set this by adding the [pvp-bounds][web:stack/pvp-bounds] option to `stack.yaml`:

```yaml
pvp-bounds: both
```

Then when you pack the distribution archive using `stack sdist`, the generated `.cabal` file will contain correct version bounds, based on the _Stackage_ version you choose, as seen in snippet below:

```yaml
build-depends:
    aeson >=1.4.6.0 && <1.5,
    base >=4.7 && <5,
    either >=5.0.1.1 && <5.1,
    file-embed >=0.0.11.1 && <0.1,
    lens >=4.17.1 && <4.18,
    mustache >=2.3.1 && <2.4,
```

## Haddock Documentation
You should document all the public API using the [Haddock][web:haddock] tool, it will help potential users with orientation in your codebase. Also consider adding some example code snippets, you can also make sure they will compile using the awesome [Doctest][hackage:doctest] tool. Example of such Haddock comment with Doctest example is below:

```haskell
-- | Parses 'License' from the raw string representation, formatted as
-- @licenseType:fileType@.
--
-- >>> parseLicense "bsd3:haskell"
-- Just (License BSD3 Haskell)
parseLicense :: Text          -- ^ raw string representation
             -> Maybe License -- ^ parsed 'License'
```

## Setup CI
This step is not necessary, but nice to have, as it will make sure your project is building and all tests are passing, so you won't upload broken stuff to Hackage. There are many CI tools to choose from, following steps describe how to setup [Travis CI][web:travis].

As a first step, you need Travis account. Follow [these steps][web:travis/tutorial] describing how to do that. In order to let Travis run your build, you need to add appropriate `.travis.yml` configuration file to your project root. If you use [Stack][web:stack] tool to build your project, you can check [the Stack documentation][web:stack/travis] with example configuration for Travis, or see the [actual configuration file][github:headroom/.travis.yml] I use for one of my projects.

# Hackage User Account
Now you should have your package ready for upload to Hackage. To do that, you need to register user account and ask Hackage trustees to add you to group with upload rights (this seems to be some kind of anti-spam protection).

You can [register new account here][web:hackage/register], shortly after you'll receive e-mail with confirmation link. When you're done with this, you need to send e-mail to Hackage trustees, asking them to add you to uploader group. In my case it took one or two days before I got any answer.

# Upload to Hackage
todo

## Package Candidate
todo

## Final Package
todo


[github:headroom]: https://github.com/vaclavsvejcar/headroom
[github:headroom/.travis.yml]: https://github.com/vaclavsvejcar/headroom/blob/master/.travis.yml
[hackage:doctest]: https://hackage.haskell.org/package/doctest
[hackage:hpack]: https://hackage.haskell.org/package/hpack
[web:github]: https://github.com
[web:hackage]: https://hackage.haskell.org
[web:hackage/register]: https://hackage.haskell.org/users/register-request
[web:haddock]: https://www.haskell.org/haddock/
[web:haskell]: https://www.haskell.org
[web:stack]: https://docs.haskellstack.org/en/stable/README/
[web:stack/pvp-bounds]: https://docs.haskellstack.org/en/stable/yaml_configuration/#pvp-bounds
[web:stack/travis]: https://docs.haskellstack.org/en/stable/travis_ci/
[web:travis]: https://travis-ci.org
[web:travis/tutorial]: https://docs.travis-ci.com/user/tutorial/