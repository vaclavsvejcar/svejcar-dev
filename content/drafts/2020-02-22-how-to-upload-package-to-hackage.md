---
title: How to Upload Package to Hackage
description: todo
tags: haskell, hackage, stack
tableOfContents: true
---

When I started to learn [Haskell][web:haskell] few months ago, I wrote just some small _Hello World_-like programs. Months passed by and I finished the initial version of my very first real world Haskell tool called [Headroom][github:headroom] (more about it in next blog post). Right now it's available on [Github][web:github], but it's more common for Haskell projects to be also available on [Hackage][web:hackage]. Uploading package to Hackage involves several not exactly intuitive steps, so I decide to sum it up in this blog post, as both future reference for myself and help for anyone else who wants to do the same thing.

<!-- MORE -->

# Prerequisities
Before uploading the package to Hackage, it's necessary to check that the package contains all required configuration, metadata and that it builds well and passes tests.

## Project Configuration
Your package needs to contain some metadata configuration required by Hackage, so now it's time to fill it in to either `package.yaml` or `.cabal` file. You can easily check what's missing by following command:

```txt
$ cabal check
```

Then just go through all the error reports and fix them. This step will make sure that Hackage won't reject your package.

## Version Bounds
Every package upload to Hackage should have correctly set _version bounds_ for its dependencies, to avoid troubles during building. If you use [hpack][hackage:hpack] instead of `.cabal` file for your package configuration, then the generated `.cabal` file will miss these version restrictions. However, you can tell Stack to generate these by adding the [pvp-bounds][web:stack/pvp-bounds] option to `stack.yaml`:

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
This step is not necessary, but nice to have, as it will make sure your project is building and all tests are passing, so you won't upload broken stuff to Hackage. There are many CI tools to choose from, I decided to go with the [Travis CI][web:travis].

As a first step, you need Travis account. Follow [these steps][web:travis/tutorial] describing how to do that. In order to let Travis run your build, you need to add appropriate `.travis.yml` configuration file to your project root. If you use [Stack][web:stack] tool to build your project, you can check [the Stack documentation][web:stack/travis] with example configuration for Travis, or see the [actual configuration file][github:headroom/.travis.yml] I use for one of my projects.

# Hackage User Account
Now you should have your package ready for upload to Hackage. To do that, you need to register user account and ask Hackage trustees to add you to group with upload rights (this seems to be some kind of anti-spam protection).

You can [register new account here][web:hackage/register], shortly after you'll receive e-mail with confirmation link. When you're done with this, you need to send e-mail to Hackage trustees, asking them to add you to uploader group. In my case it took one or two days before I got any answer, so be patient.

# Upload to Hackage
Now your project should be ready to be uploaded to Hackage. You can generate distribution tarball by running following command:

```txt
$ stack sdist
```

The generated tarball should be available on location similar to this one

```txt
.stack-work/dist/x86_64-osx/Cabal-3.0.1.0/package-x.y.z.tar.gz
```

## Package Candidate
Before you publish your package to the public index, you may want to [upload the package candidate][web:hackage/upload-candidate] first. _Package candidate_ is not released to the main index and contrary to published package, it can be reuploaded as many times as you wish, so you can check that everything is fine and do any necessary fixes. Unfortunately the package candidate workflow is not finished, for example the Haddock documentation is not generated properly.

## Publishing Package
Once you are sure that your package is ready to be published, you can do that either using the [upload form][web:hackage/upload] or by executing following command:

```txt
$ cabal upload path/to/package-x.y.z.tar.gz
```

That should be all, but for example in my case, the Haddock documentation on project Hackage page was not generated for some reason. You can fix this by uploading the Haddock documentation tarball manually using these commands:

```txt
$ cabal haddock --haddock-html-location='https://hackage.haskell.org/package/$pkg-$version/docs' --haddock-hyperlink-source --haddock-quickjump --haddock-for-hackage
$ cabal upload -d  --publish path/to/package-x.y.z-docs.tar.gz
```


[github:headroom]: https://github.com/vaclavsvejcar/headroom
[github:headroom/.travis.yml]: https://github.com/vaclavsvejcar/headroom/blob/master/.travis.yml
[hackage:doctest]: https://hackage.haskell.org/package/doctest
[hackage:hpack]: https://hackage.haskell.org/package/hpack
[web:github]: https://github.com
[web:hackage]: https://hackage.haskell.org
[web:hackage/register]: https://hackage.haskell.org/users/register-request
[web:hackage/upload]: https://hackage.haskell.org/packages/upload
[web:hackage/upload-candidate]: https://hackage.haskell.org/packages/candidates/upload
[web:haddock]: https://www.haskell.org/haddock/
[web:haskell]: https://www.haskell.org
[web:stack]: https://docs.haskellstack.org/en/stable/README/
[web:stack/pvp-bounds]: https://docs.haskellstack.org/en/stable/yaml_configuration/#pvp-bounds
[web:stack/travis]: https://docs.haskellstack.org/en/stable/travis_ci/
[web:travis]: https://travis-ci.org
[web:travis/tutorial]: https://docs.travis-ci.com/user/tutorial/

# Summary
At least for me, the process of uploading package to Hackage was relatively cumbersome, as I found the official documentation relatively stark. Also the fact that the package candidate workflow is not finished isn't really good, because you still cannot preview your Haddock documentation, etc. Hopefully this will improve in near future.