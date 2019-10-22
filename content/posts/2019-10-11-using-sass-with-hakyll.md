---
title: Using Sass with Hakyll
description: Unfortunately, there's no direct support for Sass in Hakyll, but it's really easy to implement it using the hakyll-sass library.
tags: hakyll, haskell, meta, sass
withTOC: true
---

In last few years, _CSS_ changed a lot. Many new features were added, such as [Flexbox][css-flexbox], [Media Queries][css-media] and [animations][css-animations], but other things, such as using some kind of _variables_ or _modules_ that would allow better organizing of source code are still not so great. This is where _CSS preprocessors_ come into play. And since I always had great experience with [Sass][sass] preprocessor, I decided to use if for this blog, written using [Hakyll][hakyll] static site generator.

Unfortunately there's no built-in support for _Sass_ or any other _CSS preprocessor_ in _Hakyll_, but adding it is pretty straightforward and doesn't require much coding. In this blog post, I decided to sum up this process, both for future myself and for anyone else who'd try to solve the same problem.

<!-- MORE -->

# Using hakyll-sass package
Surprisingly, we're not the first ones who solve this issue, so there's a [hakyll-sass] package, that uses the [hsass] under the hood and wraps its logic into functions ready to be used in _Hakyll_. Easiest way is to use the [sassCompiler][hakyll-sass-sassCompiler] function to compile `.sass` files to `.css`. Personally I prefer to split my _stylesheets_ into smaller `.scss` files, and then `@import` these into the one `main.scss`, which then compiles into single monolithic `main.css`. This can be done using below code.
```haskell
match "assets/css/main.scss" $ do
  route $ setExtension "css"
  compile (fmap compressCss <$> sassCompiler)
```

## Problems with hot reloading
Now try the above with _hot reloading_ using `stack exec site watch` command. When you change the `main.scss` file, changes are detected, recompiled and new `.css` file is generated, so you see the changes in web browser after refreshing. However, when you edit one of the secondary files you `@include` into the main one, no changes are detected, until you restart the _hot reloading_, which is pretty annoying.

So what's wrong? Using the `match "assets/css/main.scss"` rule, we tell _Hakyll_ to watch changes on file `main.scss`. But when you edit any of the included file, _Hakyll_ has no clue that it also needs to recompile the main one, to refresh the resulting `.css`. You might try to change the above rule to `match "assets/css/**.scss"`, but this won't help too, because when now you edit the included file, _Hakyll_ still has no clue that it also needs to recompile the `main.scss` and it results to the same problem.

## The solution
What we need to do is to tell _Hakyll_ to also recompile the `main.scss` each time any other `.scss` file is changed. It took me some time to discover how to do this, but then I found [this post from Hakyll author][solution], where he shows exactly what we need:

```haskell
scssDependency <- makePatternDependency "assets/css/**.scss"
rulesExtraDependencies [scssDependency]
  $ match "assets/css/main.scss"
  $ do
      route $ setExtension "css"
      compile (fmap compressCss <$> sassCompiler)
```

In above code, thanks to the [rulesExtraDependencies][haddock-rulesExtraDependencies] function, we're able to add extra [Dependency][haddock-Dependency] for the compiler we use to compile the `main.scss`. And to create it we use the [makePatternDependency][haddock-makePatternDependency] function that creates dependency from given [Pattern][haddock-Pattern], in our case for `assets/css/**.scss`.

[css-animations]: https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Animations/Using_CSS_animations
[css-flexbox]: https://www.w3schools.com/css/css3_flexbox.asp
[css-media]: https://developer.mozilla.org/en-US/docs/Web/CSS/Media_Queries/Using_media_queries
[haddock-Dependency]: https://jaspervdj.be/hakyll/reference/Hakyll-Core-Rules.html#t:Dependency
[haddock-makePatternDependency]: https://jaspervdj.be/hakyll/reference/Hakyll-Core-Metadata.html#v:makePatternDependency
[haddock-Pattern]: https://jaspervdj.be/hakyll/reference/Hakyll-Core-Identifier-Pattern.html#t:Pattern
[haddock-rulesExtraDependencies]: https://jaspervdj.be/hakyll/reference/Hakyll-Core-Rules.html#v:rulesExtraDependencies
[hakyll]: https://jaspervdj.be/hakyll/
[hakyll-sass]: http://hackage.haskell.org/package/hakyll-sass
[hakyll-sass-sassCompiler]: https://hackage.haskell.org/package/hakyll-sass-0.2.3/docs/Hakyll-Web-Sass.html#v:sassCompiler
[hsass]: http://hackage.haskell.org/package/hsass
[sass]: https://sass-lang.com/
[solution]: https://hakyll.narkive.com/yYNrb07M/match-many-patterns-to-the-same-route-target#post2