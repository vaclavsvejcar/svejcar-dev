---
title: Sass in Hakyll
tags: hakyll, haskell, meta
---

In last few years, _CSS_ changed a lot. Many new features were added, such as [Flexbox][css-flexbox], [Media Queries][css-media] and [animations][css-animations], but other things, such as using some kind of _variables_ or _modules_ for better organizing of source code are still not so great. This is where _CSS preprocessors_ come into play. And since I always had great experience with [Sass][sass] preprocessor, I decided to use if for this blog, written using [Hakyll][hakyll] static sites generator.

Unfortunately there's no built-in support for _Sass_ or any other _CSS preprocessor_ in _Hakyll_, but adding it is pretty straightforward and doesn't require much coding. In this blog post, I decided to sum up this process, both for future myself and for anyone else who'd try to solve the same problem.

<!-- MORE -->

# Using hakyll-sass package
Surprisingly, we're not the first ones who solve this issue, so there's a [hakyll-sass] package, that uses the [hsass] under the hood and wraps its logic into functions ready to be used in _Hakyll_. Easiest way is to use the [sassCompiler][hakyll-sass-sassCompiler] function to compile `.sass` files to `.css`. Personally I prefer to split my _stylesheets_ into smaller `.scss` files, and then `@import` these into the one `main.scss`, which then compiles into single monolithic `main.css`. This can be done using below code.
```haskell
match "assets/css/screen.scss" $ do
  route $ setExtension "css"
  compile (fmap compressCss <$> sassCompiler)
```

## Problems with hot reloading
Now try the above with _hot reloading_ using `stack exec site watch` command. When you change the `main.scss` file, changes are detected and recompiled and new `.css` file is generated, so you see the changes in web browser after refreshing. However, when you edit one of the smaller files you `@include` into the main one, no changes are detected, until you restart the _hot reloading_, which is pretty annoying.

So what's wrong? Using the `match "assets/css/screen.scss"` rule, we tell _Hakyll_ to watch changes on file `screen.scss`. But when you edit any of the included file, _Hakyll_ has no clue that it also needs to recompile the main one, to refresh the resulting `.css`. You might try to change the above rule to `match "assets/css/**.scss"`, but this won't help too, because when now you edit the included file, _Hakyll_ still has no clue that it also needs to recompile the `main.scss` and it results to the same problem.

## The solution
Basically what we need to do is to tell _Hakyll_ to also recompile the `main.scss` each time any other `.scss` file is changed.

[css-animations]: https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Animations/Using_CSS_animations
[css-flexbox]: https://www.w3schools.com/css/css3_flexbox.asp
[css-media]: https://developer.mozilla.org/en-US/docs/Web/CSS/Media_Queries/Using_media_queries
[hakyll]: https://jaspervdj.be/hakyll/
[hakyll-sass]: http://hackage.haskell.org/package/hakyll-sass
[hakyll-sass-sassCompiler]: https://hackage.haskell.org/package/hakyll-sass-0.2.3/docs/Hakyll-Web-Sass.html#v:sassCompiler
[hsass]: http://hackage.haskell.org/package/hsass
[sass]: https://sass-lang.com/