---
title: Sass in Hakyll
tags: hakyll, haskell, meta
---

In last few years, _CSS_ changed a lot. Many new features were added, such as [Flexbox][css-flexbox], [Media Queries][css-media] and [animations][css-animations], but other things, such as using some kind of _variables_ or _modules_ for better organizing of source code are still not so great. This is where _CSS preprocessors_ come into play. And since I always had great experience with [Sass][sass] preprocessor, I decided to use if for this blog, written using [Hakyll][hakyll] static sites generator.

Unfortunately there's no built-in support for _Sass_ or any other _CSS preprocessor_ in _Hakyll_, but adding it is pretty straightforward and doesn't require much coding. In this blog post, I decided to sum up this process, both for future myself and for anyone else who'd try to solve the same problem.

<!-- MORE -->

# Using hakyll-sass package
Surprisingly, we're not the first ones who solve this issue, so there's a [hakyll-sass] package, that uses the [hsass] under the hood and wraps its logic into functions ready to be used in _Hakyll_. Easiest way is to use the [sassCompiler][hakyll-sass-sassCompiler] function to compile all `.sass` files into `.css` ones as shown below:

```haskell
match "assets/css/*.scss" $ do
  route $ setExtension "css"
  compile (fmap compressCss <$> sassCompiler)
```

## Problem with hot reloading
The above solution just works, but with pretty annoying flaw. It's common practice to split your _stylesheets_ into many smaller `.scss` files and then `@include` them into the main one, which is then compiled into single monolithic _CSS_ file. In _Hakyll_, if you use the _hot reloading_ using the `stack exec site watch` command, all changes are properly processed and visible in web browser each time you edit the main `.scss` file. However, when you do some changes into files included using the `@include` directive, nothing happens. _Hakyll_ just doesn't know that in addition to the changed file, it should also recompile the main `.scss` file, to embrace changes from the included file. Because of this, no changes are visible in the web browser.

## The solution
todo

[css-animations]: https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Animations/Using_CSS_animations
[css-flexbox]: https://www.w3schools.com/css/css3_flexbox.asp
[css-media]: https://developer.mozilla.org/en-US/docs/Web/CSS/Media_Queries/Using_media_queries
[hakyll]: https://jaspervdj.be/hakyll/
[hakyll-sass]: http://hackage.haskell.org/package/hakyll-sass
[hakyll-sass-sassCompiler]: https://hackage.haskell.org/package/hakyll-sass-0.2.3/docs/Hakyll-Web-Sass.html#v:sassCompiler
[hsass]: http://hackage.haskell.org/package/hsass
[sass]: https://sass-lang.com/