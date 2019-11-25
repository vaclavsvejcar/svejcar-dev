---
title: Table of Contents in Hakyll
description: todo
tags: hakyll, haskell, meta, pandoc
tableOfContents: true
---

I always considered my personal blog to be some kind of _notepad_, where I record solutions for interesting problems I solved, or just something I'd like to keep somewhere and what might be eventually helpful for others. After few attempts to run and actually maintain some personal blog, I finally settled down on this one based on [Hakyll][web:hakyll]. Main reason was that I decided to learn [Haskell][web:haskell] and the best way for me how to learn new technology is to use it for some real world application. After publishing several blog posts I realized that some of them are pretty long with many headings and having some kind of _table of contents_ would definitely help with better navigation.

I spent some time by searching optimal solution and found that [Pandoc][web:pandoc], the document converter used by _Hakyll_ to convert _Markdown_ to _HTML_, already contains pretty decent built-in support for this. In this blog post, I'll describe how I implemented the _Table of Content_ functionality you can see even on this page. You can also check the full source code of this blog on [GitHub][github:svejcar-dev].

<!-- MORE -->

# Expected features
I started with summing up features I'd like to have in the ideal implementation:

- option to enable / disable _table of contents_ per blog post
- automatic numbering for _table of contents_ anchor links and also blog post headings
- render _table of contents_ only for full blog posts, not for previews on landing page

Fortunately all of these are solvable pretty easily, as shown in following chapters.

# Implementation
_Pandoc_ already contains support for rendering _table of contents_ and it can be enabled by setting the [writerTableOfContents][haddock:pandoc:writerTableOfContents] field from [WriterOptions][haddock:pandoc:WriterOptions] to `True`. Fortunately _Hakyll_ integrates this option as well and such rendered _table of contents_ is then available as `$toc$` context field.

## Integration with Hakyll

```haskell
withTOC :: WriterOptions
withTOC = defaultHakyllWriterOptions
        { writerNumberSections  = True
        , writerTableOfContents = True
        , writerTOCDepth        = 2
        }
```

These `WriterOptions` can then be used for rendering _blog posts_ like this:

```haskell
match "posts/*" $ do
  route   $ setExtension "html"
  compile $ pandocCompilerWith defaultHakyllReaderOptions withToc
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
```

Problem with this implementation is that _table of contents_ is rendered always for each _blog post_, which may be unwanted, mainly for shorter ones. Let's see how this can be solved.

## Enabling per blog post
One way how to implement enabling/disabling of _table of contents_ per blog post is to detect presence of some custom field in _YAML_ header, present in each blog post _markdown_ file. Let's say we want to have it disabled by default and enable it by adding following field to _YAML_ header:

```yaml
---
title: My blog post
tags: one two threee
tableOfContents: true
---

Markdown text here...
```

Based on presence of this field, we would choose whether to render or not the _table of contents_:

```haskell
match "posts/*" $ do
  route   $ setExtension "html"
  compile $ do
    underlying <- getUnderlying
    toc        <- getMetadataField underlying "tableOfContents"
    let writerOptions' = maybe defaultHakyllWriterOptions (const $ withTOC) toc
    pandocCompilerWith defaultHakyllReaderOptions writerOptions'
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
```
 
# Conclusion
todo

[haddock:pandoc:WriterOptions]: https://hackage.haskell.org/package/pandoc-2.8/docs/Text-Pandoc-Options.html#v:WriterOptions
[haddock:pandoc:writerTableOfContents]: https://hackage.haskell.org/package/pandoc-2.8/docs/Text-Pandoc-Options.html#v:writerTableOfContents
[web:hakyll]: https://jaspervdj.be/hakyll/
[web:haskell]: https://www.haskell.org/
[web:pandoc]: https://pandoc.org/
[github:svejcar-dev]: https://github.com/vaclavsvejcar/svejcar-dev