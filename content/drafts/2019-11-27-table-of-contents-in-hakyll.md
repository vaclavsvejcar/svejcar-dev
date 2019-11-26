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
As a first step, I wrote down all the expected features I'd like to have in the ideal implementation:

- option to enable / disable _table of contents_ per blog post
- automatic numbering for _table of contents_ anchor links and also blog post headings
- render _table of contents_ only for full blog posts, not for previews on landing page

Fortunately all of these can be solved pretty easily, as shown in following chapters.

# Implementation
_Pandoc_ already contains support for rendering _table of contents_ and it can be enabled by setting the [writerTableOfContents][haddock:pandoc:writerTableOfContents] field from [WriterOptions][haddock:pandoc:WriterOptions] to `True`. Fortunately _Hakyll_ integrates this option as well and such rendered _table of contents_ is then available as `$toc$` context field.

## Integration with Hakyll

```haskell
withTOC :: WriterOptions
withTOC = defaultHakyllWriterOptions
        { writerNumberSections  = True
        , writerTableOfContents = True
        , writerTOCDepth        = 2
        , writerTemplate        = Just "$toc$\n$body$"
        }
```

The `writerNumberSections` option is worth mentioning, because it automatically ads numbering to both table of content links and the headings inside blog post (as you can see also on this page). These `WriterOptions` can then be used for rendering _blog posts_ like this:

```haskell
match "posts/*" $ do
  route   $ setExtension "html"
  compile $ pandocCompilerWith defaultHakyllReaderOptions withTOC
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
```

Problem with this implementation is that _table of contents_ is rendered always for each _blog post_, which may be unwanted, mainly for shorter ones. Let's see how this can be solved.

## Enabling per blog post
One way how to implement enabling/disabling of _table of contents_ per blog post is to detect presence of some custom field in _YAML_ header, present in each blog post _markdown_ file. Let's say we want to have it disabled by default and enable it by adding `tableOfContents` field to _YAML_ header:

```yaml
---
title: My blog post
tags: one two threee
tableOfContents: true
---

Markdown text here...
```

Based on presence of this field, we would choose whether to render or not the _table of contents_ (we aren't checking the actual value, just whether the field is present or not):

```haskell
match "posts/*" $ do
  route   $ setExtension "html"
  compile $ do
    underlying <- getUnderlying
    toc        <- getMetadataField underlying "tableOfContents"
    let writerOptions' = maybe defaultHakyllWriterOptions (const withTOC) toc
    pandocCompilerWith defaultHakyllReaderOptions writerOptions'
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
```

## Adding stylesheets
Although the above code renders the _table of content_ for blog posts and adds automatic numbering to heading, it would be still nice to add some _CSS_ to make things better looking.

### Adding styles to table of contents
First thing we need to do is to wrap the `table of contents` block into some `<div>` container with custom _class_, so we can refer it in _stylesheet_. This can be done by changing the `writerTemplate` field:

```haskell
withTOC :: WriterOptions
withTOC = defaultHakyllWriterOptions
        { writerNumberSections  = True
        , writerTableOfContents = True
        , writerTOCDepth        = 2
        , writerTemplate        = Just "\n<div class=\"toc\"><div class=\"header\">Table of Contents</div>\n$toc$\n</div>\n$body$"
        }
```

Now we can add proper styling to the `.toc` _CSS_ class. If you want to change styles for the section numbers of _table of contents_ (as used on this page), you can modify it using the `.toc-section-number` class.

### Adding styles to headings
Headings itself now contain the automatically generated section numbers, and it's likely that you'd like to visually separate them from the rest of the heading. This can be done by adding styles to `.toc-section-number` class.

## Making headings clickable
One last _nice to have_ feature would be to transform headings inside blog post into _anchors_, so they can be both clicked and the links can be copied by users to share exact part of your blog post. Unfortunately _Pandoc_ doesn't render headings as anchors by default. There is probably some way how to modify the _AST_ and change this, but for now I was pretty happy with _quick&dirty_ solution based on  _JavaScript_ and [jQuery][web:jquery]. It's not that big deal in this case, because this _DOM_ modification doesn't cause any visual disruptions when the page is loading and it's loaded much earlier before user is able to do any interactions.

```javascript
// '.post-content' is the enclosing element of the blog post
$('.post-content').children('h1, h2, h3, h4, h5').each(function () {
  var id = $(this).attr('id');
  var text = $(this).html();

  $(this)
    .html('')
    .append('<a href="#' + id + '" class="header-link">' + text + '</a>');
});
```
 
# Conclusion
Adding _table of contents_ to your longer blog posts (mainly the longer ones) can help visitors navigate the content. Fortunately in case of _Hakyll_, the implementation itself is not that difficult, mainly thanks to the underlying _Pandoc_. And with help of some _CSS_ and _JavaScript_, we can make pretty decent looking _table of content_ that would match our specific needs.


[haddock:pandoc:WriterOptions]: https://hackage.haskell.org/package/pandoc-2.8/docs/Text-Pandoc-Options.html#v:WriterOptions
[haddock:pandoc:writerTableOfContents]: https://hackage.haskell.org/package/pandoc-2.8/docs/Text-Pandoc-Options.html#v:writerTableOfContents
[web:hakyll]: https://jaspervdj.be/hakyll/
[web:haskell]: https://www.haskell.org/
[web:jquery]: https://jquery.com/
[web:pandoc]: https://pandoc.org/
[github:svejcar-dev]: https://github.com/vaclavsvejcar/svejcar-dev