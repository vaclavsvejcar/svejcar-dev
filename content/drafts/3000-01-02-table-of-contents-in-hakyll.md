---
title: Table of Contents in Hakyll
description: todo
tags: hakyll, haskell, meta, pandoc
tableOfContents: true
---

After few attempts to run and actually maintain some personal blog, I finally settled down on this one based on [Hakyll][hakyll]. One of the reasons was that I decided to learn [haskell] and the best way for me how to learn new technology is to use it for some real-world solution. And so far I'm really happy with _Hakyll_ as it provides plenty of room for customization, but it also lacks some feature I'd like to have built-in support for, like the _table of contents_ for blog posts. So I started with summing up what I expect from ideal implementation:

- option to enable / disable _TOC_ per blog post
- automatic numbering for _TOC_ links and also blog post headings
- render _TOC_ only for full blog posts, not for previews on landing page

I spent some time by searching optimal solution and found that [Pandoc][pandoc], the document converter used by _Hakyll_ to convert _Markdown_ to _HTML_, already contains pretty decent built-in support for this. In this blog post, I'll describe how I implemented the _Table of Content_ functionality you can see even on this page. You can also check the full source code of this blog on [GitHub][git_svejcar-dev].

<!-- MORE -->

# TOC support in Pandoc
# Implementation in Hakyll
todo

[hakyll]: https://jaspervdj.be/hakyll/
[haskell]: https://www.haskell.org/
[pandoc]: https://pandoc.org/
[git_svejcar-dev]: https://github.com/vaclavsvejcar/svejcar-dev