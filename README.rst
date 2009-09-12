Ella
====

Ella is an attempt to create a simple, CGI based web framework in Haskell.  It
is mainly written to help me learn Haskell, and do so in the real, messy world
of HTTP and HTML.  I also didn't like the standard Haskell CGI module for
various reasons, though, where possible, I reuse its code.

The code will evolve as my needs evolve, so I can't guarantee any API
compatibility.  I don't think that anyone else is using this code but me...

Features
--------

I've been inspired by Django, but this framework is far less ambitious in scope.
It does not include an ORM or anything like that.  Some features of Ella, in no
particular order:

 * Explicit request and response objects that are passed around (like Django,
   and unlike the CGI monad).

 * Strongly typed extensible dispatching system.  This means that you can use a
   single type signature on a function to cause the URL parsing mechanism to
   match and parse only data which can be converted to the correct type e.g.::

     -- routes definition:

     routes = [
              -- more routes here ...
              "posts/" <+/> anyParam </+> "edit/"  //-> editPost $ [loginRequired]
              ]

     editPost :: Int -> Request -> IO Response

   The type signature on editPost means that "posts/123/edit" will be parsed and
   will pass the integer 123 and the request object to editPost, but
   "posts/abc/edit/" will not match.

 * 'View processors' - inspired by Django middleware, these allow pre and post
   processing for request handling, which can be installed globally, or on a
   function by function basis (as in the 'loginRequired' processor in the
   example above).  View processors for signed cookies and CSRF protection are
   included.

 * Proper support for character sets.  At the moment I've only added defaults
   for UTF8, but this is already much better than the broken support that the
   CGI module has (it packs bytestrings directly into Strings without attempting
   to convert, which works for latin1 only).  Output is ByteStrings.

Example code
------------

A small but complete example, which I have actually used, is a simple web app
for asking people to sign up to a mailing list via clicking on URLs an e-mail.

http://bitbucket.org/spookylukey/mailinglistconfirm/src/tip/src/ConfirmCgi.hs

A much bigger but incomplete example is my blog code:

http://bitbucket.org/spookylukey/haskellblog/

Some failings:
--------------

 * Assumes use of the IO monad in lots of places - it would be probably be
   better to generalise to MonadIO or something.

 * CGI only at the moment.  It might be possible to adapt for FastCGI etc, I
   don't know.

 * No decent form handling or generating code.  I've experimented with defining
   widgets, and I've used them in real code, but the advantages of raw HTML are
   not compelling.  I have looked at other form handling libraries and haven't
   found anything that satisfies my needs.  'Formlets' is neat, but far too
   inflexible and simplistic, especially with the generated HTML.
