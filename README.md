# rentScraper

Uh, yeah.  So.  This isn't like a really advanced project or anything.  It's just that I've been doing web scraping for years from WWW::Mechanize in Perl and I wanted to bring my scraping efforts a bit closer to R.  So that's what this project is all about.

The only really useful takehome message I have here is how to call native libcURL config options that define a cookiefile, then use html_session() from rvest using those configs, given that you're pointing to a valid cookiefile defined from a valid user session.  This is the easiest way I found of forwarding user credentials to an automated session.

The plots are nice too, but I need to find a cleaner way to handle some of the sorting.
