# How to use it:

This code is built using [Stack](https://docs.haskellstack.org/en/stable/README).

To run it, `stack build && stack exec simple-exe` has worked for me.

The executable asks the user for a search term, searches for gifs using the giphy api,
and presents a list of the gifs it found. You can pick one of the gifs to open in your browser,
or let the program pick one for you randomly. It opens in your browser by shelling out to the
 macOS `open` command, so this code is only expect to work on a Mac.
 
## Things this code does
* make http requests (using req)
* parse JSON (using Aeson)
* do tabular formatting of console output (using Boxes)
* invoke an external process
* generate random numbers

That's about it. Thanks for reading.