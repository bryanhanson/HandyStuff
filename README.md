
## How to install HandyStuff

### To install from Github using R:

````r
install.packages("devtools")
library("devtools")
install_github(repo = "HandyStuff", username = "bryanhanson", ref = "master")
library("HandyStuff")
````
If you use `ref = "devel"` you can get the development branch if it is available (and there may be other branches out there too).  Development branches have new, possibly incompletely tested features.  They may may also not be ready to pass checks and thus may not install automatically using the method above.  Check the news file to see what's up.  If you are interested in a particular feature in the devel branch, you can probably just grab the function of interest and source it into an existing package installation.

As of 14 April 2013, version 0.6-1, the devel branch is the one to use.  It is in need of a few small cosmetic tweaks.  The master branch is a version that worked with the old ggplot2 versions but no longer works.  It will eventually be moved to an archive branch.

Questions?  hanson@depauw.edu