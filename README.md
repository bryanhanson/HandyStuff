
## How to install HandyStuff

### To install from Github using R:

````r
install.packages("devtools")
library("devtools")
install_github(repo = "HandyStuff", username = "bryanhanson", ref = "master")
library("HandyStuff")
````
If you use `ref = "devel"` you can get the development branch if it is available (and there may be other branches out there too).  Development branches have new, possibly incompletely tested features.  They may may also not be ready to pass checks and thus may not install automatically using the method above.  Check the news file to see what's up.

<<<<<<< HEAD
Branch ggArchive contains the old code that worked with earlier versions of ggplot2.  It has been set aside for now.

Questions?  hanson@depauw.edu
=======
Questions?  hanson@depauw.edu
>>>>>>> devel
