# gymnast

Collection of useful functions for data analysis and visualization.
When calling util.R or any of the other standalone source files, use source(filename, chrdir=TRUE)

# Building R packages

## How to Code a Package

### Life Cycle: What's Run When?

The code in a package is run when the package is built, which is done by the developer of the package, far away from the end user. The end user receives the objects in memory that are the result of that code running.

That means if you write this line as top-level code in your package ("top-level" means not in any function, just an expression to be executed):

    print("Top level code is being run!")

Then the package developer will see it when they build the source code into a package:

    [1] "Top level code is being run!"

But the end user will _not_ see it when they require the package:

    > library(gymnast)
    >

An important consequence of this is your required libraries don't carry over to the end user. Any libraries required by the package are loaded when the package is _built_, and can be used throughout the package, and the end user can use the package's functions that depend on those libraries, but the end user _cannot_ use those libraries directly (they have to do the work of requiring them or using double colon syntax).

So if your package has this source code:

    library(dplyr)
    gymnast.summarise <- dplyr::summarise

The end user sees:

    > library(gymnast)
    > summarise
    Error: object 'summarise' not found
    > gymnast.summarise
    function (.data, ...)
    {
        summarise_(.data, .dots = lazyeval::lazy_dots(...))
    }
    <environment: namespace:dplyr>

**In general, do not write top-level code in packages**, because it probably won't do what you want (i.e. have any effect on the end user).

### Namespace: What Can I Reference When?

?? Something about the NAMESPACE file.

## Installing Necessary Tools

Compiling requires XCode, FORTRAN, and a developer's version of R.

### XCode

Install [XCode][1] (link opens the App Store).

[1]: https://itunes.apple.com/us/app/xcode/id497799835?ls=1&mt=12#

### FORTRAN

You may need to remove existing fortran binaries.

    which fortran

Hopefully that command produces no output, which means you have no existing installation to remove.

Then download the version of [gfortran][2] appropriate to your OS.

[2]: http://gcc.gnu.org/wiki/GFortranBinaries#MacOS

Open downloaded .dmg file, finding the .pkg file within. **Right click** the package and choose open (twice) to get around the "unidentified developer" warning. Complete the installation wizard.

Now this command should show the path of the successfully installed FORTRAN compiler.

    which fortran

### Developer's Version of R

Open your Applications folder (`/Applications`) and drag any existing R app to the trash. Then download and install one of [these][3] instead. Choose the
most appropriate stable download from the "R framework" table.

[3]: http://r.research.att.com/

Install the package. It took some time on my laptop, have patience.

## Preparing The Files

Follow [Karl Broman's instructions][4], which basically involve putting your scripts in a `R` subdirectory and adding a `DESCRIPTION` text file. Make sure to use the same name for your package directory as you use in the description file.

[4]: http://kbroman.org/pkg_primer/pages/minimal.html

In a terminal window, navigate to the **parent directory** of your package. For example, in building gymnast, my file stucture looked like this:

    ~/Sites/gymnast
    ~/Sites/gymnast/DESCRIPTION
    ~/Sites/gymnast/R

So I navigated to the `Sites` directory:

    cd ~/Sites

Then run the build:

    R CMD build gymnast
