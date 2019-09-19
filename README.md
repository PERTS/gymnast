# gymnast

Collection of useful functions for data analysis and visualization. Its authors place it into the Public Domain. This work is provided as-is and without any warranty.

## Install gymnast

Make sure you have a current version of R. The most recently tested version is 3.2.3.

Then copy and paste the following code into the top of your R script, or run it in your R console.

```r
source_urls <- c(
    "https://raw.githubusercontent.com/PERTS/gymnast/master/R/util.R",
    "https://raw.githubusercontent.com/PERTS/gymnast/master/R/util_qualtrics_cleaning.R",
    "https://raw.githubusercontent.com/PERTS/gymnast/master/R/util_graphing.R"
)
for (source_url in source_urls) {
    source(pipe(paste0("curl ", source_url)))
}
```

If you are only interested in some gynmast modules, you can modify the list of installed `source_urls`.

## Alternate Installation

This is more like a typical package installation, however you will not get any updates made to gymnast until you re-install.

    > install.packages("devtools")

    (Answer "n" if you are asked "Do you want to install from sources the package which needs compilation?")

    > devtools::install_github("PERTS/gymnast")

After installing, use just like any other package:

    > library(gymnast)

# Creating R packages

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

### Respecting The End User

Certain functions have effects that will probably upset the person who loads your package, e.g. `setwd()`. No one expects the Spanish Inquisition, nor do they expect you to change their working directly for them.

Similarly, don't use `source()`. Use `devtools::load_all()` instead.

Less intuitively, `library()` and `require()` change the end user's system by modifying the search path. Put package dependencies in the `DESCRIPTION` file (see this package's file as an example).

**In general, do not write top-level code in packages**, because it probably won't do what you want (i.e. have any positive effect on the end user).

### Namespace: What Can I Reference When?

?? Something about the NAMESPACE file.

## Synchronizing with github

The default use case for gymnast is installing from github (see the installation section above). PERTS team members can update gymnast by synchronizing with the master branch on github.

**Anyone updating gymnast should immediately attempt to re-install from github to ensure that all tests pass and the installation succeeds.**

## Installing Build Tools

Compiling a package provides a single file which users can download and manually install. It requires XCode, FORTRAN, and a developer's version of R.

**Build tools are not necessary if the goal is to have users install directly from github.** In that case, skip this section.

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

## Building a Gymnast Bundle

**Not necessary if installing from github.**

Follow [Karl Broman's instructions][4], which basically involve putting your scripts in a `R` subdirectory and adding a `DESCRIPTION` text file. Make sure to use the same name for your package directory as you use in the description file.

[4]: http://kbroman.org/pkg_primer/pages/minimal.html

In a terminal window, navigate to the **parent directory** of your package. For example, in building gymnast, my file stucture looked like this:

    ~/Sites/gymnast
    ~/Sites/gymnast/DESCRIPTION
    ~/Sites/gymnast/R

So I navigated to the `Sites` directory:

    cd ~/Sites

Then ran the build:

    R CMD build gymnast
