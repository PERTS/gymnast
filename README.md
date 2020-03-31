# gymnast

Collection of useful functions for data analysis and visualization. Its authors place it into the Public Domain. This work is provided as-is and without any warranty.

Gymnast uses R 3.6.2.

## Use gymnast as a git submodule (preferred)

This method makes files within gymnast available to be imported á-la-carte and ensures that you have a consistent version available. If updates are pushed to gymnast, your code doesn't change until you update your submodule.

### Use in R scripts

Add this import path to your `package.json` file:

```
{
    "R": {
        "importPaths": ["gymnast/R"],
    }
}
```

Bootstrap gymnast in your top-level code. Calling `install_dependencies` is not required if you already have all the right packages, but it's still a good idea.

```
bootstrap <- modules::use("gymnast/R/bootstrap.R")
bootstrap$install_dependencies(gymnast_base_path = "gymnast")
bootstrap$install_module_imports() # import_module() now in global env
```

Now you can import gymnast modules (and any of your own, see comments in `install_module_imports`):

```
sql <- import_module('sql')
```

### Dependencies

Gymnast documents exactly what packages and package versions it depends on in its `DESCRIPTION` file. It does not force you to install them; that's left up to the app using gymnast. However, you are encouraged to incorporate gymnast's dependency list into your own and/or use the `install_dependencies()` function as shown above.

For instance this doesn't check that you have the right packages, but it's a little shorter. Will probably work for one-off scripts in the `analysis` repo.

```
modules::use("gymnast/R/bootstrap.R")$install_module_imports()
```

### Updating a gymnast submodule

GitHub Desktop doesn't appear to support this ☹️, so we'll use the terminal. There are two conceptual steps:

1. Manage changes within the submodule exactly like any other repository: change, commit, push.
2. Change the parent respository's reference to the submodule to point at the updated code.

If you have uncommited changes in the submodule, the parent repository will report that the submodule is "dirty". To fix this, commit your changes within the submodule (step 1). Then the parent will show a diff of two commit hashes: the reference it currently has, and the HEAD commit you've updated. Add and commit this reference change just like any other change (step 2).

Here's a more literal, detailed example workflow. Assume `analysis` is the parent repository, and `gymnast` is checked out in a subdirectory.

```
# I've changed some gymnast code. Time to commit.
cd gymnast
git checkout -b my-gymnast-working-branch
git add .
git commit -m "my cool gymnast changes"
git push
cd ..
# Great, gymnast is taken care of. I can PR my changes there.
# But I'll also need to update analysis.
git add gymnast
git commit -m "updating gymnast"
git push
```

Note that once your `gymnast` working branch is PRed and approved and merged, you do NOT have to do anything more with the submodule reference in `analysis`, because it points at a _commit_ and not a _branch_. Merging just updates the HEAD of `master` to point at a new commit, but your submodule reference already points at that commit, so there's nothing to do. This is also why you'll find that when you `cd` into a submodule, running `git status` will usually _not_ tell you what branch you're on, but rather just what commit you're on. You can still `git checkout master` and `git pull` to get the newest code.

### Installing as a submodule in a new repo

To add gymast as a submodule to your repo:

```
git submodule add https://github.com/PERTS/gymnast
git commit -am "add gymnast"
```

Note that, in the future when cloning your repo, you'll need a separate command to also clone the submodule:

```
git submodule update --recursive --init
```

----

# Deprecated Features

## Use gymnast over the Internet

Avoid using this method because:

1. Any updates to gymnast may immediately affect your scripts and may introduce bugs.
2. Reading URLs over the Internet takes time, so if your script does this repeatedly it will be slow.

Copy and paste the following code into the top of your R script, or run it in your R console.

```r
source_urls <- c(
    "https://raw.githubusercontent.com/PERTS/gymnast/master/R/util_legacy.R",
    "https://raw.githubusercontent.com/PERTS/gymnast/master/R/util_qualtrics_cleaning.R",
    "https://raw.githubusercontent.com/PERTS/gymnast/master/R/util_graphing.R"
)
for (source_url in source_urls) {
    source(pipe(paste0("curl ", source_url)))
}
```

If you are only interested in some gynmast modules, you can modify the list of installed `source_urls`.

## Devtools Installation

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
