Reproducible Research in R and R Markdown
================
Simon Vandekar, Ali Valcarcel
05 November 2019

# What is Reproducible Research?

## Reproducible research

  - “The term **reproducible research** refers to the idea that the
    ultimate product of academic research is the paper along with the
    laboratory notebooks \[14\] and full computational environment used
    to produce the results in the paper such as the code, data, etc.
    that can be used to reproduce the results and create new work based
    on the research.”

  - Examples of reproducible research comprise compendia of data, code
    and text files, often organised around an R Markdown source document
    or a Jupyter notebook.

–
[Wikipedia](https://en.wikipedia.org/wiki/Reproducibility#Reproducible_research)

## Replicable research (not reproducibility)

  - “Research is replicable when an independent group of researchers can
    copy the same process and arrive at the same results as the original
    study.” – [Tim
    Bock](https://www.displayr.com/replicable-research-care/)
  - [Replicability](https://www.biorxiv.org/content/10.1101/066803v1) is
    a statistical feature (that we’re not talking about today).

# Why R and R Markdown for Reproducible Research?

## Why?\!?

  - There are several reasons R is a great statistical language for your
    reproducible research needs.
      - R Markdown integrates your notes, documentation, math, and code
        in a single set of documents.
      - Most statisticians use `R` as their primary statistical language
        – state-of-the-art methods are available soonest in `R`

## Point of This Tutorial

  - This tutorial is designed to show you that it is possible to do
    fully reproducible research using R Markdown and (hopefully) that it
    is accessible without too much effort.
    <p>
    </p>
    <p>
    </p>
    ### Not the Point of This Tutorial:
  - This tutorial is not designed to teach you the basics of R (sorry).

# Reproducible Research using R packages

## Why?\!?

  - R packages are what developers use for releasing new statistical
    software.
  - Turns out, they’re super useful for keeping track of your notes and
    code for research as well.
  - Once your project is finalized, your notes and code can be
    distributed within the R package.
  - **Important note**: Github repositories are public by default; do
    not put an unpublished paper in a public repository\!

## Structure of an R package

# Installing R and Rstudio

## Linux

  - Install `R` from the command line in linux.
  - Then install the version of Rstudio for your system
  - I haven’t tested these commands

<!-- end list -->

``` bash
sudo apt -y install r-base # install R
sudo apt -y install wget # install wget
wget https://download1.rstudio.org/desktop/bionic/amd64/rstudio-1.2.5019-amd64.deb # install Rstudio
```

## Mac OS

  - I like to use [Homebrew](https://brew.sh) for managing software.
  - Be sure to pay attention to homebrew output to be sure installation
    proceeds correctly.
  - Homebrew needs to create symlinks and that sometimes fails.
  - xcode command line tools are necessary for developing R packages.
  - Can install R and Rstudio at the command line (after installing
    homebrew)

<!-- end list -->

``` bash
xcode-select --install # install x-code command line tools
brew install r # install R
brew cask install --appdir=/Applications rstudio # install R-studio (GUI for R)
```

## Windows

  - Sorry, I’m not much use with Windows, but all software is supported
    for Windows.
  - Check out
  - [R on CRAN](https://cran.r-project.org/)
  - [Rstudio](https://rstudio.com/)

## Rstudio looks like this

![](inst/images/rstudio.png)

# Creating an R package for Reproducible Research

## Writing an R Markdown Document

As far as I know, R Markdown can use all the same syntax as regular
Markdown. You can type equations using syntax similar to latex. R
Markdown documents can be compiled to a github README (like this
document), html, pdf, word doc, or many other types of documents.

  - Some document formats:
    
      - github\_document, html\_document, pdf\_document

  - Some presentation formats:
    
      - ioslides\_presentation, slidy\_presentation,
        revealjs::revealjs\_presentation

  - Some basic syntax references can be found here

  - [Markdown
    Basics](https://rmarkdown.rstudio.com/authoring_basics.html)

  - [Mathematics in R
    Markdown](https://www.calvin.edu/~rpruim/courses/s341/S17/from-class/MathinRmd.html)

## R Markdown Compilation Options

There are many options for formatting the output of your markdown
document

  - [html output
    options](https://bookdown.org/yihui/rmarkdown/html-document.html)
  - [revealjs options](https://github.com/rstudio/revealjs)

## Writing R (or other) Code in an R Markdown Document

### Cloning and Updating with Git

### Installing an R package from Github or Bitbucket

### Exporting, Documenting, and Compiling Functions

# Other stuff

## Disclaimers

I am not an expert in reproducible methods and am actively learning the
best way to do reproducible research using R Markdown, Git, and Docker.
The R Markdown document is conducive with other languages, such as
Python, Bash, and Stan. There are other tools (e.g. Jupyter Notebook)
that have similar functionality and **may** be better suited for other
programming languages.

## Important Resources That I Have Neglected

There are many important tools that I have (embarrassingly) neglected to
talk about because I have not learned them myself (e.g. `tidyverse`,
`testthat`).

  - [CRAN Reproducible
    Research](https://cran.r-project.org/web/views/ReproducibleResearch.html)
