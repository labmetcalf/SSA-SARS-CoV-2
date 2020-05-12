# Burden of Covid19 in African countries

## How to set this up repository on your computer

### With Rstudio & projects (recommended for ease of GUI:)

Adapted from [Happy Git and GitHub for the useR](https://happygitwithr.com/rstudio-git-github.html):
1. Open Rstudio and go to *File > New Project > Version Control > Git*. In *Repository URL*, paste  *https://github.com/labmetcalf/covid19-africa.git*.
2. Accept the default project directory name, which coincides with the GitHub repo name.
3. Choose the top directory you want the project directory to go (don't put it in dropbox!).
3. I suggest you check “Open in new session”, as that’s what you’ll usually do in real life.
4. Click “Create Project”.

You should find yourself in a new local RStudio Project with the cloned repo and a remote set up to push to this github repository. 

**If you want to fork this, then click fork above & then do the same but with your url: 

The Happy Git and Github with R is a great resource, in particular if you're having issues check out [this section](https://happygitwithr.com/connect-intro.htm)].

### On the command line

```git clone https://github.com/labmetcalf/covid19-africa.git```
```git remote add origin https://github.com/labmetcalf/covid19-afr```

## Options for contributing

1. Push directly to this repository: remember to always go in the order of commit | pull | push! This is probably the easiest way. And if we're not directly messing with each others scripts then this would make the most sense!

2. Create a branch. Then the order will be commit to your branch |  merge with master branch | pull | push. You may also want to pull to your master branch before merging in case it's been a while since you've pushed up!

3. Rather than clone this repository, you can fork it and submit a pull request when you want to merge changes. 
   - First fork it on github & set up the same way as you did the remote except now with url  *https://github.com/{yourusername}/covid19-africa.git*
   - Commit | Pull (from labmetcalf remote) | Push 
   - 
    - To keep your fork up to date with the lab repo you will need to pull from the upstream repo (labmetcalf/covid19-afr)
    - To do this first add a remote for the upstream branch:
    ```git remote add upstream https://github.com/labmetcalf/covid19-afr```
      or with Rstudio in the Rproject in the git pane click on New branch (the two little purple boxes) >  add remote > add the url with name *upstream*.
    - Then pull from that before pushing up to your own remote
    ```git pull upstream master`` (the --ff-only tag )```
    - Finally push to your own remote with the little green button
    
    ## The git commands you need to know
    
    - ```commit``` (your own work)
    - ```pull``` (changes anyone else or if you're working on a separate branch, you yourself,  have made on the remote)
    - ```push``` (to your remote)
    - ```merge``` (two branches)
    
    What if I pull & there are merge conflicts?
    - You can go back to your previous state:
    ```git reset --hard HEAD```
    - You'll eventually need to fix the merge conflicts though if you want to push up!
    
    Resources: 
    - [happy git with r](https://happygitwithr.com)
    - [https://git-scm.com](https://git-scm.com)
    
## Repository Structure

```
├── R
│   ├── _figures
│   ├── _functions
│   ├── agedem
│   ├── climate
│   ├── comorbidities
│   ├── health_access
│   ├── health_capacity
│   └── mobility
├── bash
├── data
│   ├── processed
│   │   ├── alegana_ttimes_reporting
│   │   └── shapefiles
│   └── raw
│       ├── Africa_1km_Age_structures_2020
│       ├── alegana_pseek.tif
│       └── alegana_ttimes
├── docs
├── output
└── shiny
```
Note the raw data files are not included in the repository due to file size!

### Avoiding  setwd()
If you open the .Rproj and work from there, you do not need to set the working directory! 
- Make all paths relative to your project directory
- Alternatively use the package [`here`](https://here.r-lib.org). It's pretty awesome for working with nested directory structures!

## Tasks

- Literature spreadsheet
- Data spreadsheet
- Google doc
