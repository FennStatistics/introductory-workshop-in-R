# Slides: Introduction Workshop in R

The slides were written using RMarkdown (specifically the [xaringan](https://bookdown.org/yihui/rmarkdown/xaringan.html) presentation framework).

## PDF

I have "printed" my dynamic slides to a PDF, which you could download to make some notes: `slides Journey through R`


## Files and Folders

### 1. `introductionR_julius.Rmd`

This is the **source RMarkdown** file for the presentation. It uses `xaringan::moon_reader` to produce HTML slides and includes:
- Custom styling (`my-theme.css`, `my-fonts.css`)
- Package management and setup
- Preloaded example data (`tramo1998etal_twins.csv`)


### 2. `introductionR_julius.html`

The **rendered HTML presentation** produced from the RMarkdown source. Viewable in any modern browser.

> Note: The HTML file is best viewed in full screen and benefits from the 16:9 slide ratio.

### 3. `data/`

Contains the dataset used in the slides:
- `tramo1998etal_twins.csv`: A exemplary dataset.

### 4. `images/`

This folder holds images used in the presentation slides (e.g., for illustration or example analysis outputs).

### 5. `libs/`

This directory contains JavaScript and CSS libraries required by xaringan to render the presentation offline.

### 6. `subfiles/`

Custom stylesheets and fonts used in the presentation. Notably:
- `my-theme.css`: Defines the visual appearance of the slides.
- `my-fonts.css`: Controls font style and sizing for headings and content.

### 7. `introductionR_julius_files/`

Automatically generated folder when rendering the HTML slide deck; contains supporting files for interactivity and styling.

---

> This workshop embraces reproducibility and modular learning using R and RMarkdown. While legacy RMarkdown is used here, new users may consider switching to [Quarto](https://quarto.org/) for future projects, creating presentation.
