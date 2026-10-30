# Meta-analysis practicals

This Quarto website contains practicals on common-effect and random-effects meta-analysis for ongoing pregnancy and number of oocytes retrieved.

## Publish with GitHub Pages

The included GitHub Actions workflow renders the Quarto project when changes are pushed to `main`. Pull requests are rendered as a build check; they are not deployed. To publish:

1. Add this project to a GitHub repository and push the files to its `main` branch.
2. In the repository, open **Settings → Pages** and set **Build and deployment → Source** to **GitHub Actions**.
3. Open the **Actions** tab and check the `Publish Quarto site to GitHub Pages` workflow. The first successful run deploys the site; later pushes to `main` update it.

The rendered website is built in `_site/` and uploaded directly as a Pages artifact. You do not need to commit rendered HTML files. The workflow installs R, `metafor`, and `knitr` because the practical pages run their examples during rendering.

To preview the site locally, install Quarto, R, and the R packages `metafor` and `knitr`, then run `quarto render`. Open `_site/index.html` in a browser.
