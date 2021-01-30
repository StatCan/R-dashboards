# R-dashboards

This repo contains R-shiny dashboards that are hosted on an R-shiny server. Each
subdirectory here contains a different dashboard, with dashboards accessible at
`https://shiny.covid.cloud.statcan.ca/<DASHBOARD_DIR_NAME>`.

> **⚠ This service may change**  
> Helping you publish your visualizations is a core part of our service, and we
> will keep doing that. However, the means of submitting/updating Shiny projects
> – and the URLs they get published to – are likely to change as we improve this
> service.

## Submitting a Dashboard

To submit your own dashboard:

1. Fork this R-dashboards repo.
2. Add your dashboard to an unclaimed top level directory (similar to
   `/covid-19-dashboard` and others).
   - Note that the name you choose here also sets your dashboard's URL
     (`https://shiny.covid.cloud.statcan.ca/my-well-named-dashboard`).
   - You need to embed your dashboard's data directly in your Shiny project (for
     example, see
     [/ev-data-viz/data](https://github.com/StatCan/R-dashboards/tree/master/ev-data-viz/data/raw)).
     _A more flexible solution is in the works but not yet completed._
   - **⚠ If your data is large (more than a few MB)** please discuss with us
     first.
   - Do one of the following:
     - (Optional but appreciated - this will speed up our review/release
       process.) Test your dashboard against the central shiny implementation by
       following the **Deployment Testing Instructions** section below. In
       particular, identify any packages that are missing from the shiny repo
       and note them in the text of your PR.
     - Check this
       [packages](https://github.com/StatCan/shiny/blob/master/PACKAGES) file to
       see if all R packages you use are included in the list. If any are
       missing, note them in the text of your PR.
3. Open a PR to merge your fork into R-dashboards:master. Start your PR title
   with "[WIP]" to indicate your work in progress and remove it once you are
   ready to merge.
   - See
     [here](https://github.com/StatCan/R-dashboards/pull/16 "Example pull request 1")
     and
     [here](https://github.com/StatCan/R-dashboards/pull/17 "Example pull request 2")
     for example PRs.

## Testing your Dashboard

To test any new/updated dashboards prior to deployment, do the following:

1. Submit a pull request to the R-dashboards repo with your new/updated
   dashboard.
2. Clone the PR for review from the R-dashboards repo (replace `<PR_NUMBER>`
   with the PR number from GitHub).
   ```bash
   pr_number=<PR_NUMBER>
   git clone https://github.com/StatCan/R-dashboards.git
   cd R-dashboards
   git fetch origin pull/$pr_number/head:pr$pr_number
   git checkout pr$pr_number
   # Save this directory for later
   path_to_r_dashboards=`pwd`
   cd -
   ```
3. Clone and build the shiny server locally. You need
   [Docker](https://www.docker.com/) installed to run it.

   ```bash
   git clone https://github.com/StatCan/shiny.git
   cd shiny
   docker build . -t shiny-server-test
   # Forward the port so you can view it locally
   # Map a volume to the above dashboard directory
   docker run \
     -p 3838:3838 \
     -v $path_to_r_dashboards:/srv/shiny-server \
     shiny-server-test
   ```

4. Go to [127.0.0.1:3838](http://127.0.0.1:3838) and test your dashboard.
   - If there are any bugs, fix them locally and iterate above until solved.
   - Tip: If a package is missing, update the Shiny server Dockerfile to import
     packages from an additional PACKAGE_temp and put any new packages there
     while testing. This saves time by avoiding reinstalling previously
     installed packages. Once you have all the packages you need, edit the
     PACKAGES file directly to include those you need and remove the
     PACKAGE_temp file.
