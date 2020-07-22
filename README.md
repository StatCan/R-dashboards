# R-dashboards

This repo contains R-shiny dashboards that are hosted on an R-shiny server.  Each subdir here contains a different dashboard, with dashboards accessible at https://shiny.covid.cloud.statcan.ca/<DASHBOARD_DIR_NAME>.

# Submitting a Dashboard

To submit your own dashboard:

* Fork this R-dashboards repo
* Add your dashboard to an unclaimed top level directory (similar to /covid-19-dashboard and others).  
	* Note that the name you choose here also sets your dashboard's URL (https://shiny.covid.cloud.statcan.ca/my-well-named-dashboard)
	* As an interm solution, please embed your dashboard's data in the dashboard (for example, see [/ev-data-viz/data](https://github.com/StatCan/R-dashboards/tree/master/ev-data-viz/data/raw)).  A more flexible solution is in the works but not yet completed
	* NOTE: **if your data is large (more than a few MB)** please discuss with us first
	* Do one of the following:
		* (Optional but appreciated - this will speed up our review/release process) Test your dashboard against the central shiny implementation by following the **Deployment Testing Instructions** section below.  In particular, identify any packages that are missing from the shiny repo and note them in the text of your PR.
		* Check this [packages](https://github.com/StatCan/shiny/blob/master/PACKAGES) file to see if all R packages you use are included in the list.  If any are missing, note them in the text of your PR
* Open a PR to merge your fork into R-dashboards:master. Use the flag [WIP] in the PR title to indicate your work in progress and remove it once you are ready to merge.
	* See [here](https://github.com/StatCan/R-dashboards/pull/16) and [here](https://github.com/StatCan/R-dashboards/pull/17) for example PRs

# Deployment Testing Instructions

To test any new/updated dashboards prior to deployment, do the following:

* Submit a pull request to the R-dashboards repo with your new/updated dashboard
* Clone the PR for review from the R-dashboards repo (replace <PR_NUMBER> with the PR number from github)
	```
	pr_number=<PR_NUMBER>
	git clone https://github.com/StatCan/R-dashboards.git
	cd R-dashboards
	git fetch origin pull/$pr_number/head:pr$pr_number
	git checkout pr$pr_number
	# Save this directory for later
	path_to_r_dashboards=`pwd`
	cd -
	```
* Clone and build the shiny server locally. This requires the (Docker)[https://www.docker.com/] software on your testing machine.

	```
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
* Go to [127.0.0.1:3838](http://127.0.0.1:3838) and test the dashboard
	* If there's any bugs, fix them locally and iterate above until solved
	* Tip: If a package is missing, update the shiny server docker file to import packages from an additional PACKAGE_temp and put any new packages there while testing.  This saves time by avoiding reinstalling previously installed packages.  Once you have all the packages you need, edit the PACKAGES file directly to include those you need and remove the PACKAGE_temp file

