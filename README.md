# R-dashboards

This repo contains R-shiny dashboards that are hosted on an R-shiny server.  Each subdir here contains a different dashboard, with dashboards accessible at https://shiny.covid.cloud.statcan.ca/<DASHBOARD_DIR_NAME>.

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
* Clone and build the shiny server locally
	```
	git clone https://github.com/StatCan/shiny.git
	cd shiny
	docker build . -t shiny-server-test
	# Forward the port so you can view it locally
	# Map a volume to the above dashboard directory
	docker run \
	  -p 3838:3838 \
	  -v $path_to_r_dashboards:/srv/shiny-server \
	  shiny-test
	```
* Go to [127.0.0.1:3838](http://127.0.0.1:3838) and test the dashboard
	* If there's any bugs, fix them locally and iterate above until solved
	* Tip: If a package is missing, update the shiny server docker file to import packages from an additional PACKAGE_temp and put any new packages there while testing.  This saves time by avoiding reinstalling previously installed packages.  Once you have all the packages you need, edit the PACKAGES file directly to include those you need and remove the PACKAGE_temp file

