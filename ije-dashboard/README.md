************************************************************************
These files are used to generate IJE shiny app                           
       Prepared by Zhe (River) Yang
************************************************************************
`1. Prepare Data.r`: This r program is used to clean up the IJE data table 
   and saved data files for app generation. Before running the program, two
   lines of code need to be updated:
             a)  setwd('//scan01/Users/yanzh/IJE/2020/data')
               It specified the current working dirctory. All final datasets 
               are saved there.
             b) rawfile = quote("//Scan01/ipe/Theresa/2020_Vintage/ShinyR/")
               It specified the location of raw datafiles.
After updating thest two parts, you can run the R-file, all final datasets will 
be saved in the current working dirctory you specified above.

2. go the shiny fold and open up the `global.r`.
    a) update  setwd('//scan01/Users/yanzh/IJE/2020/data').
       again it is the place where you saved your final data
    b) update beginy = quote(2002) and endy = quote(2017)
       These are the first and the last years of data.
    c) save and close `global.r`.

3.Open RStudio run the code below in the Console
    shiny::runApp("//scan01/Users/yanzh/IJE/2020/code/shiny", host = "0.0.0.0", port=1300)
    Note:`//scan01/Users/yanzh/IJE/2020/code/shiny` is where you saved shiny files `global.r`
    `ui.r` and `server.r` 
     1300 is the port number, it can also be changed. 

4. If you run the shiny on scan02, the app will be avaiable at http://scan02:1300/ 