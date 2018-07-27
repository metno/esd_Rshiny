#/bin/bash
R --slave --no-restore -e 'shiny::runApp("metnod/", launch.browser=TRUE)'