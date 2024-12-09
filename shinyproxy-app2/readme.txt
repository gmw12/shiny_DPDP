1.  This will add the code to the base code in part1
2.  From docker dir in home directory, run the shell script to clone the app code from github
3.  docker build -t gregwaitt/app-shinyproxy:1.0 -f /home/dpmsr/Docker/Shiny_DPDP/shinyproxy-app2/Dockerfile .
4.  docker push gregwaitt/app-shinyproxy:1.0