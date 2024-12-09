Shiny_DPDP image for shinyproxy (linux and R packages) without app code
1.  This image is built off line with Dockerfile on linux system
2.  This image will build the base shiny app without the app code
3.  From docker dir in home directory, run the shell script to clone the app code from github
4.  docker build -t gregwaitt/base_app-shinyproxy:1.0 -f /home/dpmsr/Docker/Shiny_DPDP/shinyproxy-app1/Dockerfile .
5.  docker push gregwaitt/base_app-shinyproxy:1.0
