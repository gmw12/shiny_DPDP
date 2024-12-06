Build shinyproxy base on OKD
1.  Log into OKD web and get the token
2.  Use token on command line for cli oc access
3.  cd to shinyproxy-base folder
4.  Run the following command to build the shinyproxy image
5.  kubectl apply -k .
6.  Confirm on OKD web that the shinyproxy pod is running