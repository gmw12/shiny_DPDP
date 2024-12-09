#!/bin/bash

echo "clear old files"
rm -R  ./Shiny_DPDP

echo "clone github"
git clone "https://github.com/gmw12/shiny_DPDP" ./Shiny_DPDP

chmod -R 777 ./Shiny_DPDP
chown -R dpmsr ./Shiny_DPDP
chgrp -R dpmsr ./Shiny_DPDP
