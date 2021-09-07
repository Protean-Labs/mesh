#!/bin/bash
echo "{
  \"argv\": [
   \"`pwd`/_esy/default/build/install/default/bin/mesh-kernel\",
   \"--connection-file\",
   \"{connection_file}\",
   \"--log\",
   \"log.txt\"
  ],
  \"display_name\": \"Mesh\",
  \"language\": \"mesh\"
}" > jupyter-kernel/kernel.json

jupyter kernelspec install jupyter-kernel/ --user --replace --name=mesh