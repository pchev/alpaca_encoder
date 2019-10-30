#!/bin/bash

# create html doc from Github wiki

# 1. git clone https://github.com/pchev/alpaca_encoder.wiki.git
# 2. ./mkdoc.sh

cat > encoder.html << EOF
<body bgcolor="#202020" text="#E0E0E0" link="E0A0A0" vlink="E0A0A0" alink="F0B0B0">
<center>
<table BORDER=0 CELLSPACING=0 CELLPADDING=0 WIDTH="900" >
  <tr> 
    <td> 
EOF

sed 's/https:\/\/raw.githubusercontent.com\/wiki\/pchev\/alpaca_encoder\///' alpaca_encoder.wiki/Home.md | markdown - >> encoder.html 

cat >> encoder.html << EOF
   </td>
  </tr>
</table>
</center> 
</body>
EOF
