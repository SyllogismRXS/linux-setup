#!/bin/bash

# Script usage display function
usage()
{
cat << EOF
usage: $0 [options]

This script installs packages and setups symbolic links
for profile files

EOF
}


# Require the script to be run as root
if [[ $(/usr/bin/id -u) -ne 0 ]]; then
    echo 
    echo "This script must be run as root because libraries will be installed."
    usage
    exit
fi

# Grab username of caller for later
ORIGINAL_USER=$(who am i | awk '{print $1}')

#
# Install packages
#
./install-pkgs.sh

#
# Make symbolic links for profile files
#
su $ORIGINAL_USER -m -c './links.sh'

echo "===================================="
echo "=        Setup Complete            ="
echo "===================================="
