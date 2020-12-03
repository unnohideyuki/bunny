#!/bin/bash -e

INSTALL_BASE_DIR=$HOME
VERSION="0.9.0"

usage(){
    echo "Usage: install.sh [install-base-directory]"
    exit 1
}

if [ $# -gt 1 ]; then
    usage
fi

if [ "$1" = "-h" -o "$1" = "==help" ]; then
    usage
fi

if [ -n "$1" ]; then
    INSTALL_BASE_DIR=$1
fi

INSTALL_DIR=$INSTALL_BASE_DIR/bunny/$VERSION

if [ ! -x bin/bunnyc ]; then
    echo "bin/bunnyc is not found. about."
    exit 1
fi

echo "Installing bunny to $INSTALL_DIR ..."
read -p "Hit enter key to continue: "

## $INSTALL_DIR/bin
##             /lib
##             /brt/src/
##             /AndroidProjectPrototype

mkdir -p $INSTALL_DIR/bin
cp bin/bunny bin/bunnyc $INSTALL_DIR/bin/
echo "INSTALL_DIR=$INSTALL_DIR" > $INSTALL_DIR/bin/bunny.config
cat <<EOF >> $INSTALL_DIR/bin/bunny.config
  LIB_DIR=$INSTALL_DIR/lib
  BRTSRC_DIR=$INSTALL_DIR/brt/src
  PRJPROTO_DIR=$INSTALL_DIR/AndroidProjectPrototype
EOF

mkdir -p $INSTALL_DIR/lib/
cp lib/Prelude.hs $INSTALL_DIR/lib/

mkdir -p $INSTALL_DIR/brt/
cp -r ../brt/src $INSTALL_DIR/brt/

cp -r AndroidProjectPrototype $INSTALL_DIR/

echo "Install succeeded."
echo "  please add the path to PATH: $INSTALL_DIR/bin"

exit 0
