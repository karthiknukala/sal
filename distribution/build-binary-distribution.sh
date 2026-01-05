#!/bin/sh

#
# SAL 3.1 Copyright (C) 2006, 2011 SRI International.  All Rights Reserved.
#
# SAL is free software; you can redistribute it and/or 
# modify it under the terms of the GNU General Public License 
# as published by the Free Software Foundation; either version 2
# of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
# GNU General Public License for more details. 
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software 
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. 
#
#


#
# This script creates SAL binary distribution package
#
posixos=darwin
salenv_version=3.3
BIGLOOLIBDIR="/opt/homebrew/Cellar/bigloo/4.6a/lib/bigloo/4.6a"
SALENV_STATIC="no"
ARCH="arm-apple-darwin24.6.0"
SALENV_BUILD_MODE="release"
DOT_A_LIBS=" /usr/local/lib/libcudd.a /opt/homebrew/lib/libgmp.a "
EXEEXT=
ICS_BIN_DIR=
YICES_BIN_DIR=/opt/homebrew/bin
BUILD_SUBDIR=$ARCH"-"$SALENV_BUILD_MODE
SALENVDIR="sal-"$salenv_version
curr_dir=`pwd`
LIB_DIR=$curr_dir/lib/$BUILD_SUBDIR
SAL_BIN_DISTRIB_FILE="sal-"$salenv_version"-bin-"$ARCH".tar.gz"
USE_YICES=yes
USE_ICS=no

rm -r -f $SALENVDIR
rm -f $SAL_BIN_DISTRIB_FILE
 
make_dir ()
{
    if mkdir -p $1
    then
        echo "Directory $1 was created."
    else
        echo "Error: Failed to create directory \"$1\"."
        exit -1
    fi
}

make_dir $SALENVDIR
make_dir $SALENVDIR/bin
make_dir $SALENVDIR/src
make_dir $SALENVDIR/lib
make_dir $SALENVDIR/examples
make_dir $SALENVDIR/contrib
make_dir $SALENVDIR/etc
make_dir $SALENVDIR/doc
make_dir $SALENVDIR/doc/man

safe_cp ()
{
   echo "Copying $1 ..."
   if cp $1 $2 2> /dev/null 
   then
       :
   else
       echo "Failed to copy $1 to $2"
       exit -1
   fi
}

if [ $SALENV_STATIC = "no" ]; then
   bigloo_exe=`which bigloo`$EXEEXT
   safe_cp $bigloo_exe $SALENVDIR/bin
   safe_cp $BIGLOOLIBDIR/bigloo.h $SALENVDIR/lib
   safe_cp $BIGLOOLIBDIR/bigloo.heap $SALENVDIR/lib
   safe_cp $BIGLOOLIBDIR/bigloo_config.h $SALENVDIR/lib
   safe_cp $LIB_DIR/sal.heap $SALENVDIR/lib
   cp $LIB_DIR/*.a $SALENVDIR/lib
   cp $BIGLOOLIBDIR/*.a $SALENVDIR/lib
   echo "Copying Required libraries..."
   if cp $DOT_A_LIBS $SALENVDIR/lib
   then
      echo "done"
   else
      echo "Failed to copy libraries: $LIBS" 
      exit -1
   fi
   ranlib $SALENVDIR/lib/*.a
fi


if test $USE_ICS = yes; then
    safe_cp $ICS_BIN_DIR/ics$EXEEXT $SALENVDIR/bin/ics$EXEEXT
    safe_cp empty-certificate $SALENVDIR/certificate
fi

if test $USE_YICES = yes; then
    safe_cp $YICES_BIN_DIR/yices$EXEEXT $SALENVDIR/bin
    safe_cp YICES-LICENSE $SALENVDIR
fi

safe_cp README $SALENVDIR/
safe_cp LICENSE $SALENVDIR/
safe_cp NOTICES $SALENVDIR/
safe_cp WHATS_NEW $SALENVDIR/

safe_cp install/bin-INSTALL $SALENVDIR/INSTALL.txt
safe_cp install/bin-install.sh $SALENVDIR/install.sh
chmod +x $SALENVDIR/install.sh

echo "Copying documentation..."
cp doc/README* $SALENVDIR/doc
safe_cp doc/Limitations.txt $SALENVDIR/doc
safe_cp doc/sal_tutorial.ps $SALENVDIR/doc
safe_cp doc/sal_tutorial.pdf $SALENVDIR/doc
safe_cp doc/salatg.pdf $SALENVDIR/doc

echo "Copying examples..."
cp -R examples/* $SALENVDIR/examples
(cd $SALENVDIR/examples; find . -name 'CVS' -exec rm -r -f '{}' ';')
(cd $SALENVDIR/examples; find . -name '.svn' -exec rm -r -f '{}' ';')

echo "Copying contributions..."
cp contrib/* $SALENVDIR/contrib

echo "Copying etc..."
cp etc/* $SALENVDIR/etc

bindir=bin/arm-apple-darwin24.6.0-release

echo "Copying executables..."
safe_cp $bindir/salenv-exec $SALENVDIR/bin
safe_cp $bindir/salenv-exec-safe $SALENVDIR/bin

echo "Copying macros..."
cp src/*.macros $SALENVDIR/src

safe_cp src/no-compilation-support-code.scm $SALENVDIR/src

safe_cp src/ltllib.lsal $SALENVDIR/src

echo "Creating man pages..."
for name in salenv salenv-safe sal-wfc lsal2xml sal2bool sal-smc sal-bmc sal-inf-bmc sal-path-finder sal-deadlock-checker sal-sim sal-wmc ltl2buchi sal-emc sal-path-explorer sal-atg sal-atg2 sal-sld sal-sc; do
  if [ $name != salenv ] && [ $name != salenv-safe ]; then
      help2man -N --include=doc/man-extra ./bin/$name > $SALENVDIR/doc/man/$name.1
  fi
done 

echo "Copying scripts..."
for name in sal-wfc-front-end.scm lsal2xml-front-end.scm sal2bool-front-end.scm sal-path-finder-front-end.scm sal-smc-front-end.scm sal-bmc-core-front-end.scm sal-bmc-front-end.scm sal-inf-bmc-front-end.scm sal-script-util.scm sal-deadlock-checker-front-end.scm sal-simulator-front-end.scm sal-wmc-front-end.scm ltl2buchi-front-end.scm sal-emc-front-end.scm sal-path-explorer-front-end.scm sal-atg-core.scm sal-atg-front-end.scm sal-atg-core2.scm; do cp src/$name $SALENVDIR/src; done
for name in salenv salenv-safe sal-wfc lsal2xml sal2bool sal-smc sal-bmc sal-inf-bmc sal-path-finder sal-deadlock-checker sal-sim sal-wmc ltl2buchi sal-emc sal-path-explorer sal-atg sal-atg2 sal-sld sal-sc; do cp src/$name.template $SALENVDIR/src; done

echo "Generating tar file..."
chmod -R og+rX $SALENVDIR
tar -cvzf $SAL_BIN_DISTRIB_FILE $SALENVDIR
    
