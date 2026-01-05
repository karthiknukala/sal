#! /bin/sh

#
# SAL 3.1, Copyright (C) 2006, 2011, SRI International.  All Rights Reserved.
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
# This script creates SAL directory structure for source distribution
#
salenv_version=3.3

SALENVDIR="sal-"$salenv_version
SRC_DISTRIB_FILE="sal-"$salenv_version"-src.tar.gz"

rm -r -f $SALENVDIR
rm -f $SRC_DISTRIB_FILE

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
make_dir $SALENVDIR/distribution
make_dir $SALENVDIR/install
make_dir $SALENVDIR/install/mac
make_dir $SALENVDIR/gui

cp README $SALENVDIR/
cp LICENSE $SALENVDIR/
cp NOTICES $SALENVDIR/
cp WHATS_NEW $SALENVDIR/

cp install/source-INSTALL $SALENVDIR/INSTALL.txt
cp Makefile.in $SALENVDIR/
cp configure.in $SALENVDIR/
cp configure $SALENVDIR/
cp config.guess $SALENVDIR
cp os $SALENVDIR

echo "Copying documentation..."
cp doc/README* $SALENVDIR/doc
cp doc/Limitations.txt $SALENVDIR/doc
cp doc/sal_tutorial.ps $SALENVDIR/doc
cp doc/sal_tutorial.pdf $SALENVDIR/doc
cp doc/*.in $SALENVDIR/doc
cp doc/sal_tutorial.tex $SALENVDIR/doc

echo "Copying examples..."
cp -R examples/* $SALENVDIR/examples
(cd $SALENVDIR/examples; find . -name 'CVS' -exec rm -r -f '{}' ';')
(cd $SALENVDIR/examples; find . -name '.svn' -exec rm -r -f '{}' ';')

echo "Copying contributions..."
cp contrib/* $SALENVDIR/contrib

echo "Copying etc..."
cp etc/* $SALENVDIR/etc

echo "Copying ics sources.."
cp ics-2.0c-src.tar.gz $SALENVDIR/etc

echo "Copying macros..."
cp src/*.macros $SALENVDIR/src
cp src/*.in $SALENVDIR/src

echo "Copying source code..."
cp src/*.scm $SALENVDIR/src
cp src/*.sch $SALENVDIR/src
cp src/*.c $SALENVDIR/src
cp src/*.h $SALENVDIR/src
cp src/*.scm.on $SALENVDIR/src
cp src/*.scm.off $SALENVDIR/src
cp src/*.template $SALENVDIR/src
cp src/.afile $SALENVDIR/src

echo "Copying distribution scripts"
cp distribution/*.in $SALENVDIR/distribution

echo "Copying installation scripts"
cp install/*.in $SALENVDIR/install
cp install/mac/*.in $SALENVDIR/install/mac

echo "Copying ltllib..."
cp src/ltllib.lsal $SALENVDIR/src

echo "Copying gui..."
cp gui/*.in $SALENVDIR/gui
# TODO: copy python files

echo "Generating tar file..."
chmod -R og+rX $SALENVDIR
tar -cvzf $SRC_DISTRIB_FILE $SALENVDIR

