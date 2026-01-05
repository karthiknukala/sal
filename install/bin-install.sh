#!/bin/sh
#
# This script is used to install SAL in the binary distribution
#
DOT_A_LIB_NAMES=" libcudd.a libgmp.a"

echo "Installing SAL (version 3.3). Copyright (c) SRI International 2006, 2011."
echo "-------------------------------------------------------------------------------"

salenv_dir=`pwd`
echo "Installation summary:"
echo "  SAL directory: $salenv_dir"
warning=0

DOT_A_LIBS=""
for lib in $DOT_A_LIB_NAMES
do
  DOT_A_LIBS="$DOT_A_LIBS $salenv_dir/lib/$lib"
done

gen_script ()
{
  template=src/$1.template
  script=$salenv_dir/bin/$1
  if sed -e "s|__SALENV_DIR__|$salenv_dir|g;s|__BIGLOO_LIB_DIR__|$salenv_dir/lib|g;s|__DOT_A_LIBS__|$DOT_A_LIBS|g" $template > $script
  then
    chmod +x $script
  else
     echo "Error: generating script $script" 1>&2
     exit 1
  fi
}

echo "Generating auxiliary scripts..."
for name in salenv salenv-safe sal-wfc lsal2xml sal2bool sal-smc sal-bmc sal-inf-bmc sal-path-finder sal-deadlock-checker sal-sim sal-wmc ltl2buchi sal-emc sal-path-explorer sal-atg sal-atg2 sal-sld sal-sc; do gen_script $name; done

if bin/sal-smc --version 2>&1 | grep GLIBC; then
    echo "Your system contains an incompatible version of GLIBC. SAL requires GLIBC version >= 2.3. You should upgrade your system, or download the SAL static distribution package for your plataform."
    exit -1
fi

echo "Installation completed!"
echo "It is also a good idea to add the directory $salenv_dir/bin to your command search PATH environment variable."

