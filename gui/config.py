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

sal_front_end_code = "sal-wfc-front-end.scm lsal2xml-front-end.scm sal2bool-front-end.scm sal-path-finder-front-end.scm sal-smc-front-end.scm sal-bmc-core-front-end.scm sal-bmc-front-end.scm sal-inf-bmc-front-end.scm sal-script-util.scm sal-deadlock-checker-front-end.scm sal-simulator-front-end.scm sal-wmc-front-end.scm ltl2buchi-front-end.scm sal-emc-front-end.scm sal-path-explorer-front-end.scm sal-atg-core.scm sal-atg-front-end.scm sal-atg-core2.scm"
sal_scripts = "salenv salenv-safe sal-wfc lsal2xml sal2bool sal-smc sal-bmc sal-inf-bmc sal-path-finder sal-deadlock-checker sal-sim sal-wmc ltl2buchi sal-emc sal-path-explorer sal-atg sal-atg2 sal-sld sal-sc"
salenv_exec = "salenv-exec"
ARCH = "arm-apple-darwin24.6.0"
SALENV_BUILD_MODE= "release"
EXEEXT=""
ICS_BIN_DIR=""

bindir="../bin/%s-%s" % (ARCH, SALENV_BUILD_MODE)

def GetFrontEndCode(): 
		return sal_front_end_code.split(' ')
def GetScripts(): 
		return sal_scripts.split(' ')
def GetBinDir():
    return bindir
def GetICSBinDir():
    return ICS_BIN_DIR
def GetSALExec():
    return salenv_exec
def GetExeExt():
    return EXEEXT
