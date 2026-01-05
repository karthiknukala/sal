#
# SAL 3.1 Copyright (C) 2006, 2011, SRI International.  All Rights Reserved.
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

MAKEINFO=makeinfo
MAKEINFOOPT=-U oldinfo
TEXI2DVI=texi2dvi
TEXI2DVIOPT=-b
TEXI2HTML=texi2html
TEXI2HTMLOPT=-menu -monolithic -number -expandinfo
TEXI2PDF=texi2pdf

#*---------------------------------------------------------------------*/
#*    Default configuration                                            */
#*---------------------------------------------------------------------*/

DVIPS		= dvips
MAKEDVI		= export -n TEXINPUTS; $(TEXI2DVI) $(TEXI2DVIOPT)

API_MASTER		= salenv_api.texi
API_SOURCES		= salenv_api_body.texi

.PHONY: all api_info api_ps api_html api_pdf

#*---------------------------------------------------------------------*/
#*    Everything ...                                                   */
#*---------------------------------------------------------------------*/
all: api_info api_ps api_html api_pdf

api_dvi: salenv_api.dvi
salenv_api.dvi: $(API_MASTER) $(API_SOURCES)
	($(MAKEDVI) $(API_MASTER)) || $(TEXI2DVI) $(TEXI2DVIOPT) $(API_MASTER)

api_ps: salenv_api.ps
salenv_api.ps: salenv_api.dvi
	$(DVIPS) -o salenv_api.ps salenv_api.dvi

api_info: salenv_api.info
salenv_api.info: $(API_MASTER) $(API_SOURCES)
	$(MAKEINFO) $(INFOOPTION) $(API_MASTER)

#*---------------------------------------------------------------------*/
#*    html                                                             */
#*---------------------------------------------------------------------*/
api_html: salenv_api.html
salenv_api.html: $(API_MASTER) $(API_SOURCES)
	$(TEXI2HTML) $(TEXI2HTMLOPT) $(API_MASTER)
	sed -e "s|</HEAD>|</HEAD><BODY bgcolor=\"#efefef\" text=\"#000000\" link=\"#0000ff\" vlink=\"#551a8b\" alink=\"#ff0000\" background=\"\">|g" salenv_api.html > salenv_api.html.tmp
	mv salenv_api.html.tmp salenv_api.html
	rm -f salenv_api.html.tmp


#*---------------------------------------------------------------------*/
#*    pdf                                                              */
#*---------------------------------------------------------------------*/
api_pdf: salenv_api.pdf
salenv_api.pdf: $(API_MASTER) $(API_SOURCES)
	$(TEXI2PDF) $(TEXI2PDFOPT) $(API_MASTER)

#*---------------------------------------------------------------------*/
#*    touchall                                                         */
#*---------------------------------------------------------------------*/
touchall:
	touch $(API_MASTER) $(API_SOURCES)


#*---------------------------------------------------------------------*/
#*    Cleaning ...                                                     */
#*---------------------------------------------------------------------*/
.PHONY: clean cleanall distclean

clean:
	-rm -f *.cp *.cps *.fn *.fns *.ky *.op *.pg *.tp *.vr *.toc *.log *.aux
	find . \( -name '*[~%]' \
                     -o -name '.??*[~%]' \
                     -o -name '#*#' \
                     -o -name '?*#' \
                     -o -name \*core \) \
                     -type f -exec rm {} \;   

distclean:
	-rm -f salenv.dvi
	-rm -f salenv.ps
	-rm -f salenv.html
	-rm -f salenv.pdf
	-rm -f salenv_toc.html
	-rm -f salenv_foot.html
	-rm -f salenv_api.dvi
	-rm -f salenv_api.ps
	-rm -f salenv_api.html
	-rm -f salenv_api.pdf
	-rm -f salenv_api_toc.html
	-rm -f salenv_api_foot.html


cleanall: distclean
	-rm -f salenv_api.info*

#*---------------------------------------------------------------------*/
#* Internal Distribution                                               */
#*---------------------------------------------------------------------*/
internal-distribution: info pdf ps html $(INTERNAL_SALENV_DISTRIBUTION_PATH)/info $(INTERNAL_SALENV_DISTRIBUTION_PATH)/manual
	cp *.info* $(INTERNAL_SALENV_DISTRIBUTION_PATH)/info
	cp *.html $(INTERNAL_SALENV_DISTRIBUTION_PATH)/manual
	cp *.ps $(INTERNAL_SALENV_DISTRIBUTION_PATH)/manual
	cp *.pdf $(INTERNAL_SALENV_DISTRIBUTION_PATH)/manual

$(INTERNAL_SALENV_DISTRIBUTION_PATH)/info:
	mkdir -p  $(INTERNAL_SALENV_DISTRIBUTION_PATH)/info

$(INTERNAL_SALENV_DISTRIBUTION_PATH)/manual:
	mkdir -p  $(INTERNAL_SALENV_DISTRIBUTION_PATH)/manual

#*---------------------------------------------------------------------*/
#* Internal Homepage                                                   */
#*---------------------------------------------------------------------*/
internal-homepage: info pdf ps html $(INTERNAL_HOMEPAGE) $(INTERNAL_HOMEPAGE)/manual
	cp *.html $(INTERNAL_HOMEPAGE)/manual 
	cp *.ps $(INTERNAL_HOMEPAGE)/manual
	cp *.pdf $(INTERNAL_HOMEPAGE)/manual
	cp *.info $(INTERNAL_HOMEPAGE)/manual

$(INTERNAL_HOMEPAGE):
	mkdir -p  $(INTERNAL_HOMEPAGE)

$(INTERNAL_HOMEPAGE)/manual:
	mkdir -p  $(INTERNAL_HOMEPAGE)/manual

HOMEPAGE_MANUAL=../$(HOMEPAGE)/manual

#*---------------------------------------------------------------------*/
#* External Homepage                                                   */
#*---------------------------------------------------------------------*/
update-home-page: info pdf ps html $(HOMEPAGE_MANUAL)
	cp *.html $(HOMEPAGE_MANUAL)
	cp *.ps $(HOMEPAGE_MANUAL)
	cp *.pdf $(HOMEPAGE_MANUAL)
	cp *.info $(HOMEPAGE_MANUAL)

$(HOMEPAGE_MANUAL):
	mkdir -p  $(HOMEPAGE_MANUAL)

#*---------------------------------------------------------------------*/
#* External Distribution                                               */
#*---------------------------------------------------------------------*/
DISTRIBUTION_DOC_DIR=$(DISTRIBUTION_DIR)/doc
distribution: info ps html pdf
	mkdir -p  $(DISTRIBUTION_DOC_DIR)
	cp *.html $(DISTRIBUTION_DOC_DIR)
	cp *.ps $(DISTRIBUTION_DOC_DIR)
	cp *.pdf $(DISTRIBUTION_DOC_DIR)
	cp *.info $(DISTRIBUTION_DOC_DIR)
	cp Makefile.in $(DISTRIBUTION_DOC_DIR)
