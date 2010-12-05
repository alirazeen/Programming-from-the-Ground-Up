DOCBOOKDSL = /sw/share/sgml/dsssl/docbook-dsssl-nwalsh/html/docbook.dsl
CHAPTERS = AssemblyIntro.xml
APPENDICES = DeveloperWorks.xml
PROGS = 
GRAPHICS = 
BOOKNOINDEX = PTB.xml PTB.dsl $(CHAPTERS) $(PROGS) $(GRAPHICS) $(APPENDICES)
FULLBOOK = $(BOOKNOINDEX)
JADE=openjade
INDEX = index.xml
XMLDCL=/sw/share/sgml/openjade-1.3.2/pubtext/xml.dcl


all: PTB.pdf

PTB.rtf: $(FULLBOOK)
	$(JADE) -t rtf -d pgupower5.dsl#printdraft $(XMLDCL) PTB.xml

PTB.pdf: PTB.tex
	pdfjadetex PTB.tex

PTB.tex: $(FULLBOOK)
	$(JADE) -t tex -d pgupower5.dsl#printdraft $(XMLDCL) PTB.xml  

HTML.index: $(BOOKNOINDEX)
	collateindex.pl -N > index.xml
	$(JADE) -t sgml -V html-index -V nochunks -d $(DOCBOOKDSL) $(XMLDCL) PTB.xml > /dev/null
	rm -f *.htm *.html

index: index.xml

index.xml: HTML.index
	collateindex.pl -o index.xml HTML.index
	#./fixindex.pl

semiclean:
	rm -f PTB.tex PTB.pdf PTB.log PTB.aux PTB.dvi PTB.ps

clean: semiclean
	rm -f HTML.index index.xml

html:
	java org.apache.xalan.xslt.Process -IN AssemblyIntro-DW.xml -XSL developerworks/xsl/dw-document-html-4.0.xsl -OUT AssemblyIntro-DW.html
	java org.apache.xalan.xslt.Process -IN AddressingModes-DW.xml -XSL developerworks/xsl/dw-document-html-4.0.xsl -OUT AddressingModes-DW.html

