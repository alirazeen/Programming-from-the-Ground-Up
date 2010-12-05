<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN" [
<!ENTITY dbstyle-print SYSTEM "/usr/share/sgml/docbook/dsssl-stylesheets/print/docbook.dsl" CDATA DSSSL>
<!ENTITY dbstyle-html SYSTEM "/usr/share/sgml/docbook/dsssl-stylesheets/html/docbook.dsl" CDATA DSSSL>
]>

<style-sheet>
<style-specification use="docbook-html" id="html">
<style-specification-body>

(define %footnote-ulinks% #f)
(define show-comments #f)

</style-specification-body>
</style-specification>
<style-specification use="docbook-print" id="printdraft">
<style-specification-body>

;(define %page-width% 8.25in)
;(define %page-height% 10.75in)
(define %footnote-ulinks% #f)
(define %two-side% #t)
(define tex-backend #t)
(define bop-footnotes #t)
(define show-comments #t)

</style-specification-body>
</style-specification>
<style-specification use="docbook-print" id="print">
<style-specification-body>

(define %page-width% 8.25in)
(define %page-height% 10.25in)
(define %footnote-ulinks% #f)
(define %two-side% #t)
(define tex-backend #t)
(define bop-footnotes #t)
(define show-comments #f)

</style-specification-body>
</style-specification>
<external-specification id="docbook-print" document="dbstyle-print">
<external-specification id="docbook-html" document="dbstyle-html">
</style-sheet>
