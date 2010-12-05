#!/usr/bin/perl -p

BEGIN {
	undef $/;
}

s!<remark>(.*?)</remark>!<\!-- $1 -->!si;
s!<\!DOCTYPE.*?>!!si;
s!<(chapter|article)>!<dw-document xsi:noNamespaceSchemaLocation="http://dw.raleigh.ibm.com/developerworks/library/schema/4.0/dw-document-4.0.xsd" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">\n\n<dw-article local-site="worldwide" ratings-form="auto" related-contents="auto" toc="auto" skill-level="3">\n<id cma-id="" domino-uid="" content-id="" original="yes"/>\n<keywords content="FIXME -- ADD KEYWORDS" />\n\n<\!-- FIXME - update meta -->\n<meta-last-updated day="16" month="10" year="2004" initials="jlb"/>\n\n<content-area-primary name="linux" />\n!si;

s!</title>\s*<para>(.*?)</para>!</title><abstract>$1</abstract><docbody>!si;
s!</title>!</title>\n\n<author jobtitle="Director of Technology" company="New Medio" email="johnnyb\@eskimo.com"  >\n<bio>Jonathan Bartlett is the author of the book <a href="http://www.cafeshops.com/bartlettpublish.8640017"><i>Programming from the Ground Up</i></a> which is an introduction to programming using Linux assembly language.  He is the lead developer at New Medio, developing web, video, kiosk, and desktop applications for clients.\n</bio>\n<name>Jonathan Bartlett</name>\n</author>\n\n<\!-- FIXME - update date published -->\n<date-published day="01" month="09" year="2004" />!si;

s!<table>\s*<title>(.*?)</title>!<heading refname="" type="minor" toc="no">$1</heading>\n<table>!gsi;
s!<variablelist>\s*<title>(.*?)</title>!<heading refname="" type="minor" toc="no">$1</heading>\n<dl>!gsi;
s!<variablelist>!<dl>!gsi;
s!</variablelist>!</dl>!gsi;
s!<varlistentry>!!gsi;
s!<term>!<dt>!gsi;
s!</term>\s*<listitem>\s*<para>(.*?)</para>\s*</listitem>\s*</varlistentry>!</dt><dd><p>$1</p></dd>!gsi;
s!</term>!</dt>!gsi;
s!<listitem>\s*<para>!<li>!gsi;
s!</para>\s*</listitem>!</li>!gsi;
s!<row>!<tr>!gsi;
s!<entry>!<td>!gsi;
s!</row>!</tr>!gsi;
s!</entry>!</td>!gsi;
s!<(/)?emphasis>!<${1}i>!gsi;
s!<sect1>\s*<title>(.*?)</title>!<heading refname="" type="major" toc="yes" alttoc="">${1}</heading>!gsi;
s!</sect1>!!gsi;
s!<sect2>\s*<title>(.*?)</title>!<heading refname="" type="minor" toc="no" alttoc="">${1}</heading>!gsi;
s!</sect2>!!gsi;
s!<sect3>\s*<title>(.*?)</title>!<heading refname="" type="minor" toc="no" alttoc="">${1}</heading>!gsi;
s!</sect3>!!gsi;
s!<(/)?itemizedlist>!<${1}ul>!gsi;
s!<(/)?orderedlist>!<${1}ol>!gsi;
$lnum = 0;
sub make_code_section
{
	my ($heading, $code) = @_;

	$lnum++;
	return (<<EOF);
<code type="section">\n<heading refname="" type="code" toc="no">Listing ${lnum}. ${heading}</heading>${code}</code>
EOF
}

$fnum = 0;
sub make_figure
{
	my ($title, $image_name) = @_;

	$title =~ s!</?(i|emphasis)>!!gsi;
	$fnum++;
	return(<<EOF);
<figure>
<heading refname="" type="figure" toc="no" name="" alttoc="">Figure ${fnum}. ${title}</heading>
<img src="${image_name}" alt="${title}"/>
</figure>
EOF
}
s!<example>\s*<title>(.*?)</title>\s*<programlisting>(.*?)</programlisting>\s*</example>!make_code_section($1, $2)!gsie;
s!<programlisting>(.*?)</programlisting>!<code type="section">$1</code>!gsi;
s!<mediaobject>\s*<imageobject>\s*<imagedata\s*fileref=["'](.*?)['"].*?>\s*</imageobject>\s*<caption>\s*<para>\s*(.*?)\s*</para>\s*</caption>\s*</mediaobject>!make_figure($2, $1)!gsie;
s!<(/)?para>!<${1}p>!gsi;
s!<literal>!<code type="inline">!gsi;
s!</literal>!</code>!gsi;
s!<ulink\s+url=['"](.*?)['"]\s*>(.*?)</ulink>!<a href="${1}">${2}</a>!gsi;

s!</(chapter|article)>!</docbody>\n<related-list>\n</related-list>\n\n<resource-list>\n</resource-list>\n\n</dw-article>\n</dw-document>\n!si;

