<?xml version="1.0" encoding="utf-8"?>
<xsl:stylesheet version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:output method="html"/>

  <xsl:template match="wrapper">
    <xsl:apply-templates select="document(./@source)"/>
  </xsl:template>

  <xsl:template match="//ValCurs">
    <xsl:for-each
	select="Valute[contains(Name, 'Доллар США') or contains(Name, 'Евро')]">
      <p>
	<xsl:value-of select="CharCode"/>
	<xsl:text> </xsl:text>
	<xsl:value-of select="Value"/>
      </p>
    </xsl:for-each>
  </xsl:template>
</xsl:stylesheet>
