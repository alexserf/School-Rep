<?xml version="1.0" encoding="ISO-8859-1"?>
<xsl:stylesheet version="1.0"
  xmlns:xsl="https://www.w3.org/1999/XSL/Transform">
 <xsl:template match="/">
 <html>
 <body>
 <table border="1">
 <tr>
 <td>Étudiant </td>
 <td> Moyenne </td>
 </tr>
 <tr>
 <td><xsl:value-of select="//etudiant/@nom" /> </td>
 <td>
   <xsl:value-of select='sum(//cours)'/>
 </td>
 </tr>
 </table>
 </body>
 </html>
 </xsl:template>
</xsl:stylesheet>
