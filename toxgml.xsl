<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output method="xml" indent="yes"/>

    <xsl:template match="/">
        <section name="xgml">
            <attribute key="Creator" type="String">SPREAD</attribute>
            <attribute key="Version" type="String">0.1</attribute>
            <section name="graph">
                <xsl:apply-templates select="//node" mode="node"/>
                <xsl:apply-templates select="//node" mode="edge"/>
            </section>
        </section>
    </xsl:template>

    <xsl:template match="node" mode="node">
        <section name="node">
            <attribute key="id" type="int"><xsl:value-of select="@id"/></attribute>
            <xsl:apply-templates select="*" mode="node"/>
        </section>
    </xsl:template>

    <xsl:template match="node" mode="edge">
        <xsl:apply-templates select="*" mode="edge"/>
    </xsl:template>

    <!-- node mapping -->
    <xsl:template match="trace" mode="node">
        <attribute key="label" type="String"><xsl:value-of select="concat('trace: ',from/@id,':',to/@id)"/></attribute>
    </xsl:template>

    <xsl:template match="content" mode="node">
        <attribute key="label" type="String"><xsl:value-of select="@value"/></attribute>
    </xsl:template>

    <xsl:template match="lazy1" mode="node">
        <attribute key="label" type="String"><xsl:value-of select="concat('f1:',function/@name)"/></attribute>
    </xsl:template>

    <xsl:template match="lazy2" mode="node">
        <attribute key="label" type="String"><xsl:value-of select="concat('f2:',function/@name)"/></attribute>
    </xsl:template>

    <xsl:template match="lazy3" mode="node">
        <attribute key="label" type="String"><xsl:value-of select="concat('f3:',function/@name)"/></attribute>
    </xsl:template>



    <!-- edge mapping -->
    <xsl:template match="trace" mode="edge">
        <section name="edge">
            <attribute key="source" type="int"><xsl:value-of select="../@id"/></attribute>
            <attribute key="target" type="int"><xsl:value-of select="from/@id"/></attribute>
            <attribute key="label" type="String">from</attribute>
        </section>
        <section name="edge">
            <attribute key="source" type="int"><xsl:value-of select="../@id"/></attribute>
            <attribute key="target" type="int"><xsl:value-of select="to/@id"/></attribute>
            <attribute key="label" type="String">to</attribute>
        </section>
    </xsl:template>

    <xsl:template match="lazy1" mode="edge">
        <section name="edge">
            <attribute key="source" type="int"><xsl:value-of select="../@id"/></attribute>
            <attribute key="target" type="int"><xsl:value-of select="arg1/@id"/></attribute>
            <attribute key="label" type="String">arg1</attribute>
        </section>
    </xsl:template>

    <xsl:template match="lazy2" mode="edge">
        <section name="edge">
            <attribute key="source" type="int"><xsl:value-of select="../@id"/></attribute>
            <attribute key="target" type="int"><xsl:value-of select="arg1/@id"/></attribute>
            <attribute key="label" type="String">arg1</attribute>
        </section>
        <section name="edge">
            <attribute key="source" type="int"><xsl:value-of select="../@id"/></attribute>
            <attribute key="target" type="int"><xsl:value-of select="arg2/@id"/></attribute>
            <attribute key="label" type="String">arg2</attribute>
        </section>
    </xsl:template>

    <xsl:template match="lazy3" mode="edge">
        <section name="edge">
            <attribute key="source" type="int"><xsl:value-of select="../@id"/></attribute>
            <attribute key="target" type="int"><xsl:value-of select="arg1/@id"/></attribute>
            <attribute key="label" type="String">arg1</attribute>
        </section>
        <section name="edge">
            <attribute key="source" type="int"><xsl:value-of select="../@id"/></attribute>
            <attribute key="target" type="int"><xsl:value-of select="arg2/@id"/></attribute>
            <attribute key="label" type="String">arg2</attribute>
        </section>
        <section name="edge">
            <attribute key="source" type="int"><xsl:value-of select="../@id"/></attribute>
            <attribute key="target" type="int"><xsl:value-of select="arg3/@id"/></attribute>
            <attribute key="label" type="String">arg3</attribute>
        </section>
    </xsl:template>

</xsl:stylesheet>