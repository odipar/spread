<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output method="xml" indent="yes"/>

    <xsl:template match="/">
        <section name="xgml">
            <attribute key="Creator" type="String">SPREAD</attribute>
            <attribute key="Version" type="String">0.1</attribute>
            <section name="graph">
                <xsl:apply-templates select="//group"/>
                <xsl:apply-templates select="//node" mode="node"/>
                <xsl:apply-templates select="//node" mode="edge"/>
            </section>
        </section>
    </xsl:template>

    <xsl:template match="node" mode="node">
        <xsl:param name="gid"/>
        <section name="node">
            <attribute key="id" type="int"><xsl:value-of select="@id"/></attribute>
            <xsl:apply-templates select="*" mode="node"/>
            <attribute key="gid" type="int"><xsl:value-of select="@group"/></attribute>
        </section>
    </xsl:template>

    <xsl:template match="node" mode="edge">
        <xsl:param name="gid"/>
        <xsl:apply-templates select="*" mode="edge"/>
    </xsl:template>

    <xsl:template match="trace" mode="node">
        <xsl:param name="gid"/>
        <xsl:call-template name="rectangle">
            <xsl:with-param name="label" select="''"/>
            <xsl:with-param name="fill" select="'#66ccff'"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="content" mode="node">
        <xsl:param name="gid"/>
        <xsl:call-template name="octagon">
            <xsl:with-param name="label" select="@value"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="lazy1" mode="node">
        <xsl:param name="gid"/>
        <xsl:call-template name="ellipse">
            <xsl:with-param name="label" select="function/@name"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="lazy2" mode="node">
        <xsl:param name="gid"/>
        <xsl:call-template name="ellipse">
            <xsl:with-param name="label" select="function/@name"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="lazy3" mode="node">
        <xsl:param name="gid"/>
        <xsl:call-template name="ellipse">
            <xsl:with-param name="label" select="function/@name"/>
        </xsl:call-template>
    </xsl:template>

    <xsl:template match="group">
        <xsl:param name="gid"/>

        <section name="node">
            <attribute key="id" type="int"><xsl:value-of select="@id"/></attribute>
            <xsl:call-template name="rectangle">
                <xsl:with-param name="fill" select="'#FFFFFF'"/>
                <xsl:with-param name="label" select="@id"/>
            </xsl:call-template>
            <attribute key="isGroup" type="boolean">true</attribute>
            <attribute key="gid" type="int"><xsl:value-of select="@gid"/></attribute>
        </section>
    </xsl:template>

    <xsl:template name="ellipse">
        <xsl:param name="gid"/>
        <xsl:param name="label"/>

        <section name="graphics">
            <attribute key="w" type="double">50.0</attribute>
            <attribute key="h" type="double">50.0</attribute>
            <attribute key="type" type="String">ellipse</attribute>
            <attribute key="fill" type="String">#C0C0C0</attribute>
            <attribute key="outline" type="String">#000000</attribute>
        </section>
        <section name="LabelGraphics">
            <attribute key="text" type="String"><xsl:value-of select="$label"/></attribute>
            <attribute key="fontSize" type="int">14</attribute>
            <attribute key="fontStyle" type="String">bold</attribute>
            <attribute key="fontName" type="String">Courier New</attribute>
            <attribute key="anchor" type="String">c</attribute>
        </section>
    </xsl:template>

    <xsl:template name="rectangle">
        <xsl:param name="gid"/>
        <xsl:param name="label"/>
        <xsl:param name="fill"/>

        <section name="graphics">
            <attribute key="w" type="double">40.0</attribute>
            <attribute key="h" type="double">40.0</attribute>
            <attribute key="type" type="String">rectangle</attribute>
            <attribute key="fill" type="String"><xsl:value-of select="$fill"/></attribute>
            <attribute key="outline" type="String">#000000</attribute>
        </section>
        <section name="LabelGraphics">
            <attribute key="text" type="String"><xsl:value-of select="$label"/></attribute>
            <attribute key="fontSize" type="int">12</attribute>
            <attribute key="fontName" type="String">Dialog</attribute>
            <attribute key="model"/>
        </section>
    </xsl:template>

    <xsl:template name="diamond">
        <xsl:param name="gid"/>
        <xsl:param name="label"/>

        <section name="graphics">
            <attribute key="w" type="double">50.0</attribute>
            <attribute key="h" type="double">50.0</attribute>
            <attribute key="type" type="String">diamond</attribute>
            <attribute key="fill" type="String">#ff99cc</attribute>
            <attribute key="outline" type="String">#000000</attribute>
        </section>
        <section name="LabelGraphics">
            <attribute key="text" type="String"><xsl:value-of select="$label"/></attribute>
            <attribute key="fontSize" type="int">13</attribute>
            <attribute key="fontStyle" type="String">bold</attribute>
            <attribute key="fontName" type="String">Courier New</attribute>
            <attribute key="anchor" type="String">c</attribute>
        </section>
    </xsl:template>

    <xsl:template name="octagon">
        <xsl:param name="gid"/>
        <xsl:param name="label"/>

            <section name="graphics">
                <attribute key="w" type="double">50.0</attribute>
                <attribute key="h" type="double">50.0</attribute>
                <attribute key="type" type="String">octagon</attribute>
                <attribute key="fill" type="String">#66FF66</attribute>
                <attribute key="outline" type="String">#000000</attribute>
            </section>
            <section name="LabelGraphics">
                <attribute key="text" type="String"><xsl:value-of select="$label"/></attribute>
                <attribute key="fontSize" type="int">13</attribute>
                <attribute key="fontStyle" type="String">bold</attribute>
                <attribute key="fontName" type="String">Courier New</attribute>
                <attribute key="anchor" type="String">c</attribute>
            </section>
    </xsl:template>

    <xsl:template match="trace" mode="edge">
        <xsl:param name="gid"/>
        <xsl:call-template name="trace">
        </xsl:call-template>
    </xsl:template>


    <xsl:template name="trace">
        <xsl:param name="gid"/>

        <xsl:param name="from" select="'from'"/>
        <section name="edge">
            <attribute key="source" type="int"><xsl:value-of select="../@id"/></attribute>
            <attribute key="target" type="int"><xsl:value-of select="to/@id"/></attribute>
            <attribute key="label" type="String">to</attribute>
        </section>
        <section name="edge">
            <attribute key="source" type="int"><xsl:value-of select="../@id"/></attribute>
            <attribute key="target" type="int"><xsl:value-of select="from/@id"/></attribute>
            <attribute key="label" type="String">from</attribute>
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
        <xsl:param name="gid"/>

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
        <xsl:param name="gid"/>
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