# Decorate Module Output

## Introduction

The outputs produced by `teal` modules, like graphs or tables, are
created by the module developer and look a certain way. It is hard to
design an output that will satisfy every possible user, so the form of
the output should be considered a default value that can be customized.
Here we describe the concept of *decoration*, enabling the app developer
to tailor outputs to their specific requirements without rewriting the
original module code.

The decoration process is build upon transformation procedures,
introduced in `teal`. While `transformators` are meant to edit module’s
input, decorators are meant to adjust the module’s output. To
distinguish the difference, modules in `teal.osprey` have 2 separate
parameters: `transformators` and `decorators`.

To get a complete understanding refer the following vignettes:

- Transforming the input data in [this
  vignette](https://insightsengineering.github.io/teal/latest-tag/articles/transform-input-data.html).
- Transforming module output in [this
  vignette](https://insightsengineering.github.io/teal/latest-tag/articles/transform-module-output.html).

## Outputs that can be decorated

It is important to note which output objects from a given module can be
decorated. The module function documentation’s *Decorating Module*
section has this information.

You can also refer the table shown below to know which module outputs
can be decorated.

| Module                 | Output (Class)         |
|------------------------|------------------------|
| `tm_g_spiderplot`      | plot `ggplot`          |
| `tm_g_butterfly`       | plot `grob` / `gtable` |
| `tm_g_waterfall`       | plot `grob` / `gtable` |
| `tm_g_swimlane`        | plot `grob` / `gtable` |
| `tm_g_patient_profile` | plot `grob` / `ggplot` |
| `tm_g_ae_oview`        | plot `grob`            |
| `tm_g_ae_sub`          | plot `grob`            |
| `tm_g_events_term_id`  | plot `grob`            |
| `tm_g_heat_bygrade`    | plot `grob` / `gtable` |

Also, note that there are three different types of objects that can be
decorated:

1.  `ggplot`
2.  `grob`
3.  `gtable`

## Decorating `ggplot`

Here’s an example to showcase how you can edit an output of class
`ggplot`. You can extend them using `ggplot2` functions.

[`library`](https://rdrr.io/r/base/library.html)`(`[`teal.osprey`](https://insightsengineering.github.io/teal.osprey/)`)`` `` ``data`` ``<-`` `[`within`](https://rdrr.io/r/base/with.html)`(``teal_data``(``)``, ``{`` `` `[`require`](https://rdrr.io/r/base/library.html)`(`[`nestcolor`](https://insightsengineering.github.io/nestcolor/)`)`` `` ``ADSL`` ``<-`` ``rADSL`` `` ``ADTR`` ``<-`` ``rADTR`` ``}``)`` `` ``join_keys``(``data``)`` ``<-`` ``default_cdisc_join_keys``[`[`names`](https://rdrr.io/r/base/names.html)`(``data``)``]`` `` ``ggplot_caption_decorator`` ``<-`` ``function``(``default_caption`` ``=`` ``"I am a good decorator"``)`` ``{`` `` ``teal_transform_module``(`` `` label ``=`` ``"Caption"``,`` `` ui ``=`` ``function``(``id``)`` ``{`` `` ``shiny``::`[`textInput`](https://rdrr.io/pkg/shiny/man/textInput.html)`(``shiny``::`[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``, ``"footnote"``)``, ``"Footnote"``, value ``=`` ``default_caption``)`` `` ``}``,`` `` server ``=`` ``function``(``id``, ``data``)`` ``{`` `` ``moduleServer``(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`` `` ``reactive``(``{`` `` `[`data`](https://rdrr.io/r/utils/data.html)`(``)`` ``|>`` `` `[`within`](https://rdrr.io/r/base/with.html)`(`` `` ``{`` `` ``plot`` ``<-`` ``plot`` ``+`` ``ggplot2``::`[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(``caption ``=`` ``footnote``)`` `` ``}``,`` `` footnote ``=`` ``input``$``footnote`` `` ``)`` `` ``}``)`` `` ``}``)`` `` ``}`` `` ``)`` ``}`` `` ``app`` ``<-`` ``init``(`` `` data ``=`` ``data``,`` `` modules ``=`` ``modules``(`` `` `[`tm_g_spiderplot`](https://insightsengineering.github.io/teal.osprey/reference/tm_g_spiderplot.md)`(`` `` label ``=`` ``"Spiderplot"``,`` `` dataname ``=`` ``"ADTR"``,`` `` paramcd ``=`` ``variables``(`` `` choices ``=`` ``is_categorical``(``)``,`` `` selected ``=`` ``"PARAMCD"`` `` ``)``,`` `` x_var ``=`` ``variables``(`` `` choices ``=`` ``dplyr``::`[`where`](https://tidyselect.r-lib.org/reference/where.html)`(``is.numeric``)``,`` `` selected ``=`` ``1L`` `` ``)``,`` `` y_var ``=`` ``variables``(`` `` choices ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"PCHG"``, ``"CHG"``, ``"AVAL"``)``,`` `` selected ``=`` ``"PCHG"`` `` ``)``,`` `` marker_var ``=`` ``variables``(`` `` choices ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"SEX"``, ``"RACE"``, ``"USUBJID"``)``,`` `` selected ``=`` ``"SEX"`` `` ``)``,`` `` line_colorby_var ``=`` ``variables``(`` `` choices ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"SEX"``, ``"USUBJID"``, ``"RACE"``)``,`` `` selected ``=`` ``"SEX"`` `` ``)``,`` `` xfacet_var ``=`` ``variables``(`` `` choices ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"SEX"``, ``"ARM"``)``,`` `` selected ``=`` ``"SEX"`` `` ``)``,`` `` yfacet_var ``=`` ``variables``(`` `` choices ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"SEX"``, ``"ARM"``)``,`` `` selected ``=`` ``"ARM"`` `` ``)``,`` `` decorators ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`` `` plot ``=`` ``ggplot_caption_decorator``(``"I am a ggplot"``)`` `` ``)`` `` ``)`` `` ``)`` ``)`` `` ``if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`` `` ``shinyApp``(``app``$``ui``, ``app``$``server``)`` ``}`

## Decorating `grob`

Here’s an example to showcase how you can edit an output of class
`grob`. You can extend them using `grid` and `gridExtra` functions.

[`library`](https://rdrr.io/r/base/library.html)`(`[`teal.osprey`](https://insightsengineering.github.io/teal.osprey/)`)`` `` ``data`` ``<-`` `[`within`](https://rdrr.io/r/base/with.html)`(``teal_data``(``)``, ``{`` `` ``ADSL`` ``<-`` ``rADSL`` `` ``ADTR`` ``<-`` ``rADTR`` ``}``)`` `` ``join_keys``(``data``)`` ``<-`` ``default_cdisc_join_keys``[`[`names`](https://rdrr.io/r/base/names.html)`(``data``)``]`` `` ``grob_caption_decorator`` ``<-`` ``function``(``default_caption`` ``=`` ``"I am a good decorator"``)`` ``{`` `` ``teal_transform_module``(`` `` label ``=`` ``"Caption"``,`` `` ui ``=`` ``function``(``id``)`` ``{`` `` ``shiny``::`[`textInput`](https://rdrr.io/pkg/shiny/man/textInput.html)`(``shiny``::`[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``, ``"footnote"``)``, ``"Footnote"``, value ``=`` ``default_caption``)`` `` ``}``,`` `` server ``=`` ``function``(``id``, ``data``)`` ``{`` `` ``moduleServer``(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`` `` ``reactive``(``{`` `` `[`data`](https://rdrr.io/r/utils/data.html)`(``)`` ``|>`` `` `[`within`](https://rdrr.io/r/base/with.html)`(`` `` ``{`` `` ``footnote_grob`` ``<-`` ``grid``::`[`textGrob`](https://rdrr.io/r/grid/grid.text.html)`(`` `` ``footnote``,`` `` x ``=`` ``0``, hjust ``=`` ``0``,`` `` gp ``=`` ``grid``::`[`gpar`](https://rdrr.io/r/grid/gpar.html)`(``fontsize ``=`` ``10``, fontface ``=`` ``"italic"``, col ``=`` ``"gray50"``)`` `` ``)`` `` ``plot`` ``<-`` ``gridExtra``::`[`arrangeGrob`](https://rdrr.io/pkg/gridExtra/man/arrangeGrob.html)`(`` `` ``plot``,`` `` ``footnote_grob``,`` `` ncol ``=`` ``1``,`` `` heights ``=`` ``grid``::`[`unit.c`](https://rdrr.io/r/grid/unit.c.html)`(`` `` ``grid``::`[`unit`](https://rdrr.io/r/grid/unit.html)`(``1``, ``"npc"``)`` ``-`` ``grid``::`[`unit`](https://rdrr.io/r/grid/unit.html)`(``1``, ``"lines"``)``, ``grid``::`[`unit`](https://rdrr.io/r/grid/unit.html)`(``1``, ``"lines"``)`` `` ``)`` `` ``)`` `` ``}``,`` `` footnote ``=`` ``input``$``footnote`` `` ``)`` `` ``}``)`` `` ``}``)`` `` ``}`` `` ``)`` ``}`` `` ``app`` ``<-`` ``init``(`` `` data ``=`` ``data``,`` `` modules ``=`` ``modules``(`` `` `[`tm_g_ae_sub`](https://insightsengineering.github.io/teal.osprey/reference/tm_g_ae_sub.md)`(`` `` label ``=`` ``"AE subview"``,`` `` dataname ``=`` ``"ADTR"``,`` `` arm_var ``=`` ``variables``(`` `` choices ``=`` ``is_categorical``(``)``,`` `` selected ``=`` ``"ACTARMCD"`` `` ``)``,`` `` group_var ``=`` ``variables``(`` `` choices ``=`` ``is_categorical``(``)``,`` `` selected ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"SEX"``, ``"REGION1"``, ``"RACE"``)`` `` ``)``,`` `` decorators ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`` `` plot ``=`` ``grob_caption_decorator``(``"I am a grob"``)`` `` ``)`` `` ``)`` `` ``)`` ``)`` `` ``if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`` `` ``shinyApp``(``app``$``ui``, ``app``$``server``)`` ``}`

## Decorating `gtable`

Here’s an example to showcase how you can edit an output of class
`gtable`. You can extend them using `grid` and `gridExtra` functions.

[`library`](https://rdrr.io/r/base/library.html)`(`[`teal.osprey`](https://insightsengineering.github.io/teal.osprey/)`)`` `[`library`](https://rdrr.io/r/base/library.html)`(`[`"dplyr"`](https://dplyr.tidyverse.org)`)`` ``data`` ``<-`` `[`within`](https://rdrr.io/r/base/with.html)`(``teal_data``(``)``, ``{`` `` `[`library`](https://rdrr.io/r/base/library.html)`(`[`"dplyr"`](https://dplyr.tidyverse.org)`)`` `` ``ADSL`` ``<-`` ``rADSL`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``TRTDURD ``=`` `[`as.integer`](https://rdrr.io/r/base/integer.html)`(``TRTEDTM`` ``-`` ``TRTSDTM``)`` ``+`` ``1``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` `[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``STRATA1`` ``==`` ``"A"`` ``&`` ``ARMCD`` ``==`` ``"ARM A"``)`` `` ``ADRS`` ``<-`` ``rADRS`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` `[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``PARAMCD`` ``==`` ``"LSTASDI"`` ``&`` ``DCSREAS`` ``==`` ``"Death"``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` `[`mutate`](https://dplyr.tidyverse.org/reference/mutate.html)`(``AVALC ``=`` ``DCSREAS``, ADY ``=`` ``EOSDY``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` `[`rbind`](https://rdrr.io/r/base/cbind.html)`(`` `` ``rADRS`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` `[`filter`](https://dplyr.tidyverse.org/reference/filter.html)`(``PARAMCD`` ``==`` ``"OVRINV"`` ``&`` ``AVALC`` ``!=`` ``"NE"``)`` `` ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `` `[`arrange`](https://dplyr.tidyverse.org/reference/arrange.html)`(``USUBJID``)`` ``}``)`` `` ``join_keys``(``data``)`` ``<-`` ``default_cdisc_join_keys``[`[`names`](https://rdrr.io/r/base/names.html)`(``data``)``]`` `` ``gtable_caption_decorator`` ``<-`` ``function``(``default_caption`` ``=`` ``"I am a good decorator"``)`` ``{`` `` ``teal_transform_module``(`` `` label ``=`` ``"Caption"``,`` `` ui ``=`` ``function``(``id``)`` ``{`` `` ``shiny``::`[`textInput`](https://rdrr.io/pkg/shiny/man/textInput.html)`(``shiny``::`[`NS`](https://rdrr.io/pkg/shiny/man/NS.html)`(``id``, ``"footnote"``)``, ``"Footnote"``, value ``=`` ``default_caption``)`` `` ``}``,`` `` server ``=`` ``function``(``id``, ``data``)`` ``{`` `` ``moduleServer``(``id``, ``function``(``input``, ``output``, ``session``)`` ``{`` `` ``reactive``(``{`` `` `[`data`](https://rdrr.io/r/utils/data.html)`(``)`` ``|>`` `` `[`within`](https://rdrr.io/r/base/with.html)`(`` `` ``{`` `` ``footnote_grob`` ``<-`` ``grid``::`[`textGrob`](https://rdrr.io/r/grid/grid.text.html)`(`` `` ``footnote``,`` `` x ``=`` ``0``, hjust ``=`` ``0``,`` `` gp ``=`` ``grid``::`[`gpar`](https://rdrr.io/r/grid/gpar.html)`(``fontsize ``=`` ``10``, fontface ``=`` ``"italic"``, col ``=`` ``"gray50"``)`` `` ``)`` `` ``plot`` ``<-`` ``gtable``::`[`gtable_add_grob`](https://gtable.r-lib.org/reference/gtable_add_grob.html)`(`` `` ``plot``,`` `` ``footnote_grob``,`` `` t ``=`` ``1``,`` `` l ``=`` ``1``,`` `` clip ``=`` ``"on"`` `` ``)`` `` ``}``,`` `` footnote ``=`` ``input``$``footnote`` `` ``)`` `` ``}``)`` `` ``}``)`` `` ``}`` `` ``)`` ``}`` `` ``app`` ``<-`` ``init``(`` `` data ``=`` ``data``,`` `` modules ``=`` ``modules``(`` `` `[`tm_g_swimlane`](https://insightsengineering.github.io/teal.osprey/reference/tm_g_swimlane.md)`(`` `` label ``=`` ``"Swimlane Plot"``,`` `` dataname ``=`` ``"ADRS"``,`` `` bar_var ``=`` ``variables``(`` `` choices ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"TRTDURD"``, ``"EOSDY"``)``,`` `` selected ``=`` ``"TRTDURD"`` `` ``)``,`` `` bar_color_var ``=`` ``variables``(`` `` choices ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"EOSSTT"``, ``"ARM"``, ``"ARMCD"``, ``"ACTARM"``, ``"ACTARMCD"``, ``"SEX"``)``,`` `` selected ``=`` ``"EOSSTT"`` `` ``)``,`` `` decorators ``=`` `[`list`](https://rdrr.io/r/base/list.html)`(`` `` plot ``=`` ``gtable_caption_decorator``(``"I am a gtable"``)`` `` ``)`` `` ``)`` `` ``)`` ``)`` `` ``if`` ``(`[`interactive`](https://rdrr.io/r/base/interactive.html)`(``)``)`` ``{`` `` ``shinyApp``(``app``$``ui``, ``app``$``server``)`` ``}`
