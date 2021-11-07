snippet lib
	library(${1:tidyverse})

snippet req
	require(${1:package})

snippet src
	source("${1:file.R}")

snippet ret
	return(${1:code})

snippet mat
	matrix(${1:data}, nrow = ${2:rows}, ncol = ${3:cols})

snippet sg
	setGeneric("${1:generic}", function(${2:x, ...}) {
		standardGeneric("${1:generic}")
	})

snippet sm
	setMethod("${1:generic}", ${2:class}, function(${2:x, ...}) {
		${0}
	})

snippet sc
	setClass("${1:Class}", slots = c(${2:name = "type"}))

snippet if
	if (${1:condition}) {
		${0}
	}

snippet el
	else {
		${0}
	}

snippet ei
	else if (${1:condition}) {
		${0}
	}

snippet fun
	${1:name} <- function(${2:variables}) {
		${0}
	}

snippet for
	for (${1:variable} in ${2:vector}) {
		${0}
	}

snippet while
	while (${1:condition}) {
		${0}
	}

snippet switch
	switch (${1:object},
		${2:case} = ${3:action}
	)

snippet apply
	apply(${1:array}, ${2:margin}, ${3:...})

snippet lapply
	lapply(${1:list}, ${2:function})

snippet sapply
	sapply(${1:list}, ${2:function})

snippet mapply
	mapply(${1:function}, ${2:...})

snippet tapply
	tapply(${1:vector}, ${2:index}, ${3:function})

snippet vapply
	vapply(${1:list}, ${2:function}, FUN.VALUE = ${3:type}, ${4:...})

snippet rapply
	rapply(${1:list}, ${2:function})

snippet ts
	`r paste("#", date(), "------------------------------\n")`

snippet shinyapp
	library(shiny)

	ui <- fluidPage(
	  ${0}
	)

	server <- function(input, output, session) {

	}

	shinyApp(ui, server)

snippet shinymod
	${1:name}_UI <- function(id) {
	  ns <- NS(id)
	  tagList(
		${0}
	  )
	}

	${1:name} <- function(input, output, session) {

	}

snippet gg
	${1:diamonds} %>%
		ggplot(mapping = aes(x = ${2:x})) +
		geom_${3:point}()

snippet geom
	ggplot() +
	geom_${1:point}(mapping = aes(x = ${2:x}))

snippet pivot_longer
	pivot_longer(
		cols = ${1:cols_to_pivot},
		names_to = "${2:new_col_to_be_created_from_old_col_names}",
		values_to = "${3:new_col_to_be_created_from_tabular_values}"
	)

snippet pivot_wider
	pivot_wider(
		names_from = ${1:current_col_holding_variable_names},
		values_from = ${2:current_col_holding_values}
	)

snippet pivot_columns
	pivot_longer(
		cols = ${1:cols_to_pivot},
		names_to = c(".value", "${2:new_col_name}"),
		names_sep = "${3:_}",
		names_transform = list(${2:name_var_to_transform} = ${4:as.double}),
		values_transform = list(${5:value_var_to_transfrom} = ${6:as.double})
		${7:
			## When two variables are contained in the same column name,
			## this syntax splits two halves of the name apart,
			## creating a column name with the first half,
			## and another column named "new_col_name";
			## "new_col_name" contains the latter half
			## of the original column name
			## as its value for every row.}
	)

snippet labs
	labs(
		x = "${1:x_label}",
		y = "${2:y_label}",
		title = "${3:title}"
	)

snippet brewer
	scale_${2:color}${3:fill}_brewer(palette = "${1:Set1}")

snippet color
	brewer

snippet scale
	brewer

snippet scales
	brewer

snippet separate
	separate(
		col = ${1:col_with_vars_to_separate},
		into = c("${2:var_name_1}", "${3:var_name_2"}),
		sep = "${4:_}",
		remove = ${5:FALSE}
	)

snippet unite
	unite(
		col = "${1:col_name_to_create}",
		${2:var1}, ${3:var2},
		sep = "${4:_}",
		remove = ${5:FALSE}
	)

snippet join
	${4:left}_join(${1:df_2}, by = c("${2:column_from_df_1}" = "${3:column_from_df_2}"))

snippet facet
	facet_grid(${1:vertical_var} ~ ${2:horizontal_var}, scales = "free_${3:x}")

snippet tribble
	tribble(
		~${1:colA}, ~${2:colB},
		"a",   1,
		"b",   2,
		"c",   3
	)

snippet ggmap
	us <- map_data("state")
	# us %>%
	#   left_join(df_2, by = c("region" = "state"))
	ggplot(us) +
		geom_map(
			data = us,
			mapping = aes(
				map_id = region
			),
			map = us,
			fill = "transparent",
			color = "black"
			) +
			expand_limits(x = us\$long, y = us\$lat)

snippet ggmpa
	ggmap

snippet ggsave
	ggsave("${1:filename}.svg", width = 13, height = 11)

snippet lm
mylm <- mandate_cases_state %>%
	lm(cases_per100k ~ days_with_mandate, .)
	print(mylm)
	inter <- mylm$coefficients[[1]]
	slope <- mylm$coefficients[[2]]

snippet fct_relevel
	mutate(
		${1:variable_to_reorder} = fct_relevel(
			${1:variable_to_reorder},
			c(${2:list_of_factor_orders}) %>%
				rev()
		)
	) %>%

snippet fct_reorder
	mutate(${1:variable_to_sort} = fct_reorder(${1:variable_to_sort}, ${2:var_to_sort_by})) %>%

snippet fct_reorder2
	mutate(${1:variable_to_sort} = fct_reorder2(${1:variable_to_sort}, ${2:year}, ${3:val_to_sort_by_2})) %>%

snippet bootstraps
	bootstrap

snippet bootstrap
	# Calculate summaries of each sub-sample
	${1:df_sample}_bootstraps <-
	${1:df_sample} %>%
		bootstraps(times = 1000) %>%
		mutate(
		  ${1:df_sample} = map(splits, analysis),
		  ${1:df_sample}_estimates = map(
		    splits,
		    function(${1:df_sample}) {
		      analysis(${1:df_sample}) %>%
		      pull(${2:var}) %>%
		      fitdistr(densfun = "${3:normal}") %>%
		      tidy()
		    }
		  )
		)
	#
	# Display confidence intervals
	int_pctl(${1:df_sample}_bootstraps, ${1:df_sample}_estimates, alpha = ${4:0.05})

snippet facet_wrap
	facet_wrap(~${1:var_to_facet})

snippet ghost
	facet_wrap(~${1:var_to_facet}) +
	geom_${2:point}(
		data = . %>%
			select(-${1:var_to_facet}),
		color = "grey80",
		alpha = 0.2
	) +
	geom_${2:point}(color = "black")

snippet str
	str_detect(${1:str}, ${2:regex})

snippet theme
	theme(
		axis.text.x = element_text(angle = 90, vjust = 0.25, hjust=1),
		axis.title.x = element_blank(),
		axis.title.y = element_blank(),
		legend.title = element_blank(),
	)

snippet distr
	fitdistr

snippet fit_distr
	fitdistr

snippet fitdistr
	library(MASS)

	# Create distribution
	${1:df_sample}_distr <-
		${1:df_sample} %>%
		pull(${2:var}) %>%
		fitdistr("${3:normal}") %>%
		tidy()
	#
	# Estimate confidence interval
	${1:df_sample}_ci <-
		tibble(
	low = q${4:norm}(p = ${5:0.05} / 2, ${1:df_sample}_distr[[1, 2]], ${1:df_sample}_distr[[2, 2]]),
	median = q${4:norm}(p = 0.5, ${1:df_sample}_distr[[1, 2]], ${1:df_sample}_distr[[2, 2]]),
	high = q${4:norm}(p = 1 - ${5:0.05} / 2, ${1:df_sample}_distr[[1, 2]], ${1:df_sample}_distr[[2, 2]])
		)
	#
	# Display results
	${1:df_sample}_distr
	${1:df_sample}_ci