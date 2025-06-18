cell_line_output = list(
    type = "json_schema",
    json_schema = list(
        name = "cell_line",
        description = "Upon determining cell lines used in the experiment, present your findings in this structure. Use an empty list if you are unable to find any strains. Include all possible matches",
        strict = TRUE,
        schema = list(
            type = "object",
            additionalProperties = FALSE,
            required = list('cell_lines'),
            properties = list(
                cell_lines = list(
                    type = 'array',
                    items = list(
                        type = 'object',
                        additionalProperties = FALSE,
                        required = c('cell_line_name','description','quote'),
                        properties = list(
                            cell_line_name = list(
                                type = 'string',
                                description = "Name of the cell line"
                            ),
                            description = list(
                                type = 'string',
                                description = "A description of the cell line if it's provided"
                            ),
                            quote = list(
                                type = 'array',
                                items = list(
                                    type = 'string',
                                    description = 'Verbatim quotes from the source to justify your decision'
                                )

                            )
                        )
                    )
                )
            )

        )
    )
)

cell_line_annotation = list(
    type = "json_schema",
    json_schema = list(
        name = "cell_line",
        description = "From the list of annotation terms provided, pick the one that best matches the cell line used in the experiment. If no term is a suitable match, return an empty array",
        strict = TRUE,
        schema = list(
            type = "object",
            additionalProperties = FALSE,
            required = list('cell_lines'),
            properties = list(
                cell_lines = list(
                    type = 'array',
                    items = list(
                        type = 'object',
                        additionalProperties = FALSE,
                        required = c('cell_line_name','cell_line_ID'),
                        properties = list(
                            cell_line_name = list(
                                type = 'string',
                                description = "Name of the cell line."
                            ),
                            cell_line_ID = list(
                                type = 'string',
                                description = "Ontology ID of the cell line"
                            )
                        )
                    )
                )
            )

        )
    )
)


strain_output = list(
    type = "json_schema",
    json_schema = list(
        name = 'mouse_strain',
        description = "Upon determining mouse strain used in the experiment, present your findings with this structure. Use an empty list if you are unable to find any strains, include all possible matches",
        strict = TRUE,
        schema = list(
            type = 'object',
            additionalProperties = FALSE,
            required = list('strains'),
            properties = list(
                strains = list(
                    type = 'array',
                    items = list(
                        type = 'object',
                        additionalProperties = FALSE,
                        required = c('value','URI','quote'),
                        properties = list(
                            value = list(
                                type = 'string',
                                description ="Name of the mouse strain, must match value in list of strains provided"
                            ),
                            URI = list(
                                type = 'string',
                                description =  "URI of the mouse strain, must match the URI in the list of strains provided"
                            ),
                            
                            quote = list(
                                type = 'array',
                                description = "Verbatim quotes from the source to justify your decision",
                                items = list(
                                    type = 'string',
                                    description = 'A quote taken from your input'
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)
