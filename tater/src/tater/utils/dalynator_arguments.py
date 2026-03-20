from dalynator.argument_pool import (
    VALID_BURDENATOR_MEASURES,
    add_age_group_ids,
    add_data_root,
    add_loc_id,
    add_year_id,
    add_rei_id,
    add_sex_id,
    add_out_dir,
    add_tool_names,
    add_output_version,
    add_measure_ids,
    add_measure_id,
    add_verbose,
)


class DalynatorArguments:
    """
    This class reuses arguments selectively that are defined in dalynator.argument_pool
    """

    def add_by_reusing_arguments(parser):
        parser = add_age_group_ids(parser)
        parser = add_loc_id(parser)
        parser = add_year_id(parser)
        parser = add_out_dir(parser)
        parser = add_measure_ids(parser)
        parser = add_verbose(parser)

        valid_tool_names = ["dalynator", "burdenator"]
        parser = add_tool_names(parser, valid_tool_names, required=False)
        return parser
