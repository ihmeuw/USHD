import datetime

import gbd.constants


class USHDArguments:
    """Set of functions to add USHD specific arguments to the CLI."""
    def workflow_name(parser):
        parser.add_argument(
            "--workflow_name",
            default=datetime.datetime.now().strftime("run_all_taternator_%Y-%m-%d"),
            help="The name of the jobmon workflow. Use this for resuming",
        )
        return parser

    def workflow_args(parser):
        parser.add_argument(
            "--workflow_args",
            default="",
            help="UID for jobmon workflow. Pass this only if you need to resume a prior workflow",
        )
        return parser

    def max_concurrent_jobs(parser):
        parser.add_argument(
            "--max_concurrent",
            type=int,
            # 400 errored a lot
            # 100 errored
            default=150,
            help="Maximum number of concurrent jobs to run",
        )
        return parser

    def add_cause_set_ids(parser):
        parser.add_argument('--cause_set_ids', nargs='+',
                            default=[21],
                            type=int, action='store',
                            help='The cause_set_ids to use for the cause_hierarchy, an integer list')
        return parser

    def add_fatal_version_id(parser):
        parser.add_argument(
            "--fatal_version_id",
            type=int,
            action="store",
            help="The fatal model_run_id, an integer.",
        )
        return parser
    
    def add_rake_version_id(parser):
        parser.add_argument(
            "--rake_version_id",
            type=int,
            action="store",
            help="The fatal rake_version_id, an integer.",
        )
        return parser

    def add_nonfatal_run_id(parser):
        parser.add_argument(
            "--nonfatal_run_id",
            type=int,
            action="store",
            help="The non-fatal model_run_id, an integer.",
        )
        return parser

    def add_paf_compile_run_id(parser):
        parser.add_argument(
            "--paf_compile_run_id",
            type=int,
            required=True,
            action="store",
            help="The paf_compile_run_id, an integer.",
        )
        return parser

    def add_loc_set_id(parser):
        parser.add_argument("--location_set_id", type=int,
                            required=True,
                            help='The location_set_id. An int. Probably 128')
        return parser

    def add_loc_set_ids(parser):
        parser.add_argument('--location_set_ids', type=int, nargs='+',
                            default=[128],
                            action='store',
                            help='The location_set_ids, an int list')
        return parser

    def add_extra_dim(parser):
        parser.add_argument(
            "--extra_dim",
            type=str,
            required=True,
            choices=["edu", "race"],
            help="Specify whether to load ushd data by race or by edu",
        )
        return parser

    def add_age_group_set_id(parser):
        parser.add_argument(
            "--age_group_set_id",
            type=int,
            required=True,
            choices=[29, 30],
            help="USHD specific age_group_set_id from which to query "
            "age trees. 29 for Race/Ethnicity and 30 for Education",
        )
        return parser

    def add_gbd_release_id(parser):
        parser.add_argument(
            "-g",
            "--gbd_release_id",
            type=int,
            required=True,
            help="The gbd_release_id as a database ID, " "eg 15 (==USHD), defaults to 15.",
        )
        return parser

    def add_out_dir(parser):
        parser.add_argument("--out_dir", "-o",
                            default='FILEPATH',
                            help=("The root directory for the output files, --output_version will be appended. "))
        return parser

    def add_cache_dir(parser):
        parser.add_argument('--cache_dir',
                            default='cache',
                            type=str,
                            help='The directory to which USHDCache will be saved. If directory does not exist, '
                                 'it will be created.')
        return parser

    def add_year_ids(parser):
        parser.add_argument("--year_ids", type=int, nargs='+', required=True, help="The years to process.")
        return parser

    def add_n_draws(parser):
        parser.add_argument('--n_draws', type=int, required=True,
                            help='The number of draw columns. Inputs with a larger number of draws will be downsampled')
        return parser
