#' @title annotate_post_model_workflow
#'
#' @description Take incoming cause data.table, and add columns rake_model and compile_model to inform
#' raking management scripts about which causes need to be raked and compiled.  See internal comments for
#' specific details.  Also used by check_raked_result.r, submit_all_aggregation.r.
#'
#' @param cause_tree [data.table] read from submitted_cause_list.csv or settings.yaml
#'
#' @return cause_tree data.table with logical columns rake_model, and compile_model added
#'
#' @rdname annotate_post_model_workflow
#' @export
annotate_post_model_workflow <- function(cause_tree) {
  cause_tree <- copy(cause_tree)

  # raking
  cause_tree[, rake_model := TRUE]

  # rake_model will tell us which causes need to be raked as part of
  # rake_by_geography.r or rake_by_geography_and_cause.r.
  # 1. do not rake models where all children are set to run_model == 0
  rule.1 <- cause_tree[, list(rake_model = any(run_model == 1)), by = parent_id]
  # set cause_tree$rake_model to value of rule.1$rake_model aligned by cause_tree$cause_id == rule.1$parent_id
  cause_tree[rule.1, on = .(cause_id = parent_id), rake_model := i.rake_model]
  # 2. do not rake models with no children
  cause_tree[!cause_id %in% parent_id, rake_model := FALSE]
  # 3. do not rake models whose parent is set to run_model == 0
  maternal_disorders_modeled <- cause_tree[cause_id %in% c(366, 380) & run_model == 1, unique(cause_id)]
  if (length(maternal_disorders_modeled) == 0) {
    no.model <- cause_tree[run_model == 0, cause_id]
  } else {
    no.model <- cause_tree[run_model == 0 & cause_id != 962, cause_id] 
  }
  cause_tree[parent_id %in% no.model, rake_model := FALSE]

  # Compiling results
  cause_tree[, compile_model := TRUE]

  # Compile_model is used to determine what causes need to be concatenated at the end of
  # raking, in compile_raked.r
  cause_tree[rule.1, on = "parent_id", compile_model := i.rake_model]
  # 2. Cause has no parent cause in the tree and did not go through raking
  # '_all' (parent_id = 0) has no parent in the tree, but also does not need to be compiled
  cause_ids <- cause_tree[, cause_id]
  cause_tree[!parent_id %in% cause_ids, compile_model := FALSE]

  # 3. If the parent cause has run_model=0, all the children causes should skip aggregation i.e. compile_model=FALSE
  cause_tree[parent_id %in% no.model, compile_model := FALSE]

  # Aggregate_model is used to determine what causes need to be aggregated (launch_aggregation.r). 
  # The rules are: 
  # 1. do not aggregate models if all "sibling" (causes with same parent) are set to run_model == 0
  cause_tree[rule.1, on = "parent_id", aggregate_model := i.rake_model]

  # 2. If a parent cause is not modeled, none of its children should be aggregated. 
  rule.2 <- cause_tree[run_model == FALSE, cause_id] 

  for (i in 1:nrow(cause_tree)){
    parent_list_exclusive = unlist(strsplit(cause_tree[i, path_to_top_parent], ","))
    parent_list_exclusive = parent_list_exclusive[parent_list_exclusive!=cause_tree[i, cause_id]]
    cause_tree[i, path_to_top_parent_exclusive:=paste0(parent_list_exclusive, collapse = ",")]
  }
  for (cause in rule.2){
    cause_tree[grepl(cause, path_to_top_parent_exclusive), aggregate_model:= FALSE]
  }

  return(cause_tree)
}
