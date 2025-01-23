#' @title annotate_post_model_workflow
#'
#' @description Take incoming cause data.table, and add columns rake_model and compile_model to inform
#' raking management scripts about which causes need to be raked and compiled.
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

  # 1. do not rake models where all children are set to run_model == 0
  rule.1 <- cause_tree[, list(rake_model = any(run_model == 1)), by = parent_id]
  cause_tree[rule.1, on = .(cause_id = parent_id), rake_model := i.rake_model]
  
  # 2. do not rake models with no children
  cause_tree[!cause_id %in% parent_id, rake_model := FALSE]
  
  # 3. do not rake models whose parent is set to run_model == 0
  # The only exception to this rule is mater_neonat,
  # which should be raked as long as its two sub-causes are modeled.
  maternal_disorders_modeled <- cause_tree[cause_id %in% c(366, 380) & run_model == 1, unique(cause_id)]
  if (length(maternal_disorders_modeled) == 0) {
    no.model <- cause_tree[run_model == 0, cause_id]
  } else {
    no.model <- cause_tree[run_model == 0 & cause_id != 962, cause_id] # Will give mater_neonat a rake_model value of TRUE
  }
  cause_tree[parent_id %in% no.model, rake_model := FALSE]

  # Compiling results
  cause_tree[, compile_model := TRUE]

  # Compile_model is used to determine what causes need to be concatenated at the end of
  # raking, in compile_raked.r
  # If ANY sibling is run_model==1, you need to compile ALL siblings.
  # 1. do not compile models if all "sibling" (causes with same parent) are set to run_model == 0
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
  # path_to_top_parent includes the cause itself. So we want to take that out, and just consider its parents-grandparents.
  # Otherwise, the logic dictates that if a cause is not raked it will not be aggregated. 
  for (i in 1:nrow(cause_tree)){ 
    parent_list_exclusive = unlist(strsplit(cause_tree[i, path_to_top_parent], ","))
    parent_list_exclusive = parent_list_exclusive[parent_list_exclusive!=cause_tree[i, cause_id]]
    cause_tree[i, path_to_top_parent_exclusive:=paste0(parent_list_exclusive, collapse = ",")]
  }
  for (cause in rule.2){
    cause_tree[grepl(cause, path_to_top_parent_exclusive), aggregate_model:= FALSE] # Turn off aggregation for any sub-children of this cause
  }

  return(cause_tree)
}