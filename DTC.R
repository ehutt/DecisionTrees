#decision tree classifier 

entropy <- function(subData, att, response){
  ent = 0
  a = subData[[att]]
  resp = subData[[response]]
  for (i in 1:nlevels(a)){
    #P(attribute = factor i)
    mask = subData[att]==levels(a)[i]
    p_a = sum(mask) / nrow(subData)
    branch = subData[mask,]
    for (j in 1:nlevels(resp)){
      #P(response j given attribute i)
      p_r = sum(as.character(branch[[response]])==levels(resp)[j]) / nrow(branch)
      if (is.null(p_r) | is.na(p_r)){p_r = 0}
      #entropy(attribute i) = -P(left)*[P(response)*ln(P(response))]
      if (p_r != 0) {ent = ent - (p_a * p_r * log(p_r))
      } 
    }
  }
  return(ent)
}

best_split <- function(subData, attributes, response){
  min_ent = 100
  for (att in attributes){
    ent = entropy(subData,att,response)
    if (ent < min_ent) {
      min_ent = ent
      best = att
    }
  }
  return(best)
}

#recursively builds tree 
build_tree <- function(data,variables,response,node_names,edge_mat){
  
  #root of tree and its branches
  best = best_split(data,variables,response)
  node_names = c(node_names, best)
  branches = levels(data[[best]])
  
  #expand tree along low entropy splits
  for (branch in branches){
    #only use data where value of best matches branch
    #remove the column "best" from subset of data
    mask = data[best]==branch
    subData = data[mask,-match(best,colnames(data))] 
    subAtt = variables[-match(best,variables)] 
    
    #check for empty data/variables 
    if (nrow(subData)==0) {
      #res = "NA"
      #node_names = c(node_names, res)
      #new_edge = matrix(c(best,res,branch),nrow=1)
      #edge_mat = rbind(edge_mat,new_edge)
      #message(c("Empty data frame: ",best,branch))
    } else if (length(subAtt)==0){
      res = "NA"
      node_names = c(node_names, res)
      new_edge = matrix(c(best,res,branch),nrow=1)
      edge_mat = rbind(edge_mat,new_edge)
      #message("Unable to make full prediction with given formula.")
    } else {
      #check for homogeneity of subset 
      classes = levels(subData[[response]])
      n = nlevels(subData[[response]])
      homogenous = FALSE 
      for (i in 1:n){
        if (all(subData[response]==classes[i])){
          homogenous=TRUE
        }
      }
      #check for terminal nodes
      if (homogenous){
        result = as.character(subData[[response]][1])
        node_names = c(node_names, result)
        new_edge = matrix(c(best,result,branch),nrow=1)
        edge_mat = rbind(edge_mat,new_edge)
      } else {
        next_best = best_split(subData,subAtt,response)
        new_edge = matrix(c(best,next_best,branch),nrow=1)
        edge_mat = rbind(edge_mat,new_edge)
        dtc = build_tree(subData,subAtt,response,node_names,edge_mat)
        node_names = dtc[[1]]
        edge_mat = dtc[[2]]
      }
    }
  }
  return(list(node_names,edge_mat))
}

DTC <- function(formula, data){
  library(igraph)
  library(DescTools)
  library(plyr)
  
  if (!is.formula(formula)) {stop("Invalid Formula.")
  } else if (!is.data.frame(data)){ stop("Data must be a data frame.")
  } else if(any(is.na.data.frame(data))){stop("Null values detected.")} 
  
  #parse formula, extract response and variables
  parsed = ParseFormula(formula,data=data)
  response = parsed$lhs[3][[1]]
  variables = parsed$rhs[[3]]
  
  #empty matrix to store edgelist and names
  edge_mat = matrix(ncol=3)
  colnames(edge_mat) = c("V1","V2","label")
  #empty list to store node names 
  node_names = c()
  #recursively build tree
  t = build_tree(data,variables,response, node_names,edge_mat)
  
  #extract tree info
  node_names = t[[1]]
  edge_mat = t[[2]]
  #extract edgelist, remove first row of NA
  el = edge_mat[-1,1:2]
  classes = levels(data[[response]])
  #remove repeated node names
  for (i in 1:nrow(el)){
    for (j in 1:length(classes)){
      if (el[i,2]==classes[j] | el[i,2]=="NA"){
        el[i,2]=i
      }
    }
  }
  #build graph from edgelist
  g = graph_from_edgelist(el,directed=TRUE)
  #name terminal nodes
  V(g)$name = node_names
  #plot 
  plot(g,layout=layout_as_tree,edge.label=edge_mat[-1,3])  
}




