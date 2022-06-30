compute_group_scores <- function(df, group, party_vector, variable, aggregation_method = 'mean') {
  
  # Takes as input a dataframe, 
  # a string indicating which group we're computing this for ('in_' for ingroup, 'out_' for outgroup),
  # a vector of party labels which should correspond to those used in column names,
  # and a string name indicating which type of attitude we're computing this for
  # ('like_' for like-dislike ratings, 'socdis_' for social distance, and so forth).
  # Finally, aggregation_method defines whether the function will find the mean, minimum, or maximum
  # for this group.
  
  group_scores <- 1:nrow(df) # A vector to hold scores, as long as there are respondents
  
  for (i in 1:nrow(df)) {
    
    # Pick row
    
    row <- df[i,]
    
    # The dataframe holds a boolean for each party indicating whether or not that party is an inparty/outparty.
    # We'll pick those that are TRUE, i.e. are an inparty(outparty)
    
    partybools <- row[paste(group, party_vector, sep = '')]
    groupparties <- colnames(dplyr::select(partybools, where(isTRUE)))
    
    # If the resulting vector has length 0, i.e. there are no inparties/outparties, return NA and continue the loop
    
    if (length(groupparties) == 0) {
      group_scores[i] <- NA
      next
    }
    
    # Otherwise, form a selection vector by taking the inparty/outparty boolean columns
    # and transforming them into socdis/like-dislike column names
    
    selection_vector <- str_replace(groupparties, group, variable)
    
    # Select attitude items
    
    scores <- row[selection_vector]
    
    #if (any(is.na(scores))) {
    
    #group_scores[i] <- NA
    #  print('fug')
    #next
    #}
    
    if (aggregation_method == 'mean') {
      group_scores[i] <- mean(unlist(scores), na.rm = FALSE)
    } else if (aggregation_method == 'max') {
      group_scores[i] <- max(unlist(scores), na.rm = FALSE)
    } else if (aggregation_method == 'min') {
      group_scores[i] <- min(unlist(scores), na.rm = FALSE)
    }
    
    
  }
  
  return(group_scores)
}

compute_mpid_siap <- function(df, party_vector, variable, threshold, aggregation_method = 'mean') {
  
  scores <- 1:nrow(df)
  
  for (i in 1:nrow(df)) {
    
    # Get vector of affiliation scores
    
    affiliations <- df[i, paste('affiliation_', party_vector, sep = '')]
    
    # Get the highest maximum affiliation
    
    max_affiliation <- max(unlist(affiliations))
    
    # First check if a threshold has been supplied. If not, the function simply looks for the highest PID score.
    # In case a threshold has been supplied, the function first checks whether any parties pass it, and if not,
    # assigns NA to the respondent and continues the loop.
    
    if (!missing(threshold)) { 
      if (max_affiliation <= threshold) {
        scores[i] <- NA
        next
      }      
    }
    
    # Find the party/parties with the maximum affiliation score
    
    affiliated_parties <- affiliations[which(affiliations == max_affiliation)]
    
    # Get attitude ratings for all parties
    
    attitude_ratings <- df[i, paste(variable, party_vector, sep = '')]
    
    if (length(affiliated_parties) == 1) {
      
      # If we have found a unique inparty, everything is simple and we'll use the like-dislike rating for that party
      # First get the vector of like-dislike column names
      
      in_columns = str_replace(colnames(affiliated_parties), 'affiliation_', variable)
      
      # For outparty ratings, pick columns which DO NOT match inparty column names
      
      in_rating <- attitude_ratings[in_columns]
      out_ratings <- attitude_ratings[!colnames(attitude_ratings) %in% in_columns]
      
      # Take outparty mean and subtract it from the inparty rating. 
      # We'll take the mean for the inparty rating as well to get rid of the column name
      
      if (aggregation_method == 'mean') {
        out_score <- mean(unlist(out_ratings))  
      } else if (aggregation_method == 'min') {
        out_score <- min(unlist(out_ratings))
      } else if (aggregation_method == 'max') {
        out_score <- max(unlist(out_ratings))
      } else if (aggregation_method == 'n_inparties') {
        scores[i] <- 1
        next
      }
      
      distance <- mean(unlist(in_rating)) - out_score
      
      scores[i] <- distance
      
    }
    
    if (length(affiliated_parties) > 1) {
      
      # If there are multiple parties with the same highest affiliation score, 
      # we'll compare like-dislike ratings for those parties.
      
      # First
      
      in_columns = str_replace(colnames(affiliated_parties), 'affiliation_', variable)
      
      in_attitudes = attitude_ratings[in_columns]
      
      max_attitude <- max(in_attitudes)
      
      #d <- m[i, str_replace(colnames(c), 'affiliation', 'like')]
      
      #e <- max(d)
      
      #f <- d[which(d == e)]
      
      highest_attituded = in_attitudes[which(in_attitudes == max_attitude)]
      
      if (length(highest_attituded[!is.na(highest_attituded)]) == 0) {
        
        # Means we don't have like-dislike data
        
        if (aggregation_method == 'n_inparties') {
          scores[i] <- length(affiliated_parties)
          next  
        } else {
          scores[i] <- NA
          next
        }
        
      }
      
      if (aggregation_method == 'n_inparties') {
        scores[i] <- length(highest_attituded)
        next
      }
      
      if (length(highest_attituded) > 1) {
        
        highest_attituded <- sample(highest_attituded, 1)
      }
      #highest_liked <- like_ratings[which(like_ratings == max_like)]
      
      out_ratings <- attitude_ratings[!colnames(attitude_ratings) %in% colnames(highest_attituded)]
      
      if (aggregation_method == 'mean') {
        out_score <- mean(unlist(out_ratings))  
      } else if (aggregation_method == 'min') {
        out_score <- min(unlist(out_ratings))
      } else if (aggregation_method == 'max') {
        out_score <- max(unlist(out_ratings))
      }
      
      distance <- mean(unlist(highest_attituded) - out_score)
      scores[i] <- distance
      
    }
    
  }
  return(scores)
}

plot_model_marginals <- function(model, draw_x, min_y = 0, max_y = 10, dependent_var, coord_ratio = 1) {
  
  marginal_preds <- ggpredict(model, condition = c(leftright_c = 0, libcon_c = 0))  
  
  # For the first plot, always draw Y axis title and labels
  
  p1 <- marginal_preds$leftright_c %>%
    ggplot(aes(x = x, y = predicted)) +
    geom_line() +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
    scale_y_continuous(limits = c(min_y, max_y)) +
    xlab('Left-right') +
    #ylab('Like bias')
    ylab(dependent_var) +
    coord_fixed(ratio = coord_ratio)
  
  p2 <- marginal_preds$libcon_c %>%
    ggplot(aes(x = x, y = predicted)) +
    geom_line() +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
    scale_y_continuous(limits = c(min_y, max_y)) +
    xlab('Liberal-conservative') +
    #ylab('Like bias')
    ylab(dependent_var) +
    #theme(axis.text.y = element_blank())
    theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
    coord_fixed(ratio = coord_ratio)
  
  p3 <- marginal_preds$voted_party %>%
    ggplot(aes(x = x, y = predicted)) +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
    scale_y_continuous(limits = c(min_y, max_y)) +
    scale_x_discrete(guide = guide_axis(angle = 45)) +
    xlab('Party choice') +
    #ylab('Like bias')
    ylab(dependent_var) +
    theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
    coord_fixed(ratio = coord_ratio)
  
  if (draw_x == 'no') {
    p1 <- p1 + theme(axis.text.x = element_blank(), axis.title.x = element_blank())
    p2 <- p2 + theme(axis.text.x = element_blank(), axis.title.x = element_blank())
    p3 <- p3 + theme(axis.text.x = element_blank(), axis.title.x = element_blank())
  }
  
  return(list(p1, p2, p3))
  #return(list(p1, p2))
  
}
