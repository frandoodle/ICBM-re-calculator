make_depth_groups <- function(input) {
	
	z <- input %>%
		mutate(soil_depth = paste0(soil_depth_min_cm,"-",soil_depth_max_cm))
	
	one_combination_depths <- NULL
	two_combination_depths <- NULL
	three_combination_depths <- NULL
	
	one_combination_k <- NULL
	two_combination_k <- NULL
	three_combination_k <- NULL
	
	if(nrow(z) >= 1) {
		one_combination <- combn(z$soil_depth, 1) %>%
			t %>%
			as_tibble() %>%
			rowwise() %>%
			mutate(min1 = as.numeric(str_split(V1,"-")[[1]][1]),
						 max1 = as.numeric(str_split(V1,"-")[[1]][2])) %>%
			mutate(min_unique = (any(duplicated(c(min1)))),
						 max_unique = (any(duplicated(c(max1)))),
						 k = (max1) - (min1)) %>%
			filter(!min_unique,
						 !max_unique) %>%
			# Select the depth closest to 30
			ungroup() %>%
			mutate(k_diff = abs(30-k)) %>%
			filter(k_diff == min(k_diff))
		
		one_combination_k <- one_combination$k %>%
			unique()
		
		one_combination_depths <- one_combination %>%
			select(V1) %>%
			unlist
	}
	
	if(nrow(z) >= 2) {
		two_combination <- combn(z$soil_depth, 2) %>%
			t %>%
			as_tibble() %>%
			rowwise() %>%
			mutate(min1 = as.numeric(str_split(V1,"-")[[1]][1]),
						 max1 = as.numeric(str_split(V1,"-")[[1]][2]),
						 min2 = as.numeric(str_split(V2,"-")[[1]][1]),
						 max2 = as.numeric(str_split(V2,"-")[[1]][2])) %>%
			mutate(min_unique = (any(duplicated(c(min1, min2)))),
						 max_unique = (any(duplicated(c(max1, max2)))),
						 k = (max1 + max2) - (min1 + min2)) %>%
			filter(!min_unique,
						 !max_unique) %>%
			# Select the depth closest to 30
			ungroup() %>%
			mutate(k_diff = abs(30-k)) %>%
			filter(k_diff == min(k_diff))
		
		two_combination_k <- two_combination$k %>%
			unique()
		
		two_combination_depths <- two_combination %>%
			select(V1, V2) %>%
			unlist
	}
	
	if(nrow(z) >= 3) {
		three_combination <- combn(z$soil_depth, 3) %>%
			t %>%
			as_tibble() %>%
			rowwise() %>%
			mutate(min1 = as.numeric(str_split(V1,"-")[[1]][1]),
						 max1 = as.numeric(str_split(V1,"-")[[1]][2]),
						 min2 = as.numeric(str_split(V2,"-")[[1]][1]),
						 max2 = as.numeric(str_split(V2,"-")[[1]][2]),
						 min3 = as.numeric(str_split(V3,"-")[[1]][1]),
						 max3 = as.numeric(str_split(V3,"-")[[1]][2])) %>%
			mutate(min_unique = (any(duplicated(c(min1, min2, min3)))),
						 max_unique = (any(duplicated(c(max1, max2, max3)))),
						 k = (max1 + max2 + max3) - (min1 + min2 + min3)) %>%
			filter(!min_unique,
						 !max_unique) %>%
			# Select the depth closest to 30
			ungroup() %>%
			mutate(k_diff = abs(30-k)) %>%
			filter(k_diff == min(k_diff))
		
		three_combination_k <- three_combination$k %>%
			unique()
		
		three_combination_depths <- three_combination %>%
			select(V1, V2, V3) %>%
			unlist
	}
	
	one_combination_rows <- z %>%
		filter(soil_depth %in% one_combination_depths) %>%
		mutate(combination_group = 1,
					 k = one_combination_k)
	two_combination_rows <- z %>%
		filter(soil_depth %in% two_combination_depths) %>%
		mutate(combination_group = 2,
					 k = two_combination_k)
	three_combination_rows <- z %>%
		filter(soil_depth %in% three_combination_depths) %>%
		mutate(combination_group = 3,
					 k = three_combination_k)
	
	result <- bind_rows(one_combination_rows, two_combination_rows, three_combination_rows) %>%
		filter(k == max(k))
	
	print(result$location_name)
	print(result$treatment_name)
	
	return(result)
}