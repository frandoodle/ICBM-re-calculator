input <- tribble(~id, ~soil_depth_min, ~soil_depth_max, ~c,
								 1, 0, 15, 10,
								 1, 0, 7.5, 8,
								 1, 7.5, 15, 3,
								 1, 15, 30, 11,
								 2, 0, 15, 25,
								 2, 15, 30, 7,
								 3, 0, 20, 16)

input %>%
	mutate(soil_depth = paste0(soil_depth_min,"-",soil_depth_max)) %>%
	group_by(id) %>%
	summarise(lowest_depth = -sum(-soil_depth_max[soil_depth_max<=30], soil_depth_min[soil_depth_max<=30]),
						lowest_depth_c = sum(c[soil_depth_max<=30]))

output <- tribble(~id, ~lowest_depth, ~lowest_depth_c,
									1, 30, 21,
									1, 30, 22,
									2, 30, 32,
									3, 20, 16)

input <- tribble(~id, ~soil_depth_min, ~soil_depth_max, ~c,
								 1, 0, 15, 10,
								 1, 0, 7.5, 8,
								 1, 7.5, 15, 3,
								 1, 15, 30, 11,)
z <- input %>%
	mutate(soil_depth = paste0(soil_depth_min,"-",soil_depth_max))
	
combn(z$soil_depth, 3) %>%
	t %>%
	as_tibble() %>%
	rowwise() %>%
	mutate(min1 = as.numeric(str_split(V1,"-")[[1]][1]),
				 max1 = as.numeric(str_split(V1,"-")[[1]][2]),
				 min2 = as.numeric(str_split(V2,"-")[[1]][1]),
				 max2 = as.numeric(str_split(V2,"-")[[1]][2]),
				 min3 = as.numeric(str_split(V3,"-")[[1]][1]),
				 max3 = as.numeric(str_split(V3,"-")[[1]][2]),
				 min_unique = (any(duplicated(c(min1, min2, min3)))),
				 max_unique = (any(duplicated(c(max1, max2, max3)))),
				 k = (max1 + max2 + max3) - (min1 + min2 + min3)) %>%
	filter(!min_unique,
				 !max_unique)
	


	expand(k)
	summarise(lowest_depth = -sum(-soil_depth_max[soil_depth_max<=30], soil_depth_min[soil_depth_max<=30]),
						lowest_depth_c = sum(c[soil_depth_max<=30]))
