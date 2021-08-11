library(dplyr)
library(ggplot2)
library(ggforce)
library(rlang)

#### Functions ####

getPointsOnACircle = function(numPoints, numInFirstField = 0) {
  # Get points on a unit circle
  # let the first point be at pi/2
  start = pi/2
  indices = 0:(numPoints-1)
  incr = 2 * pi / numPoints # increment to move for each point
  start = start - (incr * floor(numInFirstField/2))
  theta = start + (incr * indices) # angle of each point
  data.frame(x = cos(theta), y = sin(theta))
}

radToDeg = function(x) {
  x * 180/pi
}

createBezierPoints = function(x, mapping){
  i = x[["i"]]
  j = x[["j"]]
  color = x[["color"]]
  n = x[["n"]]
  startingPoint = data.frame(val = x[["val1"]]) %>%
    left_join(mapping %>% filter(fieldIndex == i) %>% select(-fieldIndex), by = "val")
  endingPoint = data.frame(val = x[["val2"]]) %>% 
    left_join(mapping %>% filter(fieldIndex == j) %>% select(-fieldIndex), by = "val")
  controlPoint = data.frame(val = "control", x = 0, y = 0)
  rbind(startingPoint, controlPoint, endingPoint) %>%
    mutate(i = i, j = j, color = color, n=n, type = "cubic")
}

nonRibbonChordDiagram = function(df, ..., zoomScale = 1.5, pointRatio = 2, orderedFields = T) {
  fields = as.list(substitute(list(...)))[-1L]
  
  df = df %>% select(!!!fields)
  
  df = data.frame(lapply(df, as.character))
  
  numPoints = sapply(fields, function(field) length(unique(df[[field]])))
  
  pos = getPointsOnACircle(sum(numPoints), numPoints[1])
  
  mapping = do.call(rbind, lapply(1:length(fields), function(i) {
    field = fields[[i]]
    data.frame(fieldIndex = i, val = sort(unique(df[[field]])))
  }))
  
  if (!orderedFields) {
    mapping = mapping %>% slice_sample(prop = 1)
  }
  
  mapping = mapping %>% mutate(x = pos$x, y = pos$y)
  
  pointDf = do.call(rbind, lapply(1:length(fields), function(i) {
    field = fields[[i]]
    df %>% group_by(!!field) %>%
      count() %>% data.frame %>%
      setNames(c("val", "n")) %>% # allows for a common key and this route ensures user dataframe colnames don't impact what this value is set to
      left_join(mapping %>% filter(fieldIndex == i), by = "val")
  })) %>%
    mutate(angle = radToDeg(atan2(y, x)),
           angle = ifelse(x < 0, angle + 180, angle),
           nchar = nchar(val),
           hOffset = 0.53 + 1.2^-nchar, # works well for up to 60 characters
           hjust = ifelse(x < 0, 0.5+hOffset, 0.5-hOffset),
           vjust = 0.3)
  
  chordDf = do.call(rbind, lapply(1:(length(fields)-1), function(i) {
    field1 = fields[[i]]
    do.call(rbind, lapply((i+1):length(fields), function(j) {
      field2 = fields[[j]]
      # create three points for each line to make a cubic line using geom_bezier
      groupedFieldCount = df %>% 
        group_by(!!field1, !!field2) %>%
        count() %>% data.frame %>%
        setNames(c("val1", "val2", "n")) %>%
        mutate(i = i, j = j,
               color = val1)
    }))
  })) %>% mutate(index = row_number()) %>%
    group_by(index) %>%
    do(createBezierPoints(., mapping)) %>%
    data.frame
  
  ggplot() +
    geom_bezier(data = chordDf %>% filter(i != 1), aes(x, y, group = index, type = type, size = n), color = "gray72", show.legend = F) +
    geom_bezier(data = chordDf %>% filter(i == 1), aes(x, y, group = index, type = type, color = color, size = n), show.legend = F) +
    geom_point(data = pointDf %>% filter(fieldIndex == 1), aes(x, y, color = val, size = n*pointRatio), show.legend = F) +
    geom_point(data = pointDf %>% filter(fieldIndex != 1), aes(x, y, size = n*pointRatio), color = "gray72", show.legend = F) +
    coord_cartesian(ylim = c(-1*zoomScale,1*zoomScale), xlim = c(-1*zoomScale, 1*zoomScale)) +
    geom_text(data = pointDf, aes(x, y, label = val, angle = angle, hjust = hjust, vjust = vjust)) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.background = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
}