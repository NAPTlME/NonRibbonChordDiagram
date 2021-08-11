library(dplyr)
library(ggplot2)
library(ggforce)
library(rlang)


tmpTbl = mpg %>% select(class, manufacturer)

ggplot(tmpTbl %>% count(class)) +
  geom_point(aes(class, 1, size = n, color = class))

mapping = data.frame(val = c(tmpTbl$class, tmpTbl$manufacturer)) %>%
  group_by(val) %>%
  summarise(n = n()) %>% 
  data.frame() %>%
  mutate(key = row_number())

tmpTbl = tmpTbl %>%
  mutate(classKey = as.numeric(sapply(class, function(x) mapping$key[mapping$val == x])),
         manufacturerKey = as.numeric(sapply(manufacturer, function(x) mapping$key[mapping$val == x])))

ggplot(mapping) +
  geom_point(aes(val, 1, size = n, color = val))


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

# tests

ggplot(getPointsOnACircle(16)) +
  geom_point(aes(x, y))

ggplot(getPointsOnACircle(16)) +
  geom_point(aes(x, y)) +
  coord_fixed()

testParamFx = function(...) {
  x = list(...)
  for(val in x){
    print(val)
  }
}

testParamFx("ThisIsATest", "AnotherTest", "YetAnotherTest")

# test with symbols
testParamFx2 = function(...) {
  x = as.list(substitute(list(...)))[-1L]
  print(x)
  print(str(x))
}

testParamFx2(val, anotherVal, tmp)

testParamFx3 = function(...) {
  x = as.list(substitute(list(...)))[-1L]
  print(str(mpg %>% select(!!!x)))
}

testParamFx3(manufacturer, model, displ)

testParamFx4 = function(...) {
  x = as.list(substitute(list(...)))[-1L]
  for(val in x) {
    print(length(unique(mpg[[val]])))
  }
}

testParamFx4(manufacturer, model, displ)

# geom_bezier example (from documentation)

beziers <- data.frame(
  x = c(1, 2, 3, 4, 4, 6, 6),
  y = c(0, 2, 0, 0, 2, 2, 0),
  type = rep(c('cubic', 'quadratic'), c(3, 4)),
  point = c('end', 'control', 'end', 'end', 'control', 'control', 'end'),
  colour = letters[1:7]
)
help_lines <- data.frame(
  x = c(1, 3, 4, 6),
  xend = c(2, 2, 4, 6),
  y = 0,
  yend = 2
)

# See how control points affect the bezier
ggplot() +
  geom_segment(aes(x = x, xend = xend, y = y, yend = yend),
               data = help_lines,
               arrow = arrow(length = unit(c(0, 0, 0.5, 0.5), 'cm')),
               colour = 'grey') +
  geom_bezier(aes(x = x, y = y, group = type, linetype = type),
              data = beziers) +
  geom_point(aes(x = x, y = y, colour = point),
             data = beziers)

#### NonRibbonChordDiagram v1 ####

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

nonRibbonChordDiagram = function(df, ...) {
  fields = as.list(substitute(list(...)))[-1L]
  
  numPoints = sapply(fields, function(field) length(unique(df[[field]])))
  
  pos = getPointsOnACircle(sum(numPoints), numPoints[1])
  
  mapping = do.call(rbind, lapply(1:length(fields), function(i) {
    field = fields[[i]]
    data.frame(fieldIndex = i, val = unique(df[[field]]))
  })) %>% mutate(x = pos$x, y = pos$y)
  
  pointDf = do.call(rbind, lapply(1:length(fields), function(i) {
    field = fields[[i]]
    df %>% group_by(!!field) %>%
      count() %>% data.frame %>%
      setNames(c("val", "n")) %>% # allows for a common key and this route ensures user dataframe colnames don't impact what this value is set to
      left_join(mapping %>% filter(fieldIndex == i), by = "val")
  }))
  
  chordDf = do.call(rbind, lapply(1:(length(fields)-1), function(i) {
    field1 = fields[[i]]
    do.call(rbind, lapply((i+1):length(fields), function(j) {
      field2 = fields[[j]]
      # create three points for each line to make a cubic line using geom_bezier
      groupedFieldCount = df %>% 
        group_by(!!field1, !!field2) %>%
        count() %>% data.frame %>%
        setNames(c("val1", "val2", "n")) %>%
        mutate(index = row_number(),
               i = i, j = j,
               color = val1) %>% 
        group_by(index) %>%
        do(createBezierPoints(., mapping)) %>%
        data.frame
    }))
  }))
  
  ggplot() +
    geom_bezier(data = chordDf %>% filter(i == 1), aes(x, y, group = index, type = type, color = color, size = n), show.legend = F) +
    geom_bezier(data = chordDf %>% filter(i != 1), aes(x, y, group = index, type = type, size = n), color = "gray72", show.legend = F) +
    geom_point(data = pointDf %>% filter(fieldIndex == 1), aes(x, y, color = val, size = n), show.legend = F) +
    geom_point(data = pointDf %>% filter(fieldIndex != 1), aes(x, y, size = n), color = "gray72", show.legend = F)
}

nonRibbonChordDiagram(mpg, manufacturer, model)

#### NonRibbonChordDiagram v2 ####
# coord_fixed to fix ratio
# added labels
# adjusted angle and hjust of labels
# cleared theme info to get blank canvas
# set clip = "off" to allow writing outside the margins

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

nonRibbonChordDiagram = function(df, ...) {
  fields = as.list(substitute(list(...)))[-1L]
  
  numPoints = sapply(fields, function(field) length(unique(df[[field]])))
  
  pos = getPointsOnACircle(sum(numPoints), numPoints[1])
  
  mapping = do.call(rbind, lapply(1:length(fields), function(i) {
    field = fields[[i]]
    data.frame(fieldIndex = i, val = unique(df[[field]]))
  })) %>% mutate(x = pos$x, y = pos$y)
  
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
        mutate(index = row_number(),
               i = i, j = j,
               color = val1) %>% 
        group_by(index) %>%
        do(createBezierPoints(., mapping)) %>%
        data.frame
    }))
  }))
  
  ggplot() +
    geom_bezier(data = chordDf %>% filter(i == 1), aes(x, y, group = index, type = type, color = color, size = n), show.legend = F) +
    geom_bezier(data = chordDf %>% filter(i != 1), aes(x, y, group = index, type = type, size = n), color = "gray72", show.legend = F) +
    geom_point(data = pointDf %>% filter(fieldIndex == 1), aes(x, y, color = val, size = n), show.legend = F) +
    geom_point(data = pointDf %>% filter(fieldIndex != 1), aes(x, y, size = n), color = "gray72", show.legend = F) +
    coord_fixed(clip = "off") +
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

nonRibbonChordDiagram(mpg, manufacturer, model)

# test

tmp = getPointsOnACircle(16)
tmp = tmp %>% 
  mutate(text = paste(1:16 * 4, sapply(1:16, function(i) paste0(rep("Test", times = i), collapse = ""))),
         angle = radToDeg(atan2(y, x)),
         angle = ifelse(x < 0, angle + 180, angle),
         nchar = nchar(text),
         hOffset = 0.53 + 1.2^-nchar, # works well for up to 60 characters
         hjust = ifelse(x < 0, 0.5+hOffset, 0.5-hOffset),
         vjust = 0.3)

ggplot(tmp) +
  geom_point(aes(x, y)) +
  coord_equal(clip = "off") +
  #theme(aspect.ratio = 1) +
  geom_text(aes(x, y, label = text, angle = angle, hjust = hjust, vjust = vjust)) +
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

#### NonRibbonChordDiagram v3 ####
# Added zoomScale that zooms out from chart with larger numbers, making it easier to see text
# Added pointRatio that scales up the size of points (in the end makes lines smaller for cleaner viewing)

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

nonRibbonChordDiagram = function(df, ..., zoomScale = 1.5, pointRatio = 2) {
  fields = as.list(substitute(list(...)))[-1L]
  
  numPoints = sapply(fields, function(field) length(unique(df[[field]])))
  
  pos = getPointsOnACircle(sum(numPoints), numPoints[1])
  
  mapping = do.call(rbind, lapply(1:length(fields), function(i) {
    field = fields[[i]]
    data.frame(fieldIndex = i, val = unique(df[[field]]))
  })) %>% mutate(x = pos$x, y = pos$y)
  
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
        mutate(index = row_number(),
               i = i, j = j,
               color = val1) %>% 
        group_by(index) %>%
        do(createBezierPoints(., mapping)) %>%
        data.frame
    }))
  }))
  
  ggplot() +
    geom_bezier(data = chordDf %>% filter(i == 1), aes(x, y, group = index, type = type, color = color, size = n), show.legend = F) +
    geom_bezier(data = chordDf %>% filter(i != 1), aes(x, y, group = index, type = type, size = n), color = "gray72", show.legend = F) +
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

nonRibbonChordDiagram(mpg, manufacturer, model)

# test

tmp = getPointsOnACircle(16)
tmp = tmp %>% 
  mutate(text = paste(1:16 * 4, sapply(1:16, function(i) paste0(rep("Test", times = i), collapse = ""))),
         angle = radToDeg(atan2(y, x)),
         angle = ifelse(x < 0, angle + 180, angle),
         nchar = nchar(text),
         hOffset = 0.53 + 1.2^-nchar, # works well for up to 60 characters
         hjust = ifelse(x < 0, 0.5+hOffset, 0.5-hOffset),
         vjust = 0.3)

zoomScale = 10

ggplot(tmp) +
  geom_point(aes(x, y)) +
  #theme(aspect.ratio = 1) +
  geom_text(aes(x, y, label = text, angle = angle, hjust = hjust, vjust = vjust)) +
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
        axis.title.y = element_blank()) +
  coord_cartesian(ylim = c(-1*zoomScale,1*zoomScale), xlim = c(-1*zoomScale, 1*zoomScale))
  #coord_equal(clip = "off")

#### NonRibbonChordDiagram v4 ####
# Now can randomize placement of points
# Now defaults to ordering fields alphanumerically (by field) rather than by presence in dataframe
# convert all fields to character to allow fields with different types to be used
# updated to properly create beziers for more than 2 dimensions
# changed order of geoms to put colored beziers on top of the gray ones

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

nonRibbonChordDiagram(mpg, manufacturer, model, orderedFields = F, pointRatio = 2.5)

# test of capabilities with more than 2 features
nonRibbonChordDiagram(mpg, manufacturer, cyl, class, pointRatio = 2.5)
