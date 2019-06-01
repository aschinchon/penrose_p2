# Load in libraries
library(tidyverse)

# This function creates a kite. 
# A, B, C are angles
# Parameter c is the only input of the function: is the longitude of side related to angle C.
CreateKite <- function() {
  c <- 5000
  A <- 36
  B <- 72
  C <- 180 - (A + B)
  b <- sin(B*pi/180)*(c/sin(C*pi/180))
  x <- c(0, c, b*cos(A*pi/180))
  y <- c(0, 0, b*sin(A*pi/180))
  st <- c(3, 1, 2)
  xend <- lead(x, default = 0)
  yend <- lead(y, default = 0)
  tibble(x = x, y = y, xend = xend, yend = yend, st = st) %>% 
    bind_rows(tibble(x = x, y = -y, xend = xend, yend = -yend, st = st))
}

# This function rotates a polygon. I use it to rotate the initial kite to create
# the initial pattern of the tessellation.
# The initial pattern from which iterate to create tessellation is composed by 5 kites.
RotatePolygon <- function(df, angle) {
  df %>% transmute(x2    = cos(angle*pi/180)*x - sin(angle*pi/180)*y,
                   y2    = sin(angle*pi/180)*x + cos(angle*pi/180)*y,
                   xend2 = cos(angle*pi/180)*xend - sin(angle*pi/180)*yend,
                   yend2 = sin(angle*pi/180)*xend + cos(angle*pi/180)*yend,
                   st = st) %>% 
    rename_all(str_replace_all, "2", "")
}



# ggplot(init_polygon)+
#   geom_segment(aes(x = x, y = y, xend = xend, yend = yend))+
#   coord_equal() +
#   theme_void()

# In each interation I have to divide two kind of triangles. I named them Triangle123
# and Triangle124 following the specs of this site:
# https://tartarus.org/~simon/20110412-penrose/penrose.xhtml
# The sides of triangles 123 are labeled with 1, 2 and 3 and are divided into three
# other triangles using this function.
# This function receives a triangle of type 123 and divides it. It returns three
# other subtriangles: two of type 123 and another one of type 124
DivideTriangle123 <- function(df) {
  # Add longitude of sides to df
  df %>% 
    mutate(lng = sqrt((xend-x)^2+(yend-y)^2)) -> df
  
  # Longitude of the new side 3
  df %>% 
    filter(st == 1) %>% 
    select(lng) %>% as.numeric -> d1
  
  # Gradient of the new side 3
  df %>% 
    filter(st == 3) %>%
    transmute(gx = xend - x,
              gy = yend - y) %>% as.numeric() -> g1
  
  # Inner point of side 3 to divide the triangle
  df %>% filter(st == 3) %>% select(x, y) %>% 
    as.numeric() + d1 * g1/sqrt(sum(g1^2)) -> p1
  
  # # Longitude
  # df %>% 
  #   filter(st == 1) %>% 
  #   select(lng) %>% as.numeric -> d2
  # 
  
  # Gradient to find the second point p2
  df %>% 
    filter(st == 2) %>%
    transmute(gx = xend - x,
              gy = yend - y) %>% as.numeric() -> g2
  
  # Point p2
  df %>% 
    filter(st == 2) %>%
    select(x, y) %>% 
    as.numeric() + d1 * g2/sqrt(sum(g2^2)) -> p2
  
  # df1: subtriangle (type 123)
        c(p1, 
          df %>% filter(st == 3) %>% select(xend, yend) %>% as.numeric()) %>% 
  rbind(c(df %>% filter(st == 1) %>% select(x, y) %>% as.numeric(), 
          df %>% filter(st == 1) %>% select(xend, yend) %>% as.numeric())) %>% 
  rbind(c(df %>% filter(st == 1) %>% select(xend, yend) %>% as.numeric(), 
            p1)) -> df1
  
  colnames(df1) <- c("x", "y", "xend", "yend")
  
  # st: labels of new sides
  as_tibble(df1) %>% mutate(st = 1:3) -> df1
  
  # df2: subtriangle (type 123)
        c(p1, 
          p2) %>% 
  rbind(c(p2, 
          df %>% filter(st == 1) %>% select(xend, yend) %>% as.numeric())) %>% 
  rbind(c(df %>% filter(st == 1) %>% select(xend, yend) %>% as.numeric(), 
          p1)) -> df2
  
  colnames(df2) <- c("x", "y", "xend", "yend")
  as_tibble(df2) %>% mutate(st = 1:3) -> df2
  
  # df3 subtriangle (type 124)
        c(p1, 
          p2) %>% 
  rbind(c(p2, 
          df %>% filter(st == 3) %>% select(x, y) %>% as.numeric())) %>% 
  rbind(c(df %>% filter(st == 3) %>% select(x, y) %>% as.numeric(), 
          p1)) -> df3

  colnames(df3) <- c("x", "y", "xend", "yend")
  as_tibble(df3) %>% mutate(st = c(1, 4, 2)) -> df3
  
  # Binding three triangles
  bind_rows(df1, df2, df3) %>% round(0)
  
}

# In each interation I have to divide two kind of triangles. I named them Triangle123
# and Triangle124 following the specs of this site:
# https://tartarus.org/~simon/20110412-penrose/penrose.xhtml
# The sides of triangles 124 are labeled with 1, 2 and 4 and are divided into two
# other triangles using this function.
# This function receives a triangle of type 124 and divides it. It returns two
# other subtriangles: two of type 123 and another one of type 124
DivideTriangle124 <- function(df) {
  # Add longitude  
  df %>% 
    mutate(lng = sqrt((xend-x)^2+(yend-y)^2)) -> df
  
  df %>% 
    filter(st == 1) %>% 
    select(lng) %>% as.numeric -> d1
  
  # This is the longitude of the inner side to subdivide triangle
  x <- d1*sin(36*pi/180)/sin(108*pi/180)
  
  # Gradient to calculate point
  df %>% 
    filter(st == 2) %>%
    transmute(gx = x - xend,
              gy = y - yend) %>% as.numeric() -> g2
  
  # Inner point side 2
  df %>% 
    filter(st == 2) %>%
    select(xend, yend) %>% as.numeric() + x * g2/sqrt(sum(g2^2)) -> p1
  
  # df1 subtriangle (type 123)
          c(df %>% filter(st == 4) %>% select(x, y) %>% as.numeric(),
            p1) %>% 
    rbind(c(p1, 
            df %>% filter(st == 2) %>% select(x, y) %>% as.numeric())) %>% 
      rbind(c(df %>% filter(st == 2) %>% select(x, y) %>% as.numeric(), 
            df %>% filter(st == 4) %>% select(x, y) %>% as.numeric())) -> df1
  
  colnames(df1) <- c("x", "y", "xend", "yend")
  as_tibble(df1) %>% mutate(st = 1:3) -> df1
  
  # df2 subtriangle (type 124)
        c(df %>% filter(st == 4) %>% select(x, y) %>% as.numeric(),
          p1) %>% 
  rbind(c(p1, 
          df %>% filter(st == 2) %>% select(xend, yend) %>% as.numeric())) %>% 
  rbind(c(df %>% filter(st == 2) %>% select(xend, yend) %>% as.numeric(), 
          df %>% filter(st == 4) %>% select(x, y) %>% as.numeric())) -> df2
        
  colnames(df2) <- c("x", "y", "xend", "yend")
  as_tibble(df2) %>% mutate(st = c(1,4,2)) -> df2
  # Binding two triangles
  bind_rows(df1, df2) %>% round(0)
  
}


# This function gathers both previous functions to divide a generic triangle depending
# of its type
Divide <- function(df)
{
  if(df %>% select(st) %>% sum() %>% as.numeric == 6)
    DivideTriangle123(df)
  else
    DivideTriangle124(df)
}

# This function receives a data frame composed by triangles and divides it. 
# I use it to iterate
Divide_df <- function(df){
  df %>% 
    mutate(poly_id=rep(1:((nrow(.))/3), each=3)) %>% 
    group_by(poly_id) %>% 
    group_split() %>% lapply(Divide) %>%  
    bind_rows()
}

# This function performs two important tasks:
#   1. Group points (x,y), (xend, yend) to fix problems of rounding 
#      using hierarchical clustering
#   2. Gather adjacent triangles sharing sides of type 3 or 4 and remove them
Arrange_df <- function(df){
  
  df %>% round(0) -> df
  
  # Unique points which define segments
  bind_rows(df %>% select(xx = x, yy = y),
            df %>% select(xx = xend, yy = yend)) %>% unique -> points
  
  # Distance betweem points
  dist_mat <- dist(points, method = 'euclidean')
  hclust_avg <- hclust(dist_mat, method = 'average')
  
  # Group points
  points %>% mutate(group = cutree(hclust_avg, h = 8)) -> points
  
  # Centroid of each cluster
  points %>% group_by(group) %>% summarize(xnew = mean(xx), 
                                           ynew = mean(yy)) -> mean_points

  # Substitute each point by its centroid
  points %>% inner_join(mean_points, by = "group") -> points
  
  # Recreate df
  df %>% 
    inner_join(points, by = c("x"="xx", "y"="yy")) %>% 
    mutate(x = xnew,
           y = ynew) %>% 
    select(-xnew, -ynew) %>% 
    left_join(points, by = c("xend"="xx", "yend"="yy")) %>%   
    mutate(xend = xnew,
           yend = ynew) %>% 
    select(-xnew, -ynew) -> df
  
  # Create a new column called poly_id to differenciate triangles
  df %>% mutate(poly_id = rep(1:((nrow(.))/3), each=3) %>% as.character) -> df
  
  # These are the segments to remove
  df %>% filter(st %in% c(3,4)) -> df_sub 
  
  # df_new contains the ids of the new polygons as result of merging adjacent ones 
  # which share a side of type 3 or 4
  union(df_sub %>%
          inner_join(df_sub, by = c("x","y", "xend", "yend"), suffix = c(".1", ".2")),
        df_sub %>%
          inner_join(df_sub, by = c("x"="xend", "y"="yend","xend"="x", "yend"="y"), suffix = c(".1", ".2"))) %>%
    filter(poly_id.1 != poly_id.2) %>%
    select(poly_id.1, poly_id.2) %>%
    mutate(poly_id_new=apply(., 1, FUN = function (r) sort(r) %>% paste(collapse="_"))) %>%
    gather(condition, poly_id, poly_id.1:poly_id.2) %>%
    select(-condition) %>%
    distinct -> df_new
  
  # New df with definitive polygons
  df %>%
    inner_join(df_new, by = "poly_id") %>% 
    mutate(remove = st %in% c(3, 4) & !is.na(poly_id_new),
           poly_id = ifelse(is.na(poly_id_new), poly_id, poly_id_new)) %>% 
    filter(remove == FALSE) %>% 
    select(-poly_id_new, -remove) %>% unique -> df
  
  
  return(df)
  
}

# To use geom_polygon I have to convert my data frame from (x, y, xend, yend) structure
# to (x,y). I do it with the next function
Create_Polygon <- function(df)
{
  bind_rows(inner_join(df %>% slice(1),
                       df %>% slice(-1), 
                       by = c("xend" = "x", "yend" = "y")) %>% 
              select(x1 = x, y1 = y, x2 = xend, y2 = yend, x3 = xend.y, y3 = yend.y),
   
            inner_join(df %>% slice(1),
                       df %>% slice(-1), 
                       by = c("xend" = "xend", "yend" = "yend")) %>% 
              select(x1=x.x, y1=y.x, x2=xend, y2=yend, x3 = x.y, y3 = y.y)) -> temp
  
  bind_rows(temp %>% select(x = x1, y = y1),
            temp %>% select(x = x2, y = y2),
            temp %>% select(x = x3, y = y3)) -> sub1
  
  bind_rows(df %>% select(x,y),
            df %>% select(x=xend,y=yend)) %>% unique -> points
  
  bind_rows(sub1,
            points %>% anti_join(sub1, by = c("x", "y")),
            sub1 %>% slice(1))
  
}



# Depth of the tessellation (more than 6 takes so long time)
niter <- 5

init_kite <- CreateKite()

# I create an initial pattern if composed by 5 kites, rotating the init_kite from
# the origin an angle of 72 degrees.
seq(from = 0, by=72, length.out = 5) %>% 
  lapply(function(x) RotatePolygon(init_kite, angle = x)) %>% 
  bind_rows() %>% 
  round(0) -> init_polygon


# Raw Tessellation
reduce(
  .x = 1:niter,
  .f = function(old, y) {Divide_df(old)},
  .init=init_polygon) -> raw_tessellation


# Tessellation segment struct
segment_tessellation <- Arrange_df(raw_tessellation)


segment_tessellation %>% group_by(poly_id) %>% 
  group_split() %>% lapply(Create_Polygon) %>%  
  bind_rows(.id = "poly_id") -> poly_tessellation

# Calculate areas of polygons using Shoelace formula
poly_tessellation %>% 
  group_by(poly_id) %>% 
  slice(-1) %>% 
  mutate(ldx = lead(x, default = first(x)), 
         ldy = lead(y, default = first(y))) %>% 
  summarize(area=0.5*abs(sum(x*ldy - ldx*y))) -> areas

# Add areas to polygons
poly_tessellation %>% inner_join(areas, by = "poly_id") -> poly_tessellation

# Pick two colorus you like and 
ggplot(poly_tessellation) + 
  geom_polygon(aes(x=x, y=y, group = poly_id, fill = area), 
               col = "gray65",  
               lwd = 0,
               show.legend = NA) + 
  scale_fill_gradientn(colours = c("darkmagenta", "gainsboro")) +
  coord_equal() + 
  theme_void() + 
  theme(legend.position='none') 

ggsave("penrose5.png", width = 3, height = 3)


