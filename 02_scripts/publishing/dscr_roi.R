# source("02_scripts/01-libraries.R")
# source("02_scripts/02-paths.R")
# source(path(path_scripts_data, "rois.R"))

distance <- function(a_x, a_y, b_x, b_y){
    
    distance <- sqrt((b_x - a_x)**2 + (b_y - a_y)**2)
    return(distance)
}

### http://stephenrho.github.io/visual-angle.html
visAngle <- function(size, distance){
    # this function calculates visual angle
    # size and distance must be in the same units
    Rad = 2*atan(size/(2*distance))
    Ang = Rad*(180/pi)
    return(Ang)
}

### convert px into cm
    px_in_cm = 53.136 / 1920 # assumes square pixel!?

dscr_roi <- df_roi %>% 
    mutate(
        roi_width_px = roi_max_x-roi_min_x,
        roi_height_px = roi_max_y-roi_min_y,
        roi_width_cm = roi_width_px * px_in_cm,
        roi_height_cm = roi_height_px * px_in_cm,
        roi_width_dg = visAngle(roi_width_cm, 53.136),
        roi_height_dg = visAngle(roi_height_cm, 53.136)) %>% 
    group_by(roi_id) %>% 
    summarise(
        m_roi_x = mean(roi_m_x),
        m_roi_y = mean(roi_m_y),
        m_width_px = mean(roi_width_px),
        sd_width_px = sd(roi_width_px),
        m_height_px = mean(roi_height_px),
        sd_height_px = sd(roi_height_px),
        m_width_cm = mean(roi_width_cm),
        sd_width_cm = sd(roi_width_cm),
        m_height_cm = mean(roi_height_cm),
        sd_height_cm = sd(roi_height_cm),  
        m_width_dg = mean(roi_width_dg),
        sd_width_dg = sd(roi_width_dg),
        m_height_dg = mean(roi_height_dg),
        sd_height_dg = sd(roi_height_dg),)

dscr_roi_dist <- 
    bind_rows(
        df_roi %>%
            select(stim_id, roi_id, roi_m_x, roi_m_y) %>%
            filter(roi_id == "head" | roi_id == "object_cued") %>% 
            unite(x_y, roi_m_x, roi_m_y) %>% 
            spread(roi_id, x_y) %>% 
            separate(head, c("head_x", "head_y"), sep = "\\_") %>% 
            separate(object_cued, c("cued_x", "cued_y"), sep = "\\_") %>%
            mutate_at(vars(head_x:cued_y), as.double) %>% 
            group_by(stim_id) %>% 
            summarise(
                dist_id = "head_cued",
                dist_px = distance(head_x, head_y, cued_x, cued_y),
                dist_cm = dist_px * px_in_cm,
                dist_dg = visAngle(dist_cm, 53.136))%>% 
            summarise(
                dist_id = "head_cued",
                m_dist_px = mean(dist_px),
                sd_dist_px = sd(dist_px),
                m_dist_cm = mean(dist_cm),
                sd_dist_cm = sd(dist_cm),
                m_dist_dg = mean(dist_dg),
                sd_dist_dg = sd(dist_dg)),
        df_roi %>%
            select(stim_id, roi_id, roi_m_x, roi_m_y) %>%
            filter(roi_id == "head" | roi_id == "object_uncued") %>% 
            unite(x_y, roi_m_x, roi_m_y) %>% 
            spread(roi_id, x_y) %>% 
            separate(head, c("head_x", "head_y"), sep = "\\_") %>% 
            separate(object_uncued, c("uncued_x", "uncued_y"), sep = "\\_") %>%
            mutate_at(vars(head_x:uncued_y), as.double) %>% 
            group_by(stim_id) %>% 
            summarise(
                dist_id = "head_uncued",
                dist_px = distance(head_x, head_y, uncued_x, uncued_y),
                dist_cm = dist_px * px_in_cm,
                dist_dg = visAngle(dist_cm, 53.136)) %>% 
            summarise(
                dist_id = "head_uncued",
                m_dist_px = mean(dist_px),
                sd_dist_px = sd(dist_px),
                m_dist_cm = mean(dist_cm),
                sd_dist_cm = sd(dist_cm),
                m_dist_dg = mean(dist_dg),
                sd_dist_dg = sd(dist_dg)),
        df_roi %>%
            select(stim_id, roi_id, roi_m_x, roi_m_y) %>%
            filter(roi_id == "object_cued" | roi_id == "object_uncued") %>% 
            unite(x_y, roi_m_x, roi_m_y) %>% 
            spread(roi_id, x_y) %>% 
            separate(object_cued, c("cued_x", "cued_y"), sep = "\\_") %>%
            separate(object_uncued, c("uncued_x", "uncued_y"), sep = "\\_") %>%
            mutate_at(vars(cued_x:uncued_y), as.double) %>% 
            group_by(stim_id) %>% 
            summarise(
                dist_id = "cued_uncued",
                dist_px = distance(cued_x, cued_y, uncued_x, uncued_y),
                dist_cm = dist_px * px_in_cm,
                dist_dg = visAngle(dist_cm, 53.136)) %>%  
            summarise(
                dist_id = "cued_uncued",
                m_dist_px = mean(dist_px),
                sd_dist_px = sd(dist_px),
                m_dist_cm = mean(dist_cm),
                sd_dist_cm = sd(dist_cm),
                m_dist_dg = mean(dist_dg),
                sd_dist_dg = sd(dist_dg))
        )

# dscr_roi %>% 
#     group_by(roi_id) %>% 
#     summarise(
#         xmin = 1280 - (m_roi_x - m_width_px/2),
#         xmax = 1280 - (m_roi_x + m_width_px/2),
#         ymin = 960 - (m_roi_y - m_height_px/2),
#         ymax = 960 - (m_roi_y + m_height_px/2)) %>% 
#     # filter(roi_id != "background") %>% 
# ggplot() +
#     geom_rect(
#         aes(
#             xmin = xmin, xmax = xmax,
#             ymin = ymin, ymax = ymax,
#             fill = roi_id), alpha = .5)
