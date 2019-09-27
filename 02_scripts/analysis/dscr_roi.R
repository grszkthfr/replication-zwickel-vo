# source("02_scripts/01-libraries.R")
# source("02_scripts/02-paths.R")
# source(path(path_scripts_data, "rois.R"))
source(path(path_scripts_fun, "calc_roi.R"))



### convert px into cm
px_in_cm = 53.136 / 1920 # assumes square pixel!?

dscr_roi <- df_roi %>%
    mutate(
        roi_w_px = roi_max_x-roi_min_x,
        roi_h_px = roi_max_y-roi_min_y,
        roi_w_cm = roi_w_px * px_in_cm,
        roi_h_cm = roi_h_px * px_in_cm,
        roi_w_dg = visAngle(roi_w_cm, 53.136),
        roi_h_dg = visAngle(roi_h_cm, 53.136)) %>%
    group_by(roi_id) %>%
    summarise(
        m_roi_size = mean(roi_size),
        sd_roi_size = sd(roi_size),
        m_roi_x = mean(roi_m_x),
        m_roi_y = mean(roi_m_y),
        m_roi_w_px = mean(roi_w_px),
        sd_roi_w_px = sd(roi_w_px),
        m_roi_h_px = mean(roi_h_px),
        sd_roi_h_px = sd(roi_h_px),
        m_roi_w_cm = mean(roi_w_cm),
        sd_roi_w_cm = sd(roi_w_cm),
        m_roi_h_cm = mean(roi_h_cm),
        sd_roi_h_cm = sd(roi_h_cm),
        m_roi_w_dg = mean(roi_w_dg),
        sd_roi_w_dg = sd(roi_w_dg),
        m_roi_h_dg = mean(roi_h_dg),
        sd_roi_h_dg = sd(roi_h_dg),)

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
#         xmin = 1280 - (m_roi_x - m_roi_w_px/2),
#         xmax = 1280 - (m_roi_x + m_roi_w_px/2),
#         ymin = 960 - (m_roi_y - m_h_px/2),
#         ymax = 960 - (m_roi_y + m_h_px/2)) %>%
#     # filter(roi_id != "background") %>%
# ggplot() +
#     geom_rect(
#         aes(
#             xmin = xmin, xmax = xmax,
#             ymin = ymin, ymax = ymax,
#             fill = roi_id), alpha = .5)
