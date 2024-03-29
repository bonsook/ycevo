test_that("Simplest example", {
  expect_equal(
    suppressWarnings(estimate_yield(
      structure(list(
        mat_days = c(
          180, 360, 360, 540, 540, 540, 720,
          720, 720, 720
        ), qdate = structure(c(
          18262, 18262, 18262, 18262,
          18262, 18262, 18262, 18262, 18262, 18262
        ), class = "Date"), issue_date = structure(c(
          18262,
          18262, 18262, 18262, 18262, 18262, 18262, 18262, 18262, 18262
        ), class = "Date"), coupon = c(0, 1, 1, 1, 1, 1, 1, 1, 1, 1),
        id = c(1L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 4L), mat_date = structure(c(
          18442,
          18622, 18622, 18802, 18802, 18802, 18982, 18982, 18982, 18982
        ), class = "Date"), paydates = structure(c(
          18442, 18622,
          18442, 18802, 18622, 18442, 18982, 18802, 18622, 18442
        ), class = "Date"),
        pdint = c(100, 101, 1, 101, 1, 1, 101, 1, 1, 1), tumat = c(
          180,
          360, 360, 540, 540, 540, 720, 720, 720, 720
        ), tupq = c(
          180,
          360, 180, 540, 360, 180, 720, 540, 360, 180
        ), accint = c(
          0,
          0, 0, 0, 0, 0, 0, 0, 0, 0
        ), discount = c(
          0.980188189018042,
          0.965437020388093, 0.980188189018042, 0.951556097548637,
          0.965437020388093, 0.980188189018042, 0.936504311580234,
          0.951556097548637, 0.965437020388093, 0.980188189018042
        ),
        mid.price = c(
          98.0188189018042, 98.4893272482154, 98.4893272482154,
          98.0527910618185, 98.0527910618185, 98.0527910618185, 97.4841167765584,
          97.4841167765584, 97.4841167765584, 97.4841167765584
        )
      ), class = c(
        "tbl_df",
        "tbl", "data.frame"
      ), row.names = c(NA, -10L)),
      xgrid = 0.1,
      hx = Inf,
      tau = c(0.493150684931507, 0.986301369863014, 1.47945205479452, 1.97260273972603),
      ht = c(0.18, 0.18, 0.18, 0.18)
    )$yield),
    get_yield_at_vec(rep(0, 4), c(0.493150684931507, 0.986301369863014, 1.47945205479452, 1.97260273972603))
  )
})


data <- structure(list(mat_days = c(180, 360, 360, 540, 540, 540, 720, 
                                    720, 720, 720), qdate = structure(c(18262, 18262, 18262, 18262, 
                                                                        18262, 18262, 18262, 18262, 18262, 18262), class = "Date"), issue_date = structure(c(18262, 
                                                                                                                                                             18262, 18262, 18262, 18262, 18262, 18262, 18262, 18262, 18262
                                                                        ), class = "Date"), coupon = c(0, 1, 1, 1, 1, 1, 1, 1, 1, 1), 
                       id = c(1L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 4L), mat_date = structure(c(18442, 
                                                                                                  18622, 18622, 18802, 18802, 18802, 18982, 18982, 18982, 18982
                       ), class = "Date"), paydates = structure(c(18442, 18622, 
                                                                  18442, 18802, 18622, 18442, 18982, 18802, 18622, 18442), class = "Date"), 
                       pdint = c(100, 101, 1, 101, 1, 1, 101, 1, 1, 1), tumat = c(180, 
                                                                                  360, 360, 540, 540, 540, 720, 720, 720, 720), tupq = c(180, 
                                                                                                                                         360, 180, 540, 360, 180, 720, 540, 360, 180), accint = c(0, 
                                                                                                                                                                                                  0, 0, 0, 0, 0, 0, 0, 0, 0), discount = c(0.980188189018042, 
                                                                                                                                                                                                                                           0.965437020388093, 0.980188189018042, 0.951556097548637, 
                                                                                                                                                                                                                                           0.965437020388093, 0.980188189018042, 0.936504311580234, 
                                                                                                                                                                                                                                           0.951556097548637, 0.965437020388093, 0.980188189018042), 
                       mid.price = c(98.0188189018042, 98.4893272482154, 98.4893272482154, 
                                     98.0527910618185, 98.0527910618185, 98.0527910618185, 97.4841167765584, 
                                     97.4841167765584, 97.4841167765584, 97.4841167765584)), class = c("tbl_df", 
                                                                                                       "tbl", "data.frame"), row.names = c(NA, -10L))

mat_weights_qdatetime <- get_weights(1,1, length(unique(data$qdate)))
tupq_idx <- structure(c(2L, 182L, 362L, 542L, 358L, 538L, 718L, 720L), dim = c(4L, 2L))
mat_weights_tau <- get_weights(
  c(0.493150684931507, 0.986301369863014, 1.47945205479452, 1.97260273972603), 
  c(0.493150684931507, 0.493150684931507, 0.49315068493151, 0.49315068493151), 
  len = as.integer(max(data$tupq)),
  units = 365)
cfp_slist <- ycevo:::get_cfp_slist(data)
price_slist <- cfp_slist$price_slist
cf_slist <- cfp_slist$cf_slist
test_that("Simplest example", {
  expect_equal(
    calc_dbar_c(1L, 4L, 
                structure(c(1L, 1L), dim = 1:2), 
                tupq_idx, 
                mat_weights_tau, mat_weights_qdatetime, price_slist, cf_slist),
    structure(c(5678.94832046269, 5705.41441494833, 5625.45900788638, 
                5538.31638436822, 5626.6875, 5739.1875, 5738.625, 5738.0625), dim = c(4L, 2L)))
})
