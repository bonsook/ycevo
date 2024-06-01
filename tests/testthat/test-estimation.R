set.seed(1)
bonds <- ycevo_data(n = 10)
res_both <- ycevo(bonds, x = lubridate::ymd(c("2023-03-01"), c("2023-06-01")))
res <- res_both[1,]

test_that("Simulation example", {
  expect_equal(
    head(res$.est[[1]])$tau,
    c(0.0821917808219178, 0.164383561643836, 0.246575342465753, 0.328767123287671,
      0.410958904109589, 0.493150684931507)
  )
  expect_equal(
    tail(res$.est[[1]])$tau,
    c(14.1369863013699, 15.7808219178082, 16.1095890410959, 16.4383561643836,
      18.0821917808219, 18.4109589041096)
  )
  expect_equal(
    head(res$.est[[1]])$.discount,
    c(0.986225688985458, 0.997535179957893, 0.987335706233562, 0.981574003055008,
      0.986434647687132, 0.984658120829134)
  )
  expect_equal(
    tail(res$.est[[1]])$.discount,
    c(0.25173267449438, 0.169062343884191, 0.166603074969578, 0.160818163510621,
      0.100182473682101, 0.0998222330332958)
  )
  expect_equal(
    head(res$.est[[1]])$.yield,
    c(0.168752361106433, 0.0150128314968472, 0.0516887429025604,
      0.0565685218322941, 0.0332349590672765, 0.0313510332302356)
  )
  expect_equal(
    tail(res$.est[[1]])$.yield,
    c(0.0975729579577046, 0.11263594143703, 0.111246853513618, 0.111171759097201,
      0.127239111655866, 0.125162646692807)
  )
})

augmented_res <- augment(res, loess = FALSE)

test_that("Augmentation without loess", {
  expect_equal(
    head(augmented_res)$tau,
    c(0.0821917808219178, 0.164383561643836, 0.246575342465753, 0.328767123287671,
      0.410958904109589, 0.493150684931507)
  )
  expect_equal(
    tail(augmented_res)$tau,
    c(14.1369863013699, 15.7808219178082, 16.1095890410959, 16.4383561643836,
      18.0821917808219, 18.4109589041096)
  )
  expect_equal(
    head(augmented_res)$.discount,
    c(0.986225688985458, 0.997535179957893, 0.987335706233562, 0.981574003055008,
      0.986434647687132, 0.984658120829134)
  )
  expect_equal(
    tail(augmented_res)$.discount,
    c(0.25173267449438, 0.169062343884191, 0.166603074969578, 0.160818163510621,
      0.100182473682101, 0.0998222330332958)
  )
  expect_equal(
    head(augmented_res)$.yield,
    c(0.168752361106433, 0.0150128314968472, 0.0516887429025604,
      0.0565685218322941, 0.0332349590672765, 0.0313510332302356)
  )
  expect_equal(
    tail(augmented_res)$.yield,
    c(0.0975729579577046, 0.11263594143703, 0.111246853513618, 0.111171759097201,
      0.127239111655866, 0.125162646692807)
  )
})

loess_res <- augment(res, loess = TRUE)
test_that("Augmentation with loess", {
  expect_equal(
    head(loess_res)$tau,
    c(0.0821917808219178, 0.164383561643836, 0.246575342465753, 0.328767123287671,
      0.410958904109589, 0.493150684931507)
  )
  expect_equal(
    tail(loess_res)$tau,
    c(14.1369863013699, 15.7808219178082, 16.1095890410959, 16.4383561643836,
      18.0821917808219, 18.4109589041096)
  )
  expect_equal(
    head(loess_res)$.discount,
    c(0.988883162893306, 0.987501222017339, 0.986055618108189, 0.984546532186519,
      0.982974149614928, 0.981338672708148)
  )
  expect_equal(
    tail(loess_res)$.discount,
    c(0.244210089032949, 0.175654062323753, 0.1638048559747, 0.15255635355698,
      0.104834023072191, 0.0968784681344841)
  )
  expect_equal(
    head(loess_res)$.yield,
    c(0.136012273194597, 0.0765133970570866, 0.0569502125082803,
      0.0473712732385921, 0.0417863111143431, 0.0381985617233583)
  )
  expect_equal(
    tail(loess_res)$.yield,
    c(0.0997190188541892, 0.110212179202032, 0.112298299952432, 0.114380124196802,
      0.124729177717189, 0.126788506997888)
  )
})

newdata <- data.frame(
  qdate = lubridate::ymd(c("2023-02-01", "2023-03-01","2023-04-01", "2023-06-01", "2023-07-01")),
  tau = c(10)
)
predict_res <- augment(res_both, newdata = newdata)
test_that("Prediction", {
  expect_equal(predict_res$qdate, newdata$qdate)
  expect_equal(predict_res$tau, rep(10, 5))
  expect_equal(predict_res$.discount,
               c(0.48348414263405, 0.48348414263405, 0.495452546406272, 0.519875259439203,
                 0.519875259439203))
  expect_equal(predict_res$.yield,
               c(0.0726736761660645, 0.0726736761660645, 0.0702283698907689,
                 0.065416638187768, 0.065416638187768))
})


data <- data.frame(
  mat_days = c(180, 360, 360, 540, 540, 540, 720, 720, 720, 720),
  qdate = 0,
  coupon = c(0, 1, 1, 1, 1, 1, 1, 1, 1, 1),
  id = c(1L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 4L),
  pdint = c(100, 101, 1, 101, 1, 1, 101, 1, 1, 1),
  tupq = c(180, 360, 180, 540, 360, 180, 720, 540, 360, 180)
)

data$discount <- exp(-get_yield_at(0, data$tupq/365) * data$tupq/365)
data$price <- rep(tapply(data$pdint * data$discount, data$id, sum), times = rle(data$id)$lengths)

mat_weights_qdatetime <- get_weights(1,1, length(unique(data$qdate)))
tupq_idx <- structure(c(2L, 182L, 362L, 542L, 358L, 538L, 718L, 720L), dim = c(4L, 2L))
mat_weights_tau <- get_weights(
  c(180, 360, 540, 720) / 365,
  c(180, 180, 180, 180) / 365,
  len = as.integer(max(data$tupq)),
  units = 365)
cfp_slist <- ycevo:::get_cfp_slist(data)
price_slist <- cfp_slist$price_slist
cf_slist <- cfp_slist$cf_slist
test_that("Simplest example cpp", {
  expect_equal(
    calc_dbar_c(4L,
                structure(c(1L, 1L), dim = 1:2),
                tupq_idx,
                mat_weights_tau, mat_weights_qdatetime, price_slist, cf_slist),
    structure(c(5678.94832046269, 5705.41441494833, 5625.45900788638,
                5538.31638436822, 5626.6875, 5739.1875, 5738.625, 5738.0625), dim = c(4L, 2L)))
})
