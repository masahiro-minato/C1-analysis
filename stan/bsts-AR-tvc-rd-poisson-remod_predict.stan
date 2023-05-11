functions {
  // 並列処理用関数
  real partial_sum_lpmf (array[] int slice_y, int start, int end, vector lambda){
    return poisson_log_lpmf(slice_y | lambda[start:end]);
  }
}

data {
  int T;              // データ取得期間の長さ
  int pred_term;      // 予測期間の長さ
  array[T] int y;     // 観測値
  // int y[T];        // 観測値 旧表記
  vector[T] Num;      // 稼働台数
  real N_max;         // 稼働台数を正規化時の除算数
  int Num_gain;       // 増加台数予測値
  int grainsize;      // 粒度　250?
}

parameters {
  vector[T] r_raw;        // ランダム効果の再パラメータ化
  real<lower=0> s_w;      // 水準成分の変動の大きさを示す標準偏差
  real<lower=0> s_s;      // 季節変動の大きさを表す標準偏差
  real<lower=0> s_r;      // ランダム効果の標準偏差
  real<lower=0> s_t;      // 時変係数（稼働台数）の標準偏差
  real b_ar;              // 自己回帰項の係数
  real Intercept;         // 切片
  vector[T] mu_err;       // 水準成分の増減量
  vector[T] b_ope_err;    // 稼働台数項の係数の増減量
  vector[T] gamma_err;    // 季節成分の推定値
  // 予測範囲に関するパラメータ
  vector[T+pred_term] r_raw_pred;      // ランダム効果の再パラメータ化
  vector[pred_term] mu_err_pred;       // 水準成分の増減量
  vector[pred_term] b_ope_err_pred;    // 稼働台数項の係数の増減量
  vector[pred_term] gamma_err_pred;    // 季節成分の推定値
}

transformed parameters {
  vector[T] mu;           // 水準+ドリフト成分の推定値
  vector[T] b_ope;        // 稼働台数項の係数
  vector[T] lambda;       // 観測値の期待値のlogをとった値
  vector[T-6] sumgamma;   // 周期成分
  vector[T] tvc;          // 時変係数*稼働台数成分
  vector[T] r;            // ランダム効果
  vector[T] gamma;        // 季節成分の推定値
  
  // 再パラメータ化
  r = s_r * r_raw;
  // 1時点目
  mu[1] = Intercept + b_ar * mu_err[1];
  b_ope[1] = b_ope_err[1];
  // 2時点目以降
  for (i in 2:T) {
    // 標準偏差1の正規乱数にs_wをかけて実際の過程誤差としている
    mu[i] = Intercept + b_ar * mu[i-1] + s_w * mu_err[i];
    b_ope[i] = b_ope[i-1] + s_t * b_ope_err[i];
  }
  // 周期成分
  gamma[1:6] = gamma_err[1:6];
  for(i in 1:T-6) {
    sumgamma[i] = -sum(gamma[(i):(i+5)]);
    gamma[i+6] = sumgamma[i] + s_s * gamma_err[i];
  }
  // 状態推定値 λ
  for(i in 1:T) {
    tvc[i] = b_ope[i] * Num[i];
    lambda[i] = mu[i] + gamma[i] + tvc[i] + r[i];
  }
}

model {
  // 弱情報事前分布
  s_w ~ normal(0, 0.5);
  s_s ~ normal(0, 0.2);
  s_r ~ normal(0, 0.5);
  s_t ~ normal(0, 0.5);
  b_ar ~ student_t(3, 0, 2);
  Intercept ~ student_t(3, 0, 2);
  
  // 時点ごとに加わるランダム効果
  // r ~ normal(0, s_r);
  r_raw ~ normal(0,1);
  // 稼働台数の時変係数
  // b_ope[2:T] ~ normal(b_ope[1:T-1], s_t);
  b_ope_err[2:T] ~ normal(0,1);
  // 水準+自己回帰成分
  // mu[2:T] ~ normal(Intercept + b_ar * mu[1:T-1], s_w);
  // mu_err[2]以降は、標準偏差1の正規乱数を得る
  mu_err[2:T] ~ normal(0,1);
  // 季節成分
  // gamma[7:T] ~ normal(sumgamma[1:T-6], s_s);
  gamma_err[2:T] ~ normal(0,1);
  // 予測用の増減量正規乱数
  r_raw_pred ~ normal(0,1);
  b_ope_err_pred ~ normal(0,1);
  mu_err_pred ~ normal(0,1);
  gamma_err_pred ~ normal(0,1);
  
  // 観測方程式に従い観測値が得られる
  // 並列処理
  target += reduce_sum(partial_sum_lpmf , y, grainsize, lambda);
  // 並列処理を行わない場合
  // y ~ poisson_log(lambda);
}
generated quantities {
  
  // 予測範囲のパラメータ設定
  vector[T+pred_term] mu_pred;           // 水準+ドリフト成分の推定値
  vector[T+pred_term] b_ope_pred;        // 稼働台数項の係数
  vector[T+pred_term] lambda_pred;       // 観測値の期待値
  vector[T-6+pred_term] sumgamma_pred;   // 周期成分
  vector[T+pred_term] tvc_pred;          // 時変係数*稼働台数成分
  vector[T+pred_term] r_pred;            // ランダム効果
  vector[T+pred_term] gamma_pred;        // 季節成分の推定値
  vector[T+pred_term] Num_pred;          // 稼働台数
  
  // データ取得期間は元の変数と同値なので、そのまま代入する
  mu_pred[1:T] = mu;
  b_ope_pred[1:T] = b_ope;
  lambda_pred[1:T] = lambda;
  sumgamma_pred[1:(T-6)] = sumgamma;
  tvc_pred[1:T] = tvc;
  r_pred[1:T] = r;
  gamma_pred[1:T] = gamma;
  Num_pred[1:T] = Num;
  
  // ランダム効果
  r_pred = s_r * r_raw_pred;
  // 水準成分、稼働台数項
  for (i in 1:pred_term) {
    // 標準偏差1の正規乱数に、s_wをかけて実際の過程誤差としている
    mu_pred[T+i] = Intercept + b_ar * mu_pred[T+i-1] + s_w * mu_err_pred[i];
    b_ope_pred[T+i] = b_ope_pred[T+i-1] + s_t * b_ope_err_pred[i];
  }
  // 周期成分
  for(i in 1:pred_term) {
    sumgamma_pred[T-6+i] = -sum(gamma_pred[(T-6+i):(T-6+i+5)]);
    gamma_pred[T+i] = sumgamma_pred[T-6+i] + s_s * gamma_err_pred[i];
  }
  // 状態推定値 λ
  for(i in 1:pred_term) {
    Num_pred[T+i] = Num_pred[T+i-1] + Num_gain;
    tvc_pred[T+i] = b_ope_pred[T+i] * Num_pred[T+i];
    lambda_pred[T+i] = mu_pred[T+i] + gamma_pred[T+i] + tvc_pred[T+i] + r_pred[T+i];
  }
  
  // 状態推定値(EXP)
  vector[T+pred_term] lambda_exp;
  // ランダム効果を除いた状態推定値
  vector[T+pred_term] mu_gamma_exp;
  // 周期成分を除いた状態推定値
  vector[T+pred_term] mu_r_exp;
  // 水準成分+時変係数成分
  vector[T+pred_term] mu_tvc_exp;
  // 周期成分・ランダム効果を除いた状態推定値
  vector[T+pred_term] mu_exp;
  // 周期成分(EXP)
  vector[T+pred_term] gamma_exp;
  // ドリフト成分(EXP)
  vector[T+pred_term-31] delta_exp;
  // ランダム成分(EXP)
  vector[T+pred_term] r_exp;
  // 時変係数*稼働台数成分
  vector[T+pred_term] tvc_exp;
  // 稼働台数を1台に設定
  vector[T+pred_term] mu_tvc_1_exp;
  vector[T+pred_term] mu_tvc_1;
  vector[T+pred_term] tvc_1_exp;
  vector[T+pred_term] tvc_1; //EM率相当
  // 観測値の予測値
  array[T+pred_term] int y_pred;
  // 指数関数を作用させる
  lambda_exp = exp(lambda_pred);
  mu_gamma_exp = exp(mu_pred + gamma_pred);
  mu_r_exp = exp(mu_pred + r_pred);
  mu_tvc_exp = exp(mu_pred + tvc_pred);
  mu_exp = exp(mu_pred);
  gamma_exp = exp(gamma_pred);
  delta_exp = exp(mu_pred[32:(T+pred_term)]-mu_pred[31:(T-1+pred_term)]);
  r_exp = exp(r_pred);
  tvc_exp = exp(tvc_pred);
  mu_tvc_1_exp = exp(mu_pred + b_ope_pred / N_max);
  mu_tvc_1 = mu_pred + b_ope_pred / N_max;
  tvc_1_exp = exp(b_ope_pred / N_max);
  tvc_1 = b_ope_pred / N_max;
  // 観測値の予測値を算出
  y_pred = poisson_log_rng(lambda_pred);
}

