data {
  int<lower=0> N; //number of obserations (total number of germinated seeds)
  vector[N] y; //response 
//fixed effects
	vector[N] origin;
	vector[N] temp;
	vector[N] strat;

}
transformed data {
vector[N] log_y; 		//logging the response 

vector[N] inter_ts;		//4 interaction terms 
vector[N] inter_to;
vector[N] inter_so;
vector[N] inter_tso;

log_y=log(y);
inter_to= temp .* origin;
inter_so=strat .* origin;
inter_ts=strat .* temp;
inter_tso=origin .* strat .* temp; 

//m1=append_col(inter_to, inter_so);
//m2=append_col(m1, inter_ts);
//inter=append_col(m2, inter_tso);
//X_int = append_col(X, inter);
}

parameters {
  real<lower=0> alpha;
   
  real mu_b_origin;
  real mu_b_temp;
  real mu_b_strat;
  real mu_b_inter_to;
  real mu_b_inter_so;
  real mu_b_inter_ts;
  real mu_b_inter_tso;
  
  real<lower=0> sigma_b_origin;
  real<lower=0> sigma_b_temp;
  real<lower=0> sigma_b_strat;

  real<lower=0> sigma_b_inter_to;
  real<lower=0> sigma_b_inter_so;
  real<lower=0> sigma_b_inter_ts;
  real<lower=0> sigma_b_inter_tso;

real<lower=0> sigma_y;  
}

transformed parameters {
vector[N] y_hat;
		
	for(i in 1:N){
		y_hat[i] = alpha[i] + 
		origin[i]+  temp[i] +  
		strat[i] + inter_to[i] + 
		inter_so[i] + inter_ts[i] +
		inter_tso[i] 
		;
				
		}
	
}

model {
  //priors 
 log_y ~ normal(y_hat, sigma_y);
}
//generated quantities {

//vector[N2] y_pred;
  //y_pred = new_X * beta; //the y values predicted by the model
//}