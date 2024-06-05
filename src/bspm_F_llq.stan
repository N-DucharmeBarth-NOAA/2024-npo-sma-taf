// Original fletcher-schaefer surplus production model code comes from
// the Bayesian biomass dynamic model (https://github.com/cttedwards/bdm/) by Charles Edwards
// Original model code can be specifically found at: https://github.com/cttedwards/bdm/blob/master/R/bdm.R
// This version of the model uses a random effects formulation and estimates:
// logK, r, initial depletion, shape, process error, additional observation error, and longline catchability.
data {
    int T; // time dimension
    int I; // number of indices
    real index[T,I]; // matrix of indices
    real non_ll_removals[T]; // time series of non ll catch
    real ll_effort[T]; // time series of ll effort
    real sigmao_mat[T,I]; // observation error corresponding to each index (scaled to mean 1)
    real sigmao_input; // observation error
    int lambdas[I]; // lambdas for CPUE

    // priors
    real PriorMean_logK;
    real PriorSD_logK;
    real PriorMean_logr;
    real PriorSD_logr;
    real PriorMean_logx0;
    real PriorSD_logx0;
    real PriorMean_logsigmap;
    real PriorSD_logsigmap;
    real PriorMean_logshape;
    real PriorSD_logshape;
    real PriorSD_sigmao_add;
    real PriorMean_logll_q;
    real PriorSD_logll_q;
}
transformed data{
    int Tm1;
    Tm1 = T-1;

    // rescale effort
    real max_effort;
    real ll_effort_sc[T];
    max_effort = max(ll_effort);

    for(i in 1:T){
        ll_effort_sc[i] = ll_effort[i]/max_effort;
    }

}
parameters {
    // use mean-zero parameters with SD = 1
    // keeps searched parameter space on a similar scale
    real raw_logK;
    real raw_logr;
    real raw_logx0;
    real raw_logsigmap;
    real<lower=0> raw_sigmao_add;
    real raw_epsp[Tm1];
    real raw_logshape;
    real raw_logll_q;
}
transformed parameters {
    // leading parameters
    real logK;
    real r;
    real x0;
    real dev[Tm1]; // recruitment deviates
    real epsp[Tm1]; // process error (multiplicative)
    real sigmap;
    real shape;
    real ll_q;

    // process error
    sigmap = exp(raw_logsigmap*PriorSD_logsigmap + PriorMean_logsigmap); // lognormal prior
    real sigmap2;
    sigmap2 = pow(sigmap,2);

    // observation error
    real sigmao[T,I];
    real sigmao_sc;
    real sigmao_add;
    sigmao_add = raw_sigmao_add*PriorSD_sigmao_add;
    sigmao_sc = sigmao_input + sigmao_add;
    for(i in 1:T){
        for(j in 1:I){
            sigmao[i,j] = sigmao_mat[i,j] * sigmao_sc;
        }
    }

    // non-centered parametrization 
    ll_q = exp(raw_logll_q*PriorSD_logll_q + PriorMean_logll_q);
    logK = raw_logK*PriorSD_logK + PriorMean_logK; // normal prior (log-scale)
    r = exp(raw_logr*PriorSD_logr + PriorMean_logr); // lognormal prior
    x0 = exp(raw_logx0*PriorSD_logx0 + PriorMean_logx0); // lognormal prior
    shape = exp(raw_logshape*PriorSD_logshape + PriorMean_logshape); // lognormal prior
    for(t in 1:Tm1){
        dev[t] = raw_epsp[t]*sigmap;
        epsp[t] = exp(dev[t]-sigmap2/2); 
    }
    
    // fletcher-schaefer
    // parameters
    real n;
    real dmsy;
    real h;
    real m;
    real g;

    n = shape;
    dmsy = pow((1/n),(1/(n-1)));
    h = 2*dmsy;
    m = r*h/4;
    g = pow(n,(n/(n-1)))/(n-1);

    // population dynamics
        real C; // catch (relative to logK) in each time step
        real x[T]; // population time series relative to logK
        real removals[T];
        real total_u[T];
        x[1] = x0;
        total_u[1] = 1-exp(-((ll_q*ll_effort_sc[1])+(non_ll_removals[1]/(x[1]*exp(logK)))));
        removals[1] = total_u[1]*x[1]*exp(logK);
        for(t in 2:T) {
            C = fmin(exp(log(removals[t-1]) - logK),x[t-1]);
            if(x[t-1]<=dmsy) x[t] = (x[t-1] + r * x[t-1] * (1 - x[t-1]/h) - C)*epsp[t-1];
            if(x[t-1]> dmsy) x[t] = (x[t-1] + g * m * x[t-1] * (1 - pow(x[t-1],(n-1))) - C)*epsp[t-1];
            total_u[t] = 1-exp(-((ll_q*ll_effort_sc[t])+(non_ll_removals[t]/(x[t]*exp(logK)))));
            removals[t] = total_u[t]*x[t]*exp(logK);
        }

    // compute mpd catchability assuming 
    // variable sigmao over time assuming
    // uniform prior on q
        real q[I]; // catchability
        real sigmao2[T,I]; // observation error for each index
        real sum1;
        real sum2;
        real p;
        for(i in 1:I){
            sum1 = 0.0;
            sum2 = 0.0;
            p = 0.0;
            for(t in 1:T){
                sigmao2[t,i] = square(sigmao[t,i]);
                if(index[t,i]>0.0 && x[t]>0.0) {
                    sum1 = sum1 + log(index[t,i]/x[t])/sigmao2[t,i];
                    sum2 = sum2 + 1/sigmao2[t,i];
                    p = p + 1.0;
                }
            }
            if(p>2.0){
                q[i] = exp((0.5 * p + sum1) / sum2);
            } else{
                q[i] = 0.0;
            } 
        }
} 
model {
    // prior densities for
    // estimated parameters
        raw_logK ~ std_normal();
        raw_logr ~ std_normal();
        raw_logx0 ~ std_normal();
        raw_epsp ~ std_normal();
        raw_logsigmap ~ std_normal();
        raw_sigmao_add ~ std_normal();
        raw_logshape ~ std_normal();
        raw_logll_q ~ std_normal();
    
    // observation model
        real mu_index;
        for(i in 1:I){
            if(lambdas[i]==1){
                for(t in 1:T){
                    if(index[t,i]>0.0 && x[t]>0.0 && q[i]>0.0) {
                        mu_index = log(q[i]*x[t]) - sigmao2[t,i]/2;
                        target += lognormal_lpdf(index[t,i] | mu_index,sigmao[t,i]);
                    }
                }
            }
        }
}
