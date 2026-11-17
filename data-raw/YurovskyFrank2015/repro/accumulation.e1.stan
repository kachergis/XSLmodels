//Statistical Accumulation Model for Experiment 1 Data
data {
  int<lower=0> Trials;
  int<lower=0> PicConds;
  int<lower=0> IntConds;
  array[PicConds] real<lower=1> NumPic;
  array[IntConds] int<lower=0> Int;
  array[PicConds*IntConds] int<lower=0> P_s; //Num Participants for Same
  array[PicConds*IntConds] int<lower=0> P_o; //Num Participants for Switch
  array[PicConds,IntConds] int<lower=0> B_s; //Condition start indices for Same
  array[PicConds,IntConds] int<lower=0> B_o; //Condition start indices for Switch
  array[PicConds,IntConds] int<lower=0> E_s; //Condition end indices for Same
  array[PicConds,IntConds] int<lower=0> E_o; //Condition end indices for Switch
  array[sum(P_s)] int<lower=0,upper=Trials> S; //prop. correct for Same
  array[sum(P_o)] int<lower=0,upper=Trials> O; //prop. correct for Switch
}

parameters {
  real<lower=0> lambda; //Memory decay parameter
  real<lower=0> gamma; //Initial encoding strength parameter
}

transformed parameters {
  //Compute strength of encoding in each NumRef/Int condition 
  array[PicConds,IntConds] real<lower=0,upper=1> c_s;
  array[PicConds,IntConds] real<lower=0,upper=1> c_o;
  
  for (n in 1:PicConds)
    for (i in 1:IntConds) {
      c_s[n,i] = (1/NumPic[n])*pow(Int[i],-lambda)*gamma; //Same
      c_o[n,i] = c_s[n,i]; //Switch is identical to Same
    }
}

model {
  
  for (n in 1:PicConds) {
    for (i in 1:IntConds) {
      //Compute binomial probabilities of success given encoding strengths
      for(s in B_s[n,i]:E_s[n,i]){
        S[s] ~ binomial(Trials, c_s[n,i] + (1-c_s[n,i])/NumPic[n]);
      }
      for(o in B_o[n,i]:E_o[n,i]){
        O[o] ~ binomial(Trials, c_o[n,i] + (1-c_o[n,i])/NumPic[n]);
      }
    }
  }
}
