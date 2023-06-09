import optuna
import os
import pwn
import sys

pwn.context.log_level = "error"

best_score = [
   476190476, 1228822055,  256349206,  752222728,  185661764,   99920634,  452380952,  469877344,  130252100,  761446886,
   344877344,  155126050,  250000000,  197600381, 1902951215,  479974351,  135568088,   46969696,  434523809,   57588661,
   420634920,  151897122,  851190476,  383333333,   99696916,  614957264,  573056226, 2757575757,   33748755,  603289810,
   421318447,  110714285,   48779791,  243548066,  115271693,  816666666,  106017316,  279901712,  106433888,   44832329,
    16145347,  258741258,  113725321,  294801012,  847557997,  485714285,  333333333,  148344367,  703968253,  134740259,
    85527065,  163101604,   77601279,  691147741,   91421451,  705988455,  513911040,  661111111,  153846153,  299242424,
   493634477,  833333333,  175964912, 2418945518,   39126984,   53730158,  157659092,  581782106,  143733896,   55072463,
    75371376,   42519927,   95314064,  107494944,  452380952,   38291459,   59516985,  171428571,   62373737,   82262834,
   153855171,  256165216,  873608216,  424242424,  651680672,   45118451,  209523809,  220009157,  491666666,   73217491,
   420014707,  308608058,   76257994,  378968253,  183333333,  240000000,  109716511,   77568419,  107407407,  252878787,
]

def objective(trial):
  temp = trial.suggest_float("temp", 0.1, 2.)
  prob = [0.]*7
  for i in range(1, 7):
    prob[i] = trial.suggest_float("prob%d"%i, 0., 10.)

  score = 0.
  n = 100
  for i in range(n):
    cmd = "./A"
    s = pwn.process(f"./A {temp} {prob[1]:.10f} {prob[2]:.10f} {prob[3]:.10f} {prob[4]:.10f} {prob[5]:.10f} {prob[6]:.10f} < tools/in/{i:04d}.txt > /dev/null", shell=True)
    raw_score = int(s.readline().split()[4])
    s.close()
    sc = best_score[i]/raw_score
    print(i, sc)
    score += sc
  return score/n

study = optuna.create_study(
  study_name=f"ahc019_6",
  storage="sqlite:////home/kusano/ahc019/db.sqlite3",
  load_if_exists=True,
  direction="maximize")

#"""
study.enqueue_trial({
  "temp": 0.6784998756876306,
  "prob1": 1.7,
  "prob2": 1.3,
  "prob3": 5.7,
  "prob4": 5.0,
  "prob5": 1.9,
  "prob6": 1.0,
})
#"""

study.optimize(objective)
