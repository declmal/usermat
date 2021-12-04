#include <stdio.h>
#include <math.h>

#define NLQ 192

void murcia_update_stress() {
}

void murcia_a_flow(
  double fcpr, double hr, double k3, double k4, double k5,
  double sigma, double tau, double pp, double pn, double r, double ma1) {
  double rhosigma = fmax(0.0, -sigma) / fcpr;
}

void umatcv_test_(
  const double* et, const double* en, const double* fcfail, 
  double* tau1t1, double* tau2t1, double* sigmat1,
  double* ds1t, double* ds2t, double* drt, 
  double* ek, int* ifail) {
  *tau1t1 = *et * *ds1t;
  *tau2t1 = *et * *ds2t;
  *sigmat1 = *en * *drt;
  *ek = fmax(*et, *en);
  *ifail = *sigmat1 > *fcfail ? -1 : 0;
}
