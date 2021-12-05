#include <stdio.h>
#include <math.h>

extern "C" {

void umatcv_murcia_(
  const double& et, const double& en, const double& fcfail, 
  double& tau1t1, double& tau2t1, double& sigmat1,
  const double& ds1t, const double& ds2t, const double& drt, 
  double& ek, int& ifail) {
  tau1t1 = et * ds1t;
  tau2t1 = et * ds2t;
  sigmat1 = en * drt;
  ek = fmax(et, en);
  ifail = sigmat1 > fcfail ? -1 : 0;
}

}
