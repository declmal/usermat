enum Enum { ZERO, UNDEFINED };

template <typename T, Enum Ea, Enum Eb>
struct Multiplication {
  static T multiply(T a, T b) {
    return a*b;
  }
};


template <typename T, Enum Ea>
struct Multiplication<T,Ea,ZERO> {
  static T multiply(T a, T b) {
    return static_cast<T>(0);
  }
};


template <typename T, Enum Eb>
struct Multiplication<T,ZERO,Eb> {
};


template <typename T>
struct Multiplication<T,UNDEFINED,UNDEFINED> {
};

template <typename T, unsigned int DIM, Enum EA[DIM], Enum EB[DIM]>
struct DotProductor {
  T prodcut(T a[DIM], T b[DIM]) {
  }
};

template <typename T, unsigned int DIM>
struct ElasticPredictor {
  void predict(T sigmat1[DIM], const T Kt[DIM][DIM], const T sigmat[DIM]) {
  }
};

