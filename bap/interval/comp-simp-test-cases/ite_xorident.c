/* int ite_xorident(__attribute__((secret)) int a, __attribute__((secret)) int b) { */
int ite_xorident(int a, int b) {
  int c = 0;
  int res = 0;
  
  if (a) {
    res = a ^ c;
  } else if (b) {
    res = b ^ c;
  } else {
    res = a ^ b;
  }

  return res;
}
