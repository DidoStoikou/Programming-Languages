#include <iostream>
#include <algorithm>
#include <fstream>
#include <string>
#include <stack>
#include <math.h>
#include <list>

using namespace std;

void myprint(int bin[32],int msb){
  int counter = 0;

  for (int j=0; j<=msb; j++){
    if(bin[j]!=0) {
      counter= j;
    }
  }
  cout << "[";
  for (int j=0; j<counter; j++){
    cout<< bin[j]<< "," ;
  }
  cout << bin[counter] << "]" <<endl;
}

int explore(int bin[32], int sum1, int place, int K){
  if(K == sum1){
    return sum1;
  }
  int y = (int) pow(2, place);
  if ((K-sum1)>= y){
    bin[0] = bin[0] + y;
    bin[place] = bin[place] - 1;
    sum1 = sum1 + y - 1;
    return sum1;
  }
  else{
    bin[place] = bin[place] - 1;
    bin[place-1] = bin[place-1] + 2;
    sum1++;
    sum1 = explore(bin, sum1, place-1, K);
    sum1 = explore(bin, sum1, place-1, K);
    return sum1;
  }
}


int main(int argc, char** argv){
  ifstream infile;
  infile.open(argv[1]);

  int T;
  int N, K;
  infile >> T ;
  // cin >> T;

  // fun (n):
  // n = 0 end
  // solve()
  // fun(n-1)
  //
  // fun binary (bin, N, k, sum1):
  // let
  //    fun help1(bin,N):
  //      bin[k] = N%2
  //      return bin
  //    fun help2(sum1,N):
  //      if N%2 == 1 then sum1 + 1
  //      else _ then sum1
  // in
  //    if N =0 then bin
  //    else binary (help1(bin,N), N / 2, k+1, help2(sum1,N))
  // end
  for (int i=0; i<T; i++) {
      infile >> N >> K;
      // cin >> N >> K;
      int bin[32];
      int sum1 = 0;

      for (int j=0; j<32; j++){
        bin[j]=0;
      }

      int k=0;
      int oldN = N;
      while(oldN>0) {
        bin[k]= oldN%2;
        if(oldN%2 ==1){
          sum1++;
        }
        oldN = oldN/2;
        k++;
      }

      int msb= k-1;
      if ((K < sum1) || (K > N)){
        cout << "[]" << endl;
      }
      else if (K == sum1){
        myprint(bin,msb);
      }
      else if (K == N){
        cout << "[" << N << "]" << endl;
      }
      else{
        int place = 1;
        while(sum1 < K){
          while(bin[place]!=1){
            place++;
          }
          sum1 = explore(bin,sum1,place,K);
        }
        myprint(bin,msb);
      }
  }
  return 0;
}
